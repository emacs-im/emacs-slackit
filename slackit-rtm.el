;;; slackit-rtm.el --- Slack RTM lifecycle transport -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Generation-fenced RTM capability acquisition, exact URL validation,
;; WebSocket ownership, hello readiness, heartbeat/pong, reconnect_url, and
;; bounded single-timer reconnect.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-parse)
(require 'websocket)
(require 'appkit-core)
(require 'slackit-api)
(require 'slackit-customize)
(require 'slackit-normalize)
(require 'slackit-runtime)
(require 'slackit-state)

(cl-defstruct (slackit-rtm-connection
               (:constructor slackit-rtm-connection-create))
  app generation websocket handle)

(cl-defstruct (slackit-rtm-timer
               (:constructor slackit-rtm-timer-create))
  app generation kind token timer handle)

(defun slackit-rtm-valid-url-p (url)
  "Return non-nil when URL is an allowed Slack WSS capability URL."
  (when (and (stringp url) (not (string-empty-p url)))
    (condition-case nil
        (let* ((parsed (url-generic-parse-url url))
               (type (url-type parsed))
               (host (url-host parsed)))
          (and (equal type "wss")
               (stringp host)
               (string-match-p slackit-websocket-host-regexp host)
               (null (url-user parsed))
               (null (url-password parsed))))
      (error nil))))

(defun slackit-rtm--publish-connection (app status)
  "Set APP connection STATUS and invalidate dependent views."
  (slackit-state-set-connection-status (slackit-runtime-state app) status)
  (slackit-runtime-publish-changes app (list (list :kind 'connection))))

(defun slackit-rtm--timer-current-p (owner field)
  "Return non-nil when timer OWNER is current in transport FIELD."
  (and (slackit-rtm-timer-p owner)
       (slackit-runtime-current-p
        (slackit-rtm-timer-app owner)
        (slackit-rtm-timer-generation owner))
       (eq owner
           (funcall field
                    (slackit-runtime-transport
                     (slackit-rtm-timer-app owner))))
       (let ((handle (slackit-rtm-timer-handle owner)))
         (and (appkit-handle-p handle) (appkit-handle-alive-p handle)))))

(defun slackit-rtm--cancel-timer-owner (owner)
  "Cancel timer process stored in OWNER."
  (when-let* ((timer (slackit-rtm-timer-timer owner)))
    (when (timerp timer) (cancel-timer timer)))
  (setf (slackit-rtm-timer-timer owner) nil))

(defun slackit-rtm--make-timer (app kind token delay repeat callback)
  "Create an APP-owned timer of KIND calling CALLBACK with its owner."
  (let* ((owner (slackit-rtm-timer-create
                 :app app
                 :generation (slackit-runtime-generation app)
                 :kind kind
                 :token token))
         (handle (appkit-register-handle
                  app 'timer owner #'slackit-rtm--cancel-timer-owner))
         (timer (run-at-time delay repeat callback owner)))
    (setf (slackit-rtm-timer-handle owner) handle
          (slackit-rtm-timer-timer owner) timer)
    owner))

(defun slackit-rtm--retire-timer (owner)
  "Retire one-shot timer OWNER without cancelling its completed timer."
  (setf (slackit-rtm-timer-timer owner) nil)
  (when-let* ((handle (slackit-rtm-timer-handle owner)))
    (appkit-retire-handle handle)))

(defun slackit-rtm--cancel-field-timer (transport getter setter)
  "Cancel TRANSPORT timer selected by GETTER, then clear with SETTER."
  (when-let* ((owner (funcall getter transport)))
    (funcall setter nil transport)
    (when-let* ((handle (slackit-rtm-timer-handle owner)))
      (appkit-cancel-handle handle))))

(defun slackit-rtm--cancel-heartbeat (transport)
  "Cancel TRANSPORT hello, heartbeat, and pending pong timers."
  (slackit-rtm--cancel-field-timer
   transport #'slackit-transport-hello-timer
   (lambda (value object)
     (setf (slackit-transport-hello-timer object) value)))
  (slackit-rtm--cancel-field-timer
   transport #'slackit-transport-heartbeat-timer
   (lambda (value object)
     (setf (slackit-transport-heartbeat-timer object) value)))
  (slackit-rtm--cancel-field-timer
   transport #'slackit-transport-pong-timer
   (lambda (value object)
     (setf (slackit-transport-pong-timer object) value))))

(defun slackit-rtm--hello-timeout (owner)
  "Reconnect when open socket timer OWNER expires before RTM hello."
  (let ((app (slackit-rtm-timer-app owner)))
    (when (slackit-rtm--timer-current-p
           owner #'slackit-transport-hello-timer)
      (let ((transport (slackit-runtime-transport app)))
        (setf (slackit-transport-hello-timer transport) nil)
        (slackit-rtm--retire-timer owner)
        (slackit-rtm--disconnect-current app)
        (slackit-rtm--publish-connection app 'disconnected)
        (slackit-rtm--schedule-reconnect app)))))

(defun slackit-rtm--start-hello-timeout (app)
  "Start bounded hello readiness timer for APP's open socket."
  (let ((transport (slackit-runtime-transport app)))
    (slackit-rtm--cancel-field-timer
     transport #'slackit-transport-hello-timer
     (lambda (value object)
       (setf (slackit-transport-hello-timer object) value)))
    (setf (slackit-transport-hello-timer transport)
          (slackit-rtm--make-timer
           app 'hello nil slackit-rtm-hello-timeout nil
           #'slackit-rtm--hello-timeout))))

(defun slackit-rtm--connection-current-p (connection &optional websocket)
  "Return non-nil when CONNECTION and optional WEBSOCKET own publication."
  (let* ((app (slackit-rtm-connection-app connection))
         (transport (and (appkit-app-p app) (appkit-app-transport app)))
         (owned (slackit-rtm-connection-websocket connection))
         (handle (slackit-rtm-connection-handle connection)))
    (and (slackit-runtime-current-p
          app (slackit-rtm-connection-generation connection))
         (slackit-transport-p transport)
         (eq connection (slackit-transport-connection transport))
         (appkit-handle-p handle)
         (appkit-handle-alive-p handle)
         (or (null websocket) (null owned) (eq websocket owned)))))

(defun slackit-rtm--cancel-connection (connection)
  "Close WebSocket held by CONNECTION without publishing callbacks."
  (let* ((app (slackit-rtm-connection-app connection))
         (transport (and (appkit-app-p app) (appkit-app-transport app)))
         (websocket (slackit-rtm-connection-websocket connection)))
    (when (and (slackit-transport-p transport)
               (eq connection (slackit-transport-connection transport)))
      (setf (slackit-transport-connection transport) nil
            (slackit-transport-websocket transport) nil))
    (when websocket
      (ignore-errors (websocket-close websocket)))))

(defun slackit-rtm--disconnect-current (app)
  "Cancel APP's current connection and heartbeat ownership."
  (let ((transport (slackit-runtime-transport app)))
    (slackit-rtm--cancel-heartbeat transport)
    (when-let* ((connection (slackit-transport-connection transport)))
      (setf (slackit-transport-connection transport) nil
            (slackit-transport-websocket transport) nil)
      (when-let* ((handle (slackit-rtm-connection-handle connection)))
        (appkit-cancel-handle handle)))))

(defun slackit-rtm--begin-attempt (app)
  "Revoke the old socket generation and begin one APP connection attempt."
  (let ((transport (slackit-runtime-transport app)))
    (slackit-rtm--disconnect-current app)
    (setf (slackit-transport-generation transport)
          (1+ (slackit-transport-generation transport))
          (slackit-transport-ready-p transport) nil
          (slackit-transport-stopping-p transport) nil)
    (slackit-rtm--publish-connection app 'connecting)
    (slackit-transport-generation transport)))

(defun slackit-rtm--send-json (app payload)
  "Send PAYLOAD through APP's current open WebSocket."
  (let* ((transport (slackit-runtime-transport app))
         (websocket (slackit-transport-websocket transport)))
    (when (and websocket (websocket-openp websocket))
      (let ((json-encoding-pretty-print nil))
        (websocket-send-text websocket (json-encode payload)))
      t)))

(defun slackit-rtm--pong-timeout (owner)
  "Reconnect when ping timer OWNER expires without a matching pong."
  (let ((app (slackit-rtm-timer-app owner)))
    (when (slackit-rtm--timer-current-p
           owner #'slackit-transport-pong-timer)
      (let ((transport (slackit-runtime-transport app)))
        (setf (slackit-transport-pong-timer transport) nil)
        (slackit-rtm--retire-timer owner)
        (slackit-rtm--disconnect-current app)
        (slackit-rtm--publish-connection app 'disconnected)
        (slackit-rtm--schedule-reconnect app)))))

(defun slackit-rtm--heartbeat-tick (owner)
  "Send one Slack RTM ping for recurring timer OWNER."
  (let ((app (slackit-rtm-timer-app owner)))
    (when (slackit-rtm--timer-current-p
           owner #'slackit-transport-heartbeat-timer)
      (let* ((transport (slackit-runtime-transport app))
             (id (prog1 (slackit-transport-next-message-id transport)
                   (cl-incf (slackit-transport-next-message-id transport)))))
        (when (slackit-rtm--send-json
               app `((id . ,id) (type . "ping")
                     (time . ,(format-time-string "%s"))))
          (slackit-rtm--cancel-field-timer
           transport #'slackit-transport-pong-timer
           (lambda (value object)
             (setf (slackit-transport-pong-timer object) value)))
          (setf (slackit-transport-pong-timer transport)
                (slackit-rtm--make-timer
                 app 'pong id slackit-rtm-pong-timeout nil
                 #'slackit-rtm--pong-timeout)))))))

(defun slackit-rtm--start-heartbeat (app)
  "Start APP heartbeat after RTM hello."
  (let ((transport (slackit-runtime-transport app)))
    (slackit-rtm--cancel-heartbeat transport)
    (setf (slackit-transport-heartbeat-timer transport)
          (slackit-rtm--make-timer
           app 'heartbeat nil
           slackit-rtm-ping-interval slackit-rtm-ping-interval
           #'slackit-rtm--heartbeat-tick))))

(defun slackit-rtm--handle-pong (app event)
  "Settle APP pong timer when normalized EVENT matches its ping ID."
  (let* ((transport (slackit-runtime-transport app))
         (owner (slackit-transport-pong-timer transport)))
    (when (and owner
               (equal (slackit-rtm-timer-token owner)
                      (plist-get event :reply-to)))
      (setf (slackit-transport-pong-timer transport) nil)
      (when-let* ((handle (slackit-rtm-timer-handle owner)))
        (appkit-cancel-handle handle)))))

(defun slackit-rtm--handle-event (app payload)
  "Handle one decoded RTM PAYLOAD for APP."
  (let* ((event (slackit-normalize-event payload))
         (kind (plist-get event :kind))
         (transport (slackit-runtime-transport app)))
    (pcase kind
      ('hello
       (slackit-rtm--cancel-field-timer
        transport #'slackit-transport-hello-timer
        (lambda (value object)
          (setf (slackit-transport-hello-timer object) value)))
       (setf (slackit-transport-ready-p transport) t
             (slackit-transport-reconnect-attempt transport) 0)
       (slackit-rtm--start-heartbeat app)
       (slackit-runtime-reduce-event app event))
      ('pong (slackit-rtm--handle-pong app event))
      ('reconnect-url
       (let ((url (plist-get event :url)))
         (when (slackit-rtm-valid-url-p url)
           (setf (slackit-transport-reconnect-url transport) url))))
      ('ignored nil)
      (_ (slackit-runtime-reduce-event app event)))))

(defun slackit-rtm--frame-text (frame)
  "Return complete WebSocket FRAME payload text, or nil."
  (when (and (websocket-frame-completep frame)
             (stringp (websocket-frame-payload frame)))
    (websocket-frame-payload frame)))

(defun slackit-rtm--connection-closed (connection websocket)
  "Retire current CONNECTION after WEBSOCKET closes and reconnect."
  (when (slackit-rtm--connection-current-p connection websocket)
    (let* ((app (slackit-rtm-connection-app connection))
           (transport (slackit-runtime-transport app)))
      (setf (slackit-transport-connection transport) nil
            (slackit-transport-websocket transport) nil
            (slackit-transport-ready-p transport) nil)
      (appkit-retire-handle (slackit-rtm-connection-handle connection))
      (slackit-rtm--cancel-heartbeat transport)
      (slackit-rtm--publish-connection app 'disconnected)
      (slackit-rtm--schedule-reconnect app))))

(defun slackit-rtm--websocket-headers (app)
  "Return account-local WebSocket cookie headers for APP."
  (let* ((credential (slackit-transport-credential
                      (slackit-runtime-transport app)))
         (cookie (and credential (slackit-credential-cookie credential))))
    (when (and (stringp cookie) (not (string-empty-p cookie)))
      (list (cons "Cookie"
                  (if (string-prefix-p "d=" cookie)
                      cookie
                    (concat "d=" cookie)))))))

(defun slackit-rtm--open (app url)
  "Open APP WebSocket using exact validated capability URL."
  (unless (slackit-rtm-valid-url-p url)
    (error "slackit: rejected RTM capability URL"))
  (let* ((generation (slackit-runtime-generation app))
         (transport (slackit-runtime-transport app))
         (connection (slackit-rtm-connection-create
                      :app app :generation generation))
         (handle (appkit-register-handle
                  app 'websocket connection #'slackit-rtm--cancel-connection))
         websocket)
    (setf (slackit-rtm-connection-handle connection) handle
          (slackit-transport-connection transport) connection
          (slackit-transport-capability-url transport) url)
    (condition-case nil
        (progn
          (setq websocket
                (websocket-open
                 url
                 :custom-header-alist (slackit-rtm--websocket-headers app)
                 :on-open
                 (lambda (opened)
                   (when (slackit-rtm--connection-current-p connection opened)
                     (slackit-rtm--publish-connection app 'handshaking)
                     (slackit-rtm--start-hello-timeout app)))
                 :on-message
                 (lambda (message-websocket frame)
                   (when (slackit-rtm--connection-current-p
                          connection message-websocket)
                     (condition-case nil
                         (when-let* ((text (slackit-rtm--frame-text frame)))
                           (slackit-rtm--handle-event
                            app (slackit-normalize-json text)))
                       (error
                        (slackit-rtm--disconnect-current app)
                        (slackit-rtm--publish-connection app 'protocol-error)
                        (slackit-rtm--schedule-reconnect app)))))
                 :on-close
                 (lambda (closed)
                   (slackit-rtm--connection-closed connection closed))
                 :on-error
                 (lambda (error-websocket _type _error)
                   (when (slackit-rtm--connection-current-p
                          connection error-websocket)
                     (slackit-rtm--disconnect-current app)
                     (slackit-rtm--publish-connection app 'disconnected)
                     (slackit-rtm--schedule-reconnect app)))))
          (if (slackit-rtm--connection-current-p connection)
              (setf (slackit-rtm-connection-websocket connection) websocket
                    (slackit-transport-websocket transport) websocket)
            (when websocket (ignore-errors (websocket-close websocket))))
          websocket)
      (error
       (when (appkit-handle-alive-p handle) (appkit-cancel-handle handle))
       (slackit-rtm--publish-connection app 'disconnected)
       (slackit-rtm--schedule-reconnect app)
       nil))))

(defun slackit-rtm--capability-success (app generation body)
  "Open APP RTM capability from BODY when GENERATION remains current."
  (when (slackit-runtime-current-p app generation)
    (let ((url (slackit-normalize-get body 'url))
          (team (slackit-normalize-get body 'team))
          (self (slackit-normalize-get body 'self)))
      (when (and team self)
        (slackit-state-put-team-self (slackit-runtime-state app) team self))
      (if (slackit-rtm-valid-url-p url)
          (slackit-rtm--open app url)
        (slackit-rtm--publish-connection app 'protocol-error)
        (slackit-rtm--schedule-reconnect app)))))

(defun slackit-rtm--request-capability (app)
  "Request and open a fresh RTM capability for APP."
  (let ((generation (slackit-runtime-generation app)))
    (slackit-api-rtm-connect
     app
     :on-success (lambda (body)
                   (slackit-rtm--capability-success app generation body))
     :on-error (lambda (_error)
                 (when (slackit-runtime-current-p app generation)
                   (slackit-rtm--publish-connection app 'disconnected)
                   (slackit-rtm--schedule-reconnect app))))))

(defun slackit-rtm--reconnect-delay (attempt)
  "Return bounded exponential reconnect delay for ATTEMPT."
  (min slackit-reconnect-max-delay
       (* slackit-reconnect-min-delay (expt 2 (min attempt 10)))))

(defun slackit-rtm--reconnect-tick (owner)
  "Run one reconnect timer OWNER."
  (let ((app (slackit-rtm-timer-app owner)))
    (when (slackit-rtm--timer-current-p
           owner #'slackit-transport-reconnect-timer)
      (let* ((transport (slackit-runtime-transport app))
             (reconnect-url (slackit-transport-reconnect-url transport)))
        (setf (slackit-transport-reconnect-timer transport) nil
              (slackit-transport-reconnect-url transport) nil)
        (slackit-rtm--retire-timer owner)
        (slackit-rtm--begin-attempt app)
        (if (slackit-rtm-valid-url-p reconnect-url)
            (slackit-rtm--open app reconnect-url)
          (slackit-rtm--request-capability app))))))

(defun slackit-rtm--schedule-reconnect (app)
  "Schedule at most one bounded reconnect attempt for APP."
  (let ((transport (slackit-runtime-transport app)))
    (when (and (slackit-runtime-current-p
                app (slackit-runtime-generation app))
               (null (slackit-transport-reconnect-timer transport)))
      (let* ((attempt (slackit-transport-reconnect-attempt transport))
             (delay (slackit-rtm--reconnect-delay attempt))
             (owner (slackit-rtm--make-timer
                     app 'reconnect nil delay nil
                     #'slackit-rtm--reconnect-tick)))
        (setf (slackit-transport-reconnect-attempt transport) (1+ attempt)
              (slackit-transport-reconnect-timer transport) owner)
        owner))))

(defun slackit-rtm-start (app)
  "Start generation-fenced Slack RTM for APP."
  (let ((transport (slackit-runtime-transport app)))
    (slackit-rtm--cancel-field-timer
     transport #'slackit-transport-reconnect-timer
     (lambda (value object)
       (setf (slackit-transport-reconnect-timer object) value)))
    (setf (slackit-transport-reconnect-attempt transport) 0
          (slackit-transport-reconnect-url transport) nil)
    (slackit-rtm--begin-attempt app)
    (slackit-rtm--request-capability app)))

(provide 'slackit-rtm)

;;; slackit-rtm.el ends here
