;;; slackit-realtime.el --- Slack Web/Desktop realtime transport -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Generation-fenced Slack Web/Desktop gateway ownership, exact URL validation,
;; hello readiness, heartbeat/pong, and bounded single-timer reconnect.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-parse)
(require 'url-cookie)
(require 'url-util)
(require 'websocket)
(require 'appkit-core)
(require 'slackit-customize)
(require 'slackit-normalize)
(require 'slackit-runtime)
(require 'slackit-state)

(cl-defstruct (slackit-realtime-connection
               (:constructor slackit-realtime-connection-create))
  app generation websocket handle)

(cl-defstruct (slackit-realtime-timer
               (:constructor slackit-realtime-timer-create))
  app generation kind token timer handle)

(defconst slackit-realtime--browser-start-args
  "?agent=client&org_wide_aware=true&agent_version=1785403654&eac_cache_ts=true&cache_ts=0&name_tagging=true&only_self_subteams=true&connect_only=true&ms_latest=true"
  "Captured Slack browser client arguments encoded into the WebSocket URL.")

(defun slackit-realtime--gateway-url (app)
  "Return APP's authenticated Slack Web/Desktop gateway URL, or nil."
  (let* ((credential (slackit-runtime-credential app))
         (token (and credential (slackit-credential-token credential)))
         (team-id (and credential (slackit-credential-team-id credential))))
    (when (and (stringp token) (not (string-empty-p token))
               (stringp team-id) (not (string-empty-p team-id)))
      (format
       (concat
        "wss://wss-primary.slack.com/?token=%s"
        "&sync_desync=1&slack_client=desktop&start_args=%s"
        "&no_query_on_subscribe=1&flannel=3&lazy_channels=1"
        "&gateway_server=%s-1&batch_presence_aware=1")
       (url-hexify-string token)
       (url-hexify-string slackit-realtime--browser-start-args)
       (url-hexify-string team-id)))))

(defun slackit-realtime-valid-url-p (url)
  "Return non-nil when URL is the exact Slack Web/Desktop WSS gateway."
  (when (and (stringp url) (not (string-empty-p url)))
    (condition-case nil
        (let* ((parsed (url-generic-parse-url url))
               (type (url-type parsed))
               (host (url-host parsed)))
          (and (equal type "wss")
               (stringp host)
               (equal (downcase host) "wss-primary.slack.com")
               (null (url-user parsed))
               (null (url-password parsed))))
      (error nil))))

(defun slackit-realtime--publish-connection (app status)
  "Set APP connection STATUS and invalidate dependent views."
  (slackit-state-set-connection-status (slackit-runtime-state app) status)
  (slackit-runtime-publish-changes app (list (list :kind 'connection))))

(defun slackit-realtime--generation-current-p (app generation)
  "Return non-nil when APP owns realtime connection GENERATION."
  (and (appkit-app-live-p app)
       (let ((transport (appkit-app-transport app)))
         (and (slackit-transport-p transport)
              (not (slackit-transport-stopping-p transport))
              (= generation
                 (slackit-transport-connection-generation transport))))))

(defun slackit-realtime--timer-current-p (owner field)
  "Return non-nil when timer OWNER is current in transport FIELD."
  (and (slackit-realtime-timer-p owner)
       (slackit-realtime--generation-current-p
        (slackit-realtime-timer-app owner)
        (slackit-realtime-timer-generation owner))
       (eq owner
           (funcall field
                    (slackit-runtime-transport
                     (slackit-realtime-timer-app owner))))
       (let ((handle (slackit-realtime-timer-handle owner)))
         (and (appkit-handle-p handle) (appkit-handle-alive-p handle)))))

(defun slackit-realtime--cancel-timer-owner (owner)
  "Cancel timer process stored in OWNER."
  (when-let* ((timer (slackit-realtime-timer-timer owner)))
    (when (timerp timer) (cancel-timer timer)))
  (setf (slackit-realtime-timer-timer owner) nil))

(defun slackit-realtime--make-timer (app kind token delay repeat callback)
  "Create an APP-owned timer of KIND calling CALLBACK with its owner."
  (let* ((owner (slackit-realtime-timer-create
                 :app app
                 :generation (slackit-runtime-connection-generation app)
                 :kind kind
                 :token token))
         (handle (appkit-register-handle
                  app 'timer owner #'slackit-realtime--cancel-timer-owner))
         (timer (run-at-time delay repeat callback owner)))
    (setf (slackit-realtime-timer-handle owner) handle
          (slackit-realtime-timer-timer owner) timer)
    owner))

(defun slackit-realtime--retire-timer (owner)
  "Retire one-shot timer OWNER without cancelling its completed timer."
  (setf (slackit-realtime-timer-timer owner) nil)
  (when-let* ((handle (slackit-realtime-timer-handle owner)))
    (appkit-retire-handle handle)))

(defun slackit-realtime--cancel-field-timer (transport getter setter)
  "Cancel TRANSPORT timer selected by GETTER, then clear with SETTER."
  (when-let* ((owner (funcall getter transport)))
    (funcall setter nil transport)
    (when-let* ((handle (slackit-realtime-timer-handle owner)))
      (appkit-cancel-handle handle))))

(defun slackit-realtime--cancel-heartbeat (transport)
  "Cancel TRANSPORT hello, heartbeat, and pending pong timers."
  (slackit-realtime--cancel-field-timer
   transport #'slackit-transport-hello-timer
   (lambda (value object)
     (setf (slackit-transport-hello-timer object) value)))
  (slackit-realtime--cancel-field-timer
   transport #'slackit-transport-heartbeat-timer
   (lambda (value object)
     (setf (slackit-transport-heartbeat-timer object) value)))
  (slackit-realtime--cancel-field-timer
   transport #'slackit-transport-pong-timer
   (lambda (value object)
     (setf (slackit-transport-pong-timer object) value))))

(defun slackit-realtime--hello-timeout (owner)
  "Reconnect when open socket timer OWNER expires before realtime hello."
  (let ((app (slackit-realtime-timer-app owner)))
    (when (slackit-realtime--timer-current-p
           owner #'slackit-transport-hello-timer)
      (let ((transport (slackit-runtime-transport app)))
        (setf (slackit-transport-hello-timer transport) nil)
        (slackit-realtime--retire-timer owner)
        (slackit-realtime--disconnect-current app)
        (slackit-realtime--publish-connection app 'disconnected)
        (slackit-realtime--schedule-reconnect app)))))

(defun slackit-realtime--start-hello-timeout (app)
  "Start bounded hello readiness timer for APP's open socket."
  (let ((transport (slackit-runtime-transport app)))
    (slackit-realtime--cancel-field-timer
     transport #'slackit-transport-hello-timer
     (lambda (value object)
       (setf (slackit-transport-hello-timer object) value)))
    (setf (slackit-transport-hello-timer transport)
          (slackit-realtime--make-timer
           app 'hello nil slackit-realtime-hello-timeout nil
           #'slackit-realtime--hello-timeout))))

(defun slackit-realtime--connection-current-p (connection &optional websocket)
  "Return non-nil when CONNECTION and optional WEBSOCKET own publication."
  (let* ((app (slackit-realtime-connection-app connection))
         (transport (and (appkit-app-p app) (appkit-app-transport app)))
         (owned (slackit-realtime-connection-websocket connection))
         (handle (slackit-realtime-connection-handle connection)))
    (and (slackit-realtime--generation-current-p
          app (slackit-realtime-connection-generation connection))
         (slackit-transport-p transport)
         (eq connection (slackit-transport-connection transport))
         (appkit-handle-p handle)
         (appkit-handle-alive-p handle)
         (or (null websocket) (null owned) (eq websocket owned)))))

(defun slackit-realtime--cancel-connection (connection)
  "Close WebSocket held by CONNECTION without publishing callbacks."
  (let* ((app (slackit-realtime-connection-app connection))
         (transport (and (appkit-app-p app) (appkit-app-transport app)))
         (websocket (slackit-realtime-connection-websocket connection)))
    (when (and (slackit-transport-p transport)
               (eq connection (slackit-transport-connection transport)))
      (setf (slackit-transport-connection transport) nil
            (slackit-transport-websocket transport) nil))
    (when websocket
      (ignore-errors (websocket-close websocket)))))

(defun slackit-realtime--disconnect-current (app)
  "Cancel APP's current connection and heartbeat ownership."
  (let ((transport (slackit-runtime-transport app)))
    (slackit-realtime--cancel-heartbeat transport)
    (when-let* ((connection (slackit-transport-connection transport)))
      (setf (slackit-transport-connection transport) nil
            (slackit-transport-websocket transport) nil)
      (when-let* ((handle (slackit-realtime-connection-handle connection)))
        (appkit-cancel-handle handle)))))

(defun slackit-realtime--begin-attempt (app)
  "Revoke the old socket generation and begin one APP connection attempt."
  (let ((transport (slackit-runtime-transport app)))
    (slackit-realtime--disconnect-current app)
    (setf (slackit-transport-connection-generation transport)
          (1+ (slackit-transport-connection-generation transport))
          (slackit-transport-ready-p transport) nil
          (slackit-transport-stopping-p transport) nil)
    (slackit-realtime--publish-connection app 'connecting)
    (slackit-transport-connection-generation transport)))

(defun slackit-realtime--send-json (app payload)
  "Send PAYLOAD through APP's current open WebSocket."
  (let* ((transport (slackit-runtime-transport app))
         (websocket (slackit-transport-websocket transport)))
    (when (and websocket (websocket-openp websocket))
      (let ((json-encoding-pretty-print nil))
        (websocket-send-text websocket (json-encode payload)))
      t)))

(defun slackit-realtime--pong-timeout (owner)
  "Reconnect when ping timer OWNER expires without a matching pong."
  (let ((app (slackit-realtime-timer-app owner)))
    (when (slackit-realtime--timer-current-p
           owner #'slackit-transport-pong-timer)
      (let ((transport (slackit-runtime-transport app)))
        (setf (slackit-transport-pong-timer transport) nil)
        (slackit-realtime--retire-timer owner)
        (slackit-realtime--disconnect-current app)
        (slackit-realtime--publish-connection app 'disconnected)
        (slackit-realtime--schedule-reconnect app)))))

(defun slackit-realtime--heartbeat-tick (owner)
  "Send one Slack realtime ping for recurring timer OWNER."
  (let ((app (slackit-realtime-timer-app owner)))
    (when (slackit-realtime--timer-current-p
           owner #'slackit-transport-heartbeat-timer)
      (let* ((transport (slackit-runtime-transport app))
             (id (prog1 (slackit-transport-next-message-id transport)
                   (cl-incf (slackit-transport-next-message-id transport)))))
        (when (slackit-realtime--send-json
               app `((id . ,id) (type . "ping")
                     (time . ,(format-time-string "%s"))))
          (slackit-realtime--cancel-field-timer
           transport #'slackit-transport-pong-timer
           (lambda (value object)
             (setf (slackit-transport-pong-timer object) value)))
          (setf (slackit-transport-pong-timer transport)
                (slackit-realtime--make-timer
                 app 'pong id slackit-realtime-pong-timeout nil
                 #'slackit-realtime--pong-timeout)))))))

(defun slackit-realtime--start-heartbeat (app)
  "Start APP heartbeat after RTM hello."
  (let ((transport (slackit-runtime-transport app)))
    (slackit-realtime--cancel-heartbeat transport)
    (setf (slackit-transport-heartbeat-timer transport)
          (slackit-realtime--make-timer
           app 'heartbeat nil
           slackit-realtime-ping-interval slackit-realtime-ping-interval
           #'slackit-realtime--heartbeat-tick))))

(defun slackit-realtime--handle-pong (app event)
  "Settle APP pong timer when normalized EVENT matches its ping ID."
  (let* ((transport (slackit-runtime-transport app))
         (owner (slackit-transport-pong-timer transport)))
    (when (and owner
               (equal (slackit-realtime-timer-token owner)
                      (plist-get event :reply-to)))
      (setf (slackit-transport-pong-timer transport) nil)
      (when-let* ((handle (slackit-realtime-timer-handle owner)))
        (appkit-cancel-handle handle)))))

(defun slackit-realtime--handle-event (app payload)
  "Handle one decoded Slack realtime PAYLOAD for APP."
  (let* ((event (slackit-normalize-event payload))
         (kind (plist-get event :kind))
         (transport (slackit-runtime-transport app)))
    (pcase kind
      ('hello
       (slackit-realtime--cancel-field-timer
        transport #'slackit-transport-hello-timer
        (lambda (value object)
          (setf (slackit-transport-hello-timer object) value)))
       (setf (slackit-transport-ready-p transport) t
             (slackit-transport-reconnect-attempt transport) 0)
       (slackit-realtime--start-heartbeat app)
       (slackit-runtime-reduce-event app event))
      ('pong (slackit-realtime--handle-pong app event))
      ('ignored nil)
      (_ (slackit-runtime-reduce-event app event)))))

(defun slackit-realtime--frame-text (frame)
  "Return complete WebSocket FRAME payload text, or nil."
  (when (and (websocket-frame-completep frame)
             (stringp (websocket-frame-payload frame)))
    (websocket-frame-payload frame)))

(defun slackit-realtime--connection-closed (connection websocket)
  "Retire current CONNECTION after WEBSOCKET closes and reconnect."
  (when (slackit-realtime--connection-current-p connection websocket)
    (let* ((app (slackit-realtime-connection-app connection))
           (transport (slackit-runtime-transport app)))
      (setf (slackit-transport-connection transport) nil
            (slackit-transport-websocket transport) nil
            (slackit-transport-ready-p transport) nil)
      (appkit-retire-handle (slackit-realtime-connection-handle connection))
      (slackit-realtime--cancel-heartbeat transport)
      (slackit-realtime--publish-connection app 'disconnected)
      (slackit-realtime--schedule-reconnect app))))

(defun slackit-realtime--websocket-headers (app)
  "Return account-local browser headers for APP's WebSocket upgrade."
  (let ((d-cookie (slackit-runtime-credential-cookie-value app "d")))
    (append
     `(("User-Agent" . ,slackit-browser-user-agent)
       ("Accept-Language" . "en-US,en;q=0.9")
       ("Cache-Control" . "no-cache")
       ("Pragma" . "no-cache")
       ("Origin" . "https://app.slack.com"))
     (and d-cookie (list (cons "Cookie" (concat "d=" d-cookie)))))))

(defun slackit-realtime--open (app url)
  "Open APP WebSocket using its exact validated gateway URL."
  (unless (slackit-realtime-valid-url-p url)
    (error "slackit: rejected realtime gateway URL"))
  (let* ((generation (slackit-runtime-connection-generation app))
         (transport (slackit-runtime-transport app))
         (connection (slackit-realtime-connection-create
                      :app app :generation generation))
         (handle (appkit-register-handle
                  app 'websocket connection #'slackit-realtime--cancel-connection))
         websocket)
    (setf (slackit-realtime-connection-handle connection) handle
          (slackit-transport-connection transport) connection
          (slackit-transport-websocket-url transport) url)
    (condition-case nil
        (progn
          (setq websocket
                (let ((url-cookie-storage nil)
                      (url-cookie-secure-storage nil))
                  (websocket-open
                   url
                   :custom-header-alist (slackit-realtime--websocket-headers app)
                   :on-open
                   (lambda (opened)
                     (when (slackit-realtime--connection-current-p connection opened)
                       (slackit-realtime--publish-connection app 'handshaking)
                       (slackit-realtime--start-hello-timeout app)))
                   :on-message
                   (lambda (message-websocket frame)
                     (when (slackit-realtime--connection-current-p
                            connection message-websocket)
                       (condition-case nil
                           (when-let* ((text (slackit-realtime--frame-text frame)))
                             (slackit-realtime--handle-event
                              app (slackit-normalize-json text)))
                         (error
                          (slackit-realtime--disconnect-current app)
                          (slackit-realtime--publish-connection app 'protocol-error)
                          (slackit-realtime--schedule-reconnect app)))))
                   :on-close
                   (lambda (closed)
                     (slackit-realtime--connection-closed connection closed))
                   :on-error
                   (lambda (error-websocket _type _error)
                     (when (slackit-realtime--connection-current-p
                            connection error-websocket)
                       (slackit-realtime--disconnect-current app)
                       (slackit-realtime--publish-connection app 'disconnected)
                       (slackit-realtime--schedule-reconnect app))))))
          (if (slackit-realtime--connection-current-p connection)
              (setf (slackit-realtime-connection-websocket connection) websocket
                    (slackit-transport-websocket transport) websocket)
            (when websocket (ignore-errors (websocket-close websocket))))
          websocket)
      (error
       (when (appkit-handle-alive-p handle) (appkit-cancel-handle handle))
       (slackit-realtime--publish-connection app 'disconnected)
       (slackit-realtime--schedule-reconnect app)
       nil))))


(defun slackit-realtime--open-gateway (app)
  "Open APP's authenticated Slack Web/Desktop realtime gateway."
  (if-let* ((url (slackit-realtime--gateway-url app)))
      (slackit-realtime--open app url)
    (slackit-realtime--publish-connection app 'protocol-error)
    nil))

(defun slackit-realtime--reconnect-delay (attempt)
  "Return bounded exponential reconnect delay for ATTEMPT."
  (min slackit-reconnect-max-delay
       (* slackit-reconnect-min-delay (expt 2 (min attempt 10)))))

(defun slackit-realtime--reconnect-tick (owner)
  "Run one reconnect timer OWNER."
  (let ((app (slackit-realtime-timer-app owner)))
    (when (slackit-realtime--timer-current-p
           owner #'slackit-transport-reconnect-timer)
      (let ((transport (slackit-runtime-transport app)))
        (setf (slackit-transport-reconnect-timer transport) nil)
        (slackit-realtime--retire-timer owner)
        (slackit-realtime--begin-attempt app)
        (slackit-realtime--open-gateway app)))))

(defun slackit-realtime--schedule-reconnect (app)
  "Schedule at most one bounded reconnect attempt for APP."
  (let ((transport (slackit-runtime-transport app)))
    (when (and (slackit-realtime--generation-current-p
                app (slackit-runtime-connection-generation app))
               (null (slackit-transport-reconnect-timer transport)))
      (let* ((attempt (slackit-transport-reconnect-attempt transport))
             (delay (slackit-realtime--reconnect-delay attempt))
             (owner (slackit-realtime--make-timer
                     app 'reconnect nil delay nil
                     #'slackit-realtime--reconnect-tick)))
        (setf (slackit-transport-reconnect-attempt transport) (1+ attempt)
              (slackit-transport-reconnect-timer transport) owner)
        owner))))

(defun slackit-realtime-start (app)
  "Start generation-fenced Slack Web/Desktop realtime for APP."
  (let ((transport (slackit-runtime-transport app)))
    (slackit-realtime--cancel-field-timer
     transport #'slackit-transport-reconnect-timer
     (lambda (value object)
       (setf (slackit-transport-reconnect-timer object) value)))
    (setf (slackit-transport-reconnect-attempt transport) 0)
    (slackit-realtime--begin-attempt app)
    (slackit-realtime--open-gateway app)))

(provide 'slackit-realtime)

;;; slackit-realtime.el ends here
