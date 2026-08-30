;;; slackit-runtime.el --- Slackit account ownership runtime -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; One Appkit application, canonical state, credential bundle, transport
;; generation, request table, and view registry per stable local Slack account.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'slackit-state)

(declare-function slackit-api-user-info "slackit-api"
                  (app user-id &rest arguments))

(cl-defstruct (slackit-credential
               (:constructor slackit-credential-create))
  token
  cookie
  team-id
  user-id)

(cl-defstruct (slackit-transport
               (:constructor slackit-transport-create))
  credential
  generation
  connection-generation
  websocket
  connection
  websocket-url
  hello-timer
  heartbeat-timer
  pong-timer
  reconnect-timer
  reconnect-attempt
  next-message-id
  ready-p
  stopping-p)

(cl-defstruct (slackit-operation
               (:constructor slackit-operation-create))
  key
  nonce
  generation
  view
  composer-revision
  payload)

(defvar slackit-runtime--accounts (make-hash-table :test #'equal)
  "Stable local account ID to live Slackit Appkit application.")

(defvar slackit-runtime--operation-nonce 0
  "Monotonic process-local Slackit operation nonce.")

(defun slackit-runtime-account (account-id)
  "Return live Slackit application for ACCOUNT-ID, or nil."
  (let ((app (gethash account-id slackit-runtime--accounts)))
    (and (appkit-app-live-p app) app)))

(defun slackit-runtime-accounts ()
  "Return all live Slackit account applications."
  (let (apps)
    (maphash (lambda (_id app)
               (when (appkit-app-live-p app) (push app apps)))
             slackit-runtime--accounts)
    (sort apps (lambda (left right)
                 (string< (format "%s" (appkit-app-id left))
                          (format "%s" (appkit-app-id right)))))))

(defun slackit-runtime-state (app)
  "Return canonical Slackit state owned by APP."
  (unless (and (appkit-app-p app)
               (slackit-account-state-p (appkit-app-state app)))
    (error "slackit: invalid account application"))
  (appkit-app-state app))

(defun slackit-runtime-transport (app)
  "Return transport owned by Slackit APP."
  (unless (and (appkit-app-p app)
               (slackit-transport-p (appkit-app-transport app)))
    (error "slackit: invalid account transport"))
  (appkit-app-transport app))

(defun slackit-runtime-credential (app)
  "Return the credential owned by Slackit APP, or nil."
  (slackit-transport-credential (slackit-runtime-transport app)))

(defun slackit-runtime-credential-cookie-value (app name)
  "Return APP credential cookie NAME without exposing sibling cookies."
  (unless (and (stringp name)
               (string-match-p "\\`[[:alnum:]-]+\\'" name))
    (error "slackit: invalid credential cookie name"))
  (let ((cookie
         (when-let* ((credential (slackit-runtime-credential app)))
           (slackit-credential-cookie credential))))
    (when (stringp cookie)
      (if (and (equal name "d")
               (not (string-match-p "=" cookie)))
          cookie
        (when (string-match
               (format "\\(?:\\`\\|;[[:space:]]*\\)%s=\\([^;]+\\)"
                       (regexp-quote name))
               cookie)
          (match-string 1 cookie))))))

(defun slackit-runtime-bind-credential-identity (app team-id user-id)
  "Bind authenticated TEAM-ID and USER-ID to APP's credential.

Return non-nil after binding missing identity fields.  Return nil without
mutation when either value is empty or conflicts with an already pinned
browser-session identity."
  (let* ((transport (slackit-runtime-transport app))
         (credential (slackit-transport-credential transport))
         (expected-team-id
          (and credential (slackit-credential-team-id credential)))
         (expected-user-id
          (and credential (slackit-credential-user-id credential))))
    (when (and credential
               (stringp team-id)
               (not (string-empty-p team-id))
               (stringp user-id)
               (not (string-empty-p user-id))
               (or (null expected-team-id) (equal expected-team-id team-id))
               (or (null expected-user-id) (equal expected-user-id user-id)))
      (setf (slackit-credential-team-id credential) team-id
            (slackit-credential-user-id credential) user-id)
      t)))

(defun slackit-runtime-generation (app)
  "Return APP's account-lifecycle callback generation."
  (slackit-transport-generation (slackit-runtime-transport app)))

(defun slackit-runtime-connection-generation (app)
  "Return APP's realtime connection-attempt generation."
  (slackit-transport-connection-generation
   (slackit-runtime-transport app)))

(defun slackit-runtime-current-p (app generation)
  "Return non-nil when APP and GENERATION still own callback publication."
  (and (appkit-app-live-p app)
       (let ((transport (appkit-app-transport app)))
         (and (slackit-transport-p transport)
              (not (slackit-transport-stopping-p transport))
              (= generation (slackit-transport-generation transport))))))

(defun slackit-runtime--clear-credential (transport)
  "Erase credential references held by TRANSPORT."
  (when-let* ((credential (slackit-transport-credential transport)))
    (setf (slackit-credential-token credential) nil
          (slackit-credential-cookie credential) nil
          (slackit-credential-team-id credential) nil
          (slackit-credential-user-id credential) nil))
  (setf (slackit-transport-credential transport) nil
        (slackit-transport-websocket-url transport) nil))

(defun slackit-runtime--app-shutdown (app)
  "Complete cleanup for stopped Slackit APP."
  (let ((transport (appkit-app-transport app))
        (account-id (appkit-app-id app)))
    (when (slackit-transport-p transport)
      (setf (slackit-transport-websocket transport) nil
            (slackit-transport-connection transport) nil
            (slackit-transport-hello-timer transport) nil
            (slackit-transport-heartbeat-timer transport) nil
            (slackit-transport-pong-timer transport) nil
            (slackit-transport-reconnect-timer transport) nil
            (slackit-transport-ready-p transport) nil)
      (slackit-runtime--clear-credential transport))
    (clrhash (appkit-app-request-table app))
    (when (eq app (gethash account-id slackit-runtime--accounts))
      (remhash account-id slackit-runtime--accounts))))

(appkit-define-app-kind slackit-account
  :shutdown #'slackit-runtime--app-shutdown)

(defun slackit-runtime-start-account (account-id credential)
  "Start or return account ACCOUNT-ID using secret CREDENTIAL plist."
  (unless (and (stringp account-id) (not (string-empty-p account-id)))
    (user-error "slackit: account ID must be a non-empty string"))
  (or (slackit-runtime-account account-id)
      (let* ((token (plist-get credential :token))
             (cookie (plist-get credential :cookie))
             (team-id (plist-get credential :team-id))
             (user-id (plist-get credential :user-id)))
        (unless (and (stringp token) (not (string-empty-p token)))
          (user-error "slackit: account %s has no token" account-id))
        (let* ((state (slackit-state-create))
               (transport
                (slackit-transport-create
                 :credential (slackit-credential-create
                              :token token
                              :cookie cookie
                              :team-id team-id
                              :user-id user-id)
                 :generation 1
                 :connection-generation 0
                 :reconnect-attempt 0
                 :next-message-id 1
                 :ready-p nil
                 :stopping-p nil))
               (app (appkit-start-app
                     'slackit-account
                     :id account-id
                     :state state
                     :transport transport)))
          (puthash account-id app slackit-runtime--accounts)
          app))))

(defun slackit-runtime-revoke-generation (app)
  "Revoke account callbacks and realtime connection ownership for APP."
  (let ((transport (slackit-runtime-transport app)))
    (setf (slackit-transport-stopping-p transport) t
          (slackit-transport-ready-p transport) nil
          (slackit-transport-generation transport)
          (1+ (slackit-transport-generation transport))
          (slackit-transport-connection-generation transport)
          (1+ (slackit-transport-connection-generation transport)))
    (slackit-state-set-connection-status
     (slackit-runtime-state app) 'stopped)
    (slackit-transport-generation transport)))

(defun slackit-runtime-begin-generation (app)
  "Begin a fresh account lifecycle and realtime generation for live APP."
  (let ((transport (slackit-runtime-transport app)))
    (setf (slackit-transport-generation transport)
          (1+ (slackit-transport-generation transport))
          (slackit-transport-connection-generation transport)
          (1+ (slackit-transport-connection-generation transport))
          (slackit-transport-stopping-p transport) nil
          (slackit-transport-ready-p transport) nil
          (slackit-transport-reconnect-attempt transport) 0)
    (slackit-state-set-connection-status
     (slackit-runtime-state app) 'connecting)
    (slackit-transport-generation transport)))

(defun slackit-runtime-stop-account (account-or-id)
  "Stop Slackit ACCOUNT-OR-ID after revoking callback publication."
  (let ((app (if (appkit-app-p account-or-id)
                 account-or-id
               (slackit-runtime-account account-or-id))))
    (when (appkit-app-live-p app)
      (slackit-runtime-revoke-generation app)
      (appkit-stop-app app)
      t)))

(defun slackit-runtime-stop-all ()
  "Stop every live Slackit account."
  (dolist (app (slackit-runtime-accounts))
    (slackit-runtime-stop-account app))
  t)

(defun slackit-runtime--secret-values (app)
  "Return secret strings currently owned by APP."
  (let* ((transport (and (appkit-app-p app) (appkit-app-transport app)))
         (credential (and (slackit-transport-p transport)
                          (slackit-transport-credential transport))))
    (delq nil
          (list (and credential (slackit-credential-token credential))
                (and credential (slackit-credential-cookie credential))
                (and transport (slackit-transport-websocket-url transport))))))

(defun slackit-runtime-redact (app value)
  "Return printable VALUE with APP secrets and credential syntax redacted."
  (let ((text (format "%s" (or value ""))))
    (dolist (secret (slackit-runtime--secret-values app))
      (when (and (stringp secret) (not (string-empty-p secret)))
        (setq text (replace-regexp-in-string
                    (regexp-quote secret) "[REDACTED]" text t t))))
    (dolist (pattern '("\\bBearer[[:space:]]+[^[:space:]\r\n]+"
                       "\\bxox[a-z]-[[:alnum:]-]+"
                       "[?&]token=[^&#[:space:]]+"
                       "\\bd=[^;[:space:]]+"))
      (setq text (replace-regexp-in-string pattern "[REDACTED]" text t)))
    text))

(defun slackit-runtime-operation-begin
    (app key &optional view composer-revision payload)
  "Begin latest account-owned operation KEY in APP."
  (let* ((table (appkit-app-request-table app))
         (operation
          (slackit-operation-create
           :key key
           :nonce (cl-incf slackit-runtime--operation-nonce)
           :generation (slackit-runtime-generation app)
           :view view
           :composer-revision composer-revision
           :payload payload)))
    (puthash key operation table)
    operation))

(defun slackit-runtime-operation-current-p (app operation)
  "Return non-nil when OPERATION is still latest and current in APP."
  (and (slackit-operation-p operation)
       (slackit-runtime-current-p app
                                  (slackit-operation-generation operation))
       (eq operation
           (gethash (slackit-operation-key operation)
                    (appkit-app-request-table app)))))

(defun slackit-runtime-operation-end (app operation)
  "Retire current OPERATION from APP and return non-nil when retired."
  (when (slackit-runtime-operation-current-p app operation)
    (remhash (slackit-operation-key operation) (appkit-app-request-table app))
    t))

(defun slackit-runtime--user-view-current-p (app view user-id)
  "Return non-nil when VIEW is APP's exact live USER-ID profile view."
  (let ((view-id (list 'user user-id)))
    (and (appkit-view-live-p view)
         (eq (appkit-view-app view) app)
         (equal (appkit-view-id view) view-id)
         (eq view (appkit-view-for-id app view-id)))))

(defun slackit-runtime--publish-user-info-error
    (app operation error-data)
  "Publish redacted ERROR-DATA to OPERATION's exact user view."
  (let ((view (slackit-operation-view operation))
        (user-id (slackit-operation-payload operation))
        (code (or (plist-get error-data :code) "request_failed")))
    (when (slackit-runtime--user-view-current-p app view user-id)
      (appkit-view-enqueue-event
       view (list :kind 'user-profile-error
                  :user-id user-id
                  :code (format "%s" code)))
      (appkit-request-sync view :part 'profile))))
(defun slackit-runtime--user-info-owner-current-p (app operation)
  "Return non-nil when OPERATION's optional profile view remains current."
  (let ((view (slackit-operation-view operation))
        (user-id (slackit-operation-payload operation)))
    (or (null view)
        (slackit-runtime--user-view-current-p app view user-id))))


(defun slackit-runtime--user-info-success (app operation body)
  "Settle current lazy user OPERATION for APP from API BODY."
  (when (slackit-runtime-operation-current-p app operation)
    (if (not (slackit-runtime--user-info-owner-current-p app operation))
        (slackit-runtime-operation-end app operation)
      (let* ((state (slackit-runtime-state app))
             (requested-id (slackit-operation-payload operation))
             (user (alist-get 'user body))
             (returned-id (alist-get 'id user)))
        (if (and user (equal requested-id returned-id))
            (progn
              (slackit-runtime-operation-end app operation)
              (slackit-state-put-user state user)
              (slackit-runtime-publish-changes
               app (list (list :kind 'user :user-id requested-id))))
          (slackit-runtime--publish-user-info-error
           app operation '(:code "invalid_response"))
          (slackit-runtime-operation-end app operation))))))

(defun slackit-runtime--user-info-failure (app operation error-data)
  "Settle failed lazy user OPERATION without exposing its response."
  (when (slackit-runtime-operation-current-p app operation)
    (slackit-runtime--publish-user-info-error app operation error-data)
    (slackit-runtime-operation-end app operation)))

(defun slackit-runtime-user-pending-p (app user-id)
  "Return non-nil when APP owns a current USER-ID profile request."
  (let ((operation
         (and (appkit-app-live-p app)
              (gethash (list 'user user-id)
                       (appkit-app-request-table app)))))
    (and (slackit-runtime-operation-current-p app operation) operation)))

(cl-defun slackit-runtime-ensure-user
    (app user-id &key force view)
  "Fetch Slack USER-ID once for live APP.

Normally fetch only an unknown user.  FORCE refreshes a cached user.  VIEW,
when it is the exact `(user USER-ID)' view, owns presentation-only failure
notification.  Return the current/new operation, or nil for an invalid or
already-cached identity."
  (when (and (appkit-app-live-p app)
             (stringp user-id)
             (not (string-empty-p user-id))
             (not (string-prefix-p "B" user-id))
             (or force
                 (not (slackit-state-user
                       (slackit-runtime-state app) user-id))))
    (let* ((key (list 'user user-id))
           (pending (gethash key (appkit-app-request-table app))))
      (if (slackit-runtime-operation-current-p app pending)
          (progn
            (when (and (null (slackit-operation-view pending))
                       (slackit-runtime--user-view-current-p
                        app view user-id))
              (setf (slackit-operation-view pending) view))
            pending)
        (let ((operation
               (slackit-runtime-operation-begin
                app key
                (and (slackit-runtime--user-view-current-p
                      app view user-id)
                     view)
                nil user-id)))
          (slackit-api-user-info
           app user-id
           :owner (slackit-operation-view operation)
           :on-success
           (apply-partially
            #'slackit-runtime--user-info-success app operation)
           :on-error
           (apply-partially
            #'slackit-runtime--user-info-failure app operation))
          operation)))))

(defun slackit-runtime--view-matches-conversation-p (view conversation-id)
  "Return non-nil when VIEW belongs to CONVERSATION-ID."
  (let ((id (appkit-view-id view)))
    (and (consp id)
         (memq (car id) '(room thread))
         (equal (cadr id) conversation-id))))

(defun slackit-runtime--publish-change-to-view (view change)
  "Enqueue canonical CHANGE and invalidate matching live VIEW."
  (let* ((id (appkit-view-id view))
         (view-kind (car-safe id))
         (chat-p (memq view-kind '(room thread)))
         (kind (plist-get change :kind))
         (conversation-id (plist-get change :conversation-id))
         (conversation-match-p
          (slackit-runtime--view-matches-conversation-p
           view conversation-id))
         (ts (plist-get change :ts))
         (user-id (plist-get change :user-id)))
    (cond
     ((and (eq view-kind 'user)
           (eq kind 'user)
           (equal (cadr id) user-id))
      (appkit-view-enqueue-event view change)
      (appkit-request-sync view :structure t :part 'profile))
     ((eq view-kind 'root)
      (cond
       ((and conversation-id
             (memq kind
                   '(message-create message-update message-delete read)))
        (appkit-view-enqueue-event view change)
        (appkit-request-sync view :entry conversation-id))
       ((memq kind '(connection bootstrap user conversation))
        (appkit-view-enqueue-event view change)
        (appkit-request-sync view :structure t))))
     ((and conversation-match-p
           (memq kind
                 '(message-create message-update message-delete reaction read)))
      (appkit-view-enqueue-event view change)
      (if (memq kind '(message-create message-delete))
          (appkit-request-sync view :structure t :position t)
        (appkit-request-sync view :entry ts :position t)))
     ((and chat-p (eq kind 'connection))
      (appkit-view-enqueue-event view change)
      (appkit-request-sync view :part 'frame))
     ((and chat-p (eq kind 'conversation))
      (appkit-view-enqueue-event view change)
      (appkit-request-sync
       view
       :part (and conversation-match-p 'frame)
       :resource (list :conversation conversation-id)))
     ((and chat-p (eq kind 'user))
      (appkit-view-enqueue-event view change)
      (appkit-request-sync view :resource (list :user user-id))))))

(defun slackit-runtime-publish-changes (app changes)
  "Publish canonical CHANGES to all affected views owned by APP."
  (when (appkit-app-live-p app)
    (maphash
     (lambda (_id view)
       (when (appkit-view-live-p view)
         (dolist (change changes)
           (slackit-runtime--publish-change-to-view view change))))
     (appkit-app-view-registry app)))
  changes)

(defun slackit-runtime-publish-resource (app resource)
  "Invalidate opaque RESOURCE in every live view owned by APP."
  (when (appkit-app-live-p app)
    (maphash
     (lambda (_id view)
       (when (appkit-view-live-p view)
         (appkit-request-sync view :resource resource)))
     (appkit-app-view-registry app)))
  resource)

(defun slackit-runtime-reduce-event (app event)
  "Reduce normalized EVENT into APP and request view synchronization."
  (when (appkit-app-live-p app)
    (let ((changes (slackit-state-apply-event
                    (slackit-runtime-state app) event)))
      (slackit-runtime-publish-changes app changes)
      changes)))

(defun slackit-runtime-publish-bootstrap (app)
  "Publish current APP bootstrap state to its views."
  (slackit-runtime-publish-changes
   app (list (list :kind 'bootstrap))))

(provide 'slackit-runtime)

;;; slackit-runtime.el ends here
