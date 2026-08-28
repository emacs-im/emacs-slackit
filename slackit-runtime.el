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

(cl-defstruct (slackit-credential
               (:constructor slackit-credential-create))
  token
  cookie)

(cl-defstruct (slackit-transport
               (:constructor slackit-transport-create))
  credential
  generation
  websocket
  connection
  capability-url
  reconnect-url
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

(defun slackit-runtime-generation (app)
  "Return APP's current transport generation."
  (slackit-transport-generation (slackit-runtime-transport app)))

(defun slackit-runtime-current-p (app generation)
  "Return non-nil when APP and GENERATION still own callback publication."
  (and (appkit-app-live-p app)
       (let ((transport (appkit-app-transport app)))
         (and (slackit-transport-p transport)
              (not (slackit-transport-stopping-p transport))
              (= generation (slackit-transport-generation transport))))))

(defun slackit-runtime--clear-credential (transport)
  "Erase secret references held by TRANSPORT."
  (when-let* ((credential (slackit-transport-credential transport)))
    (setf (slackit-credential-token credential) nil
          (slackit-credential-cookie credential) nil))
  (setf (slackit-transport-credential transport) nil
        (slackit-transport-capability-url transport) nil
        (slackit-transport-reconnect-url transport) nil))

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
             (cookie (plist-get credential :cookie)))
        (unless (and (stringp token) (not (string-empty-p token)))
          (user-error "slackit: account %s has no token" account-id))
        (let* ((state (slackit-state-create))
               (transport
                (slackit-transport-create
                 :credential (slackit-credential-create
                              :token token :cookie cookie)
                 :generation 1
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
  "Revoke all callbacks in APP's current transport generation."
  (let ((transport (slackit-runtime-transport app)))
    (setf (slackit-transport-stopping-p transport) t
          (slackit-transport-ready-p transport) nil
          (slackit-transport-generation transport)
          (1+ (slackit-transport-generation transport)))
    (slackit-state-set-connection-status
     (slackit-runtime-state app) 'stopped)
    (slackit-transport-generation transport)))

(defun slackit-runtime-begin-generation (app)
  "Begin a fresh callback generation for live APP."
  (let ((transport (slackit-runtime-transport app)))
    (setf (slackit-transport-generation transport)
          (1+ (slackit-transport-generation transport))
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
                (and transport (slackit-transport-capability-url transport))))))

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
         (kind (plist-get change :kind))
         (conversation-id (plist-get change :conversation-id))
         (ts (plist-get change :ts))
         (user-id (plist-get change :user-id)))
    (cond
     ((eq view-kind 'root)
      (when (memq kind '(connection bootstrap user conversation read))
        (appkit-view-enqueue-event view change)
        (appkit-request-sync view :structure t)))
     ((and (slackit-runtime--view-matches-conversation-p
            view conversation-id)
           (memq kind '(message-create message-update message-delete reaction read)))
      (appkit-view-enqueue-event view change)
      (if (memq kind '(message-create message-delete))
          (appkit-request-sync view :structure t :position t)
        (appkit-request-sync view :entry ts :position t)))
     ((and (memq view-kind '(room thread)) (eq kind 'user))
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
