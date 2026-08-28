;;; slackit.el --- Appkit-based Slack client -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;; Author: Slackit contributors
;; Keywords: comm
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (appkit "0.2.18") (browser-session "0.1.0") (plz "0.8") (transient "0.7") (websocket "1.16"))
;; URL: https://github.com/emacs-slack/emacs-slackit

;;; Commentary:

;; Slackit is an independent, multi-account Slack client built on Appkit.
;; Credentials and protocol state remain account-local; all generated buffers
;; reconcile through Appkit views.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'slackit-customize)
(require 'slackit-auth)
(require 'slackit-normalize)
(require 'slackit-state)
(require 'slackit-runtime)
(require 'slackit-http)
(require 'slackit-api)
(require 'slackit-realtime)
(require 'slackit-history)
(require 'slackit-emoji)
(require 'slackit-media)
(require 'slackit-render)
(require 'slackit-completion)
(require 'slackit-compose)
(require 'slackit-room)
(require 'slackit-thread)
(require 'slackit-reaction)
(require 'slackit-read)
(require 'slackit-actions)
(require 'slackit-root)
(require 'slackit-transient)

(with-eval-after-load 'evil
  (require 'slackit-evil))

(defun slackit--bootstrap-failure (app operation error-data)
  "Settle APP bootstrap OPERATION with redacted ERROR-DATA."
  (when (slackit-runtime-operation-current-p app operation)
    (slackit-state-set-bootstrap-error
     (slackit-runtime-state app)
     (or (plist-get error-data :code) "request_failed"))
    (slackit-runtime-operation-end app operation)
    (slackit-runtime-publish-bootstrap app)))

(defun slackit--bootstrap-maybe-complete (app operation)
  "Retire APP bootstrap OPERATION when every collection is complete."
  (when (and (slackit-runtime-operation-current-p app operation)
             (slackit-state-bootstrap-ready-p
              (slackit-runtime-state app)))
    (slackit-runtime-operation-end app operation))
  (slackit-runtime-publish-bootstrap app))

(defun slackit--bootstrap-identity-success
    (app operation body &optional identity-ready-function)
  "Reduce auth.test BODY for current APP bootstrap OPERATION.

After binding the authenticated identity, call IDENTITY-READY-FUNCTION with
APP when it is non-nil."
  (when (slackit-runtime-operation-current-p app operation)
    (let ((team-id (slackit-normalize-get body 'team_id))
          (user-id (slackit-normalize-get body 'user_id)))
      (if (not (slackit-runtime-bind-credential-identity
                app team-id user-id))
          (slackit--bootstrap-failure
           app operation '(:code "identity_mismatch"))
        (let* ((state (slackit-runtime-state app))
               (team `((id . ,team-id)
                       (name . ,(slackit-normalize-get body 'team))))
               (self `((id . ,user-id)
                       (name . ,(slackit-normalize-get body 'user)))))
          (slackit-state-put-team-self state team self)
          (slackit-state-put-user state self)
          (slackit-state-set-bootstrap-complete state 'identity)
          (slackit--bootstrap-maybe-complete app operation)
          (when identity-ready-function
            (funcall identity-ready-function app)))))))


(defun slackit--bootstrap-conversations-page (app operation conversations)
  "Reduce one CONVERSATIONS page for current APP bootstrap OPERATION."
  (when (slackit-runtime-operation-current-p app operation)
    (let* ((state (slackit-runtime-state app))
           (ids (slackit-state-put-conversations state conversations)))
      (slackit-runtime-publish-changes
       app (mapcar (lambda (id)
                     (list :kind 'conversation :conversation-id id))
                   ids)))))

(defun slackit--bootstrap-collection-complete (app operation kind)
  "Mark APP bootstrap collection KIND complete for OPERATION."
  (when (slackit-runtime-operation-current-p app operation)
    (slackit-state-set-bootstrap-complete
     (slackit-runtime-state app) kind)
    (slackit--bootstrap-maybe-complete app operation)))

(defun slackit-bootstrap-account (app &optional identity-ready-function)
  "Start identity and conversation bootstrap for APP.

Call IDENTITY-READY-FUNCTION with APP after `auth.test' binds the credential's
workspace identity.  The user cache is ready immediately and grows through
exact `users.info' lookups requested by visible conversations and messages.
Startup never scans the workspace-wide `users.list' collection."
  (let* ((key '(bootstrap))
         (operation (slackit-runtime-operation-begin app key))
         (state (slackit-runtime-state app))
         (failure (apply-partially
                   #'slackit--bootstrap-failure app operation)))
    (slackit-state-bootstrap-reset state)
    (slackit-state-set-bootstrap-complete state 'users)
    (slackit-runtime-publish-bootstrap app)
    (slackit-api-auth-test
     app
     :on-success
     (lambda (body)
       (slackit--bootstrap-identity-success
        app operation body identity-ready-function))
     :on-error failure)
    (slackit-api-conversations-list-all
     app
     :on-page (apply-partially
               #'slackit--bootstrap-conversations-page app operation)
     :on-complete (apply-partially
                   #'slackit--bootstrap-collection-complete
                   app operation 'conversations)
     :on-error failure)
    operation))

(defun slackit-start-account (account-id &optional credential)
  "Start ACCOUNT-ID, authenticate it, start realtime, and open its root.

When CREDENTIAL is nil, resolve it through
`slackit-credential-function'."
  (interactive
   (list (completing-read "Slackit account: " slackit-account-ids nil nil)))
  (when (string-empty-p account-id)
    (user-error "slackit: account ID is required"))
  (let* ((existing (slackit-runtime-account account-id))
         (resolved (and (not existing)
                        (or credential
                            (funcall slackit-credential-function account-id))))
         (app (or existing
                  (slackit-runtime-start-account account-id resolved))))
    (slackit-root-open app t)
    (unless existing
      (slackit-bootstrap-account app #'slackit-realtime-start))
    app))

(defun slackit--login-error (account-id error)
  "Report non-secret browser login ERROR for ACCOUNT-ID."
  (message "slackit: login failed for %s: %s"
           account-id
           (browser-session-error-message error)))

;;;###autoload
(defun slackit-login (account-id)
  "Capture, import, and start local Slackit ACCOUNT-ID in a login browser."
  (interactive
   (list
    (let ((value
           (completing-read
            "Slackit account to log in: " slackit-account-ids nil nil)))
      (if (string-empty-p value)
          (user-error "slackit: account ID is required")
        value))))
  (when (slackit-auth-capture-running-p account-id)
    (user-error "slackit: browser login is already running for %s" account-id))
  (message "slackit: opening isolated Slack login browser for %s" account-id)
  (slackit-auth-capture
   account-id
   :callback
   (lambda (credential)
     (when-let* ((existing (slackit-runtime-account account-id)))
       (slackit-runtime-stop-account existing))
     (add-to-list 'slackit-account-ids account-id t)
     (message "slackit: imported browser session for %s" account-id)
     (slackit-start-account account-id credential))
   :errorback (apply-partially #'slackit--login-error account-id)))

;;;###autoload
(defun slackit-cancel-login (account-id)
  "Cancel the in-progress browser login for local ACCOUNT-ID."
  (interactive
   (list
    (let ((ids (slackit-auth-capturing-account-ids)))
      (unless ids (user-error "slackit: no browser login is running"))
      (completing-read "Cancel Slackit login: " ids nil t))))
  (unless (slackit-auth-cancel-capture account-id)
    (user-error "slackit: browser login is not running for %s" account-id))
  (message "slackit: cancelled browser login for %s" account-id))

;;;###autoload
(defun slackit-register-account (account-id token &optional cookie)
  "Start ACCOUNT-ID using TOKEN and optional xoxc COOKIE for this session."
  (interactive
   (let ((id (read-string "Stable local Slackit account ID: ")))
     (list id
           (read-passwd "Slack token: ")
           (let ((value (read-passwd "Slack cookie (empty when unused): ")))
             (and (not (string-empty-p value)) value)))))
  (slackit-start-account account-id (list :token token :cookie cookie)))

;;;###autoload
(defun slackit ()
  "Open a Slackit account, starting browser login when auth is absent."
  (interactive)
  (let* ((ids (delete-dups
               (append slackit-account-ids
                       (mapcar (lambda (app) (format "%s" (appkit-app-id app)))
                               (slackit-runtime-accounts)))))
         (account-id
          (if ids
              (completing-read "Slackit account: " ids nil t)
            (read-string "Stable local Slackit account ID: ")))
         (existing (slackit-runtime-account account-id)))
    (cond
     (existing (slackit-root-open existing t))
     ((slackit-auth-available-p account-id)
      (slackit-start-account account-id))
     (t (slackit-login account-id)))))

;;;###autoload
(defun slackit-stop-account (account-id)
  "Stop live Slackit ACCOUNT-ID and all of its owned resources."
  (interactive
   (list
    (let ((ids (mapcar (lambda (app) (format "%s" (appkit-app-id app)))
                       (slackit-runtime-accounts))))
      (unless ids (user-error "slackit: no live accounts"))
      (completing-read "Stop Slackit account: " ids nil t))))
  (unless (slackit-runtime-stop-account account-id)
    (user-error "slackit: account is not running: %s" account-id)))

;;;###autoload
(defun slackit-clear-auth (account-id)
  "Delete ACCOUNT-ID auth and stop it without signing out its login browser."
  (interactive
   (list
    (let ((value
           (completing-read
            "Clear Slackit account auth: " slackit-account-ids nil t)))
      (if (string-empty-p value)
          (user-error "slackit: account ID is required")
        value))))
  (when (slackit-auth-capture-running-p account-id)
    (user-error "slackit: browser login is running for %s" account-id))
  (when (yes-or-no-p
         (format "Delete private Slackit auth for %s? " account-id))
    (when-let* ((app (slackit-runtime-account account-id)))
      (slackit-runtime-stop-account app))
    (if (slackit-auth-clear account-id)
        (message "slackit: deleted auth for %s; browser login remains" account-id)
      (message "slackit: no auth file exists for %s" account-id))))

;;;###autoload
(defun slackit-stop-all ()
  "Stop all Slackit accounts and in-progress browser login captures."
  (interactive)
  (slackit-auth-cancel-all)
  (slackit-runtime-stop-all))

(provide 'slackit)

;;; slackit.el ends here
