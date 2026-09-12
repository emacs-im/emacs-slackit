;;; slackit.el --- Appkit-based Slack client -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;; Author: Slackit contributors
;; Keywords: comm
;; Version: 0.1.0
;; Package-Requires: ((emacs "32.0") (appkit "0.3.0") (browser-session "0.1.0") (plz "0.8") (transient "0.7") (websocket "1.16"))
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
(require 'slackit-state)
(require 'slackit-runtime)
(require 'slackit-http)
(require 'slackit-api)
(require 'slackit-realtime)
(require 'slackit-history)
(require 'slackit-emoji)
(require 'slackit-upload)
(require 'slackit-media)
(require 'slackit-code)
(require 'slackit-render)
(require 'slackit-completion)
(require 'slackit-compose)
(require 'slackit-room)
(require 'slackit-thread)
(require 'slackit-reaction)
(require 'slackit-read)
(require 'slackit-actions)
(require 'slackit-user)
(require 'slackit-root)
(require 'slackit-transient)

(defun slackit--bootstrap-start (_context input observe resolve reject)
  "Start the finite identity and cursor-complete bootstrap acquisition."
  (pcase-let ((`(,app ,_ ,_) input))
    (let ((remaining 2) (active t) identity-request pages)
      (cl-labels
          ((done () (when (and active (zerop (cl-decf remaining)))
                      (setq active nil) (funcall resolve t)))
           (failed (reason)
             (when active
               (setq active nil)
               (when (slackit-http-request-p identity-request)
                 (slackit-http-cancel identity-request))
               (when (appkit-handle-p pages) (appkit-cancel-handle pages))
               (funcall reject reason))))
        (setq identity-request
              (slackit-api-auth-test
               app :on-success (lambda (body)
                                 (when active (funcall observe 'identity body) (done)))
               :on-error #'failed))
        (when active
          (setq pages
                (slackit-api-conversations-list-all
                 app :on-page (lambda (items)
                                (when active (funcall observe 'conversations items)))
                 :on-complete (lambda ()
                                (when active (funcall observe 'conversations-complete nil) (done)))
                 :on-error #'failed)))
        (appkit-cancellation-create
         :kind 'transport
         :cancel (lambda ()
                   (setq active nil)
                   (when (slackit-http-request-p identity-request)
                     (slackit-http-cancel identity-request))
                   (when (appkit-handle-p pages) (appkit-cancel-handle pages))))))))

(defun slackit-bootstrap-update (context model message)
  "Commit bootstrap observations and return routing and acquisition commands."
  (pcase-let ((`(slackit-bootstrap ,phase ,input . ,payload) message))
    (pcase-let ((`(,app ,operation ,ready) input))
      (let ((state (slackit-runtime-model-state model)) commands)
        (when (slackit-runtime-operation-current-p app operation)
          (pcase phase
            ('begin
             (push (appkit-command-start-effect
                    (appkit-effect-create
                     :key 'bootstrap :input input :start #'slackit--bootstrap-start
                     :cancellation-requirement 'transport
                     :observe (lambda (input kind value)
                                (list 'slackit-bootstrap kind input value))
                     :observation-policy 'lossless :observation-pending-limit 128
                     :success (lambda (input &rest _) (list 'slackit-bootstrap 'complete input))
                     :failure (lambda (input reason) (list 'slackit-bootstrap 'failed input reason))))
                   commands))
            ('identity
             (let* ((body (car payload))
                    (team (alist-get 'team_id body)) (user (alist-get 'user_id body)))
               (if (not (slackit-runtime-bind-credential-identity app team user))
                   (progn
                     (slackit-state-set-bootstrap-error state "identity_mismatch")
                     (slackit-runtime-operation-end app operation)
                     (push (appkit-command-cancel-effect 'bootstrap) commands))
                 (let ((self (list (cons 'id user) (cons 'name (alist-get 'user body)))))
                   (slackit-state-put-team-self
                    state (list (cons 'id team) (cons 'name (alist-get 'team body))) self)
                   (slackit-state-put-user state self))
                 (slackit-state-set-bootstrap-complete state 'identity)
                 (when ready
                   (setf (slackit-runtime-model-realtime-p model) t)
                   (push (appkit-command-start-effect (slackit-emoji-catalog-effect app)) commands)))))
            ('conversations (slackit-state-put-conversations state (car payload)))
            ('conversations-complete (slackit-state-set-bootstrap-complete state 'conversations))
            ('complete (slackit-runtime-operation-end app operation))
            ('failed
             (slackit-state-set-bootstrap-error state
                                                (or (plist-get (car payload) :code) "request_failed"))
             (slackit-runtime-operation-end app operation))))
        (let ((next (slackit-runtime--app-update context model '(slackit-changes ((:kind bootstrap))))))
          (appkit-next :model model :render appkit-render-none
                       :commands (append (nreverse commands) (appkit-next-commands next))))))))


(defun slackit-bootstrap-account (app &optional start-services-p)
  "Commit finite identity and cursor-complete bootstrap for APP."
  (let* ((operation (slackit-runtime-operation-begin app '(bootstrap)))
         (state (slackit-runtime-state app)))
    (slackit-state-bootstrap-reset state)
    (slackit-state-set-bootstrap-complete state 'users)
    (appkit-app-send app (list 'slackit-bootstrap 'begin
                               (list app operation start-services-p)))
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
      (slackit-bootstrap-account
       app t))
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
                       (mapcar (lambda (app) (format "%s" (appkit-app-identity app)))
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
    (let ((ids (mapcar (lambda (app) (format "%s" (appkit-app-identity app)))
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

(with-eval-after-load 'evil
  (require 'slackit-evil nil t))

;;; slackit.el ends here
