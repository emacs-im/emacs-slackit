;;; slackit-actions.el --- Stable Slack message actions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Row-keyed reply/thread, edit, delete, reaction, and copy entry points.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-chatbuf)
(require 'slackit-api)
(require 'slackit-completion)
(require 'slackit-reaction)
(require 'slackit-room)
(require 'slackit-runtime)
(require 'slackit-state)
(require 'slackit-thread)

(defun slackit-actions--context ()
  "Return `(APP CONVERSATION-ID TS MESSAGE)' for the row at point."
  (let* ((app (slackit-room-current-app))
         (conversation-id (slackit-room-current-conversation-id))
         (ts (or (slackit-room-message-ts-at-point)
                 (user-error "slackit: no message at point")))
         (message (slackit-state-message
                   (slackit-runtime-state app) conversation-id ts)))
    (unless message (user-error "slackit: message no longer exists"))
    (list app conversation-id ts message)))

(defun slackit-actions--require-owned (state message)
  "Reject MESSAGE unless it belongs to current user in STATE."
  (unless (equal (slackit-state-self-id state)
                 (slackit-normalize-get message 'user))
    (user-error "slackit: this action requires your own message")))

(defun slackit-actions-open-thread ()
  "Open the stable thread for the message at point."
  (interactive)
  (pcase-let* ((`(,app ,conversation-id ,ts ,message)
                 (slackit-actions--context))
                (root-ts (or (slackit-normalize-get message 'thread_ts) ts)))
    (slackit-thread-open app conversation-id root-ts t)))

(defun slackit-actions-edit ()
  "Edit the owned message at point in the current composer."
  (interactive)
  (pcase-let* ((`(,app ,_conversation-id ,ts ,message)
                 (slackit-actions--context))
                (state (slackit-runtime-state app)))
    (slackit-actions--require-owned state message)
    (unless (appkit-chatbuf-composer-idle-p)
      (user-error "slackit: finish or clear the current composer first"))
    (appkit-chatbuf-aux-set
     (list :aux-type 'edit
           :message-id ts
           :title "Edit message"
           :preview (slackit-normalize-get message 'text)))
    (appkit-chatbuf-input-set-text
     (slackit-completion-decode-wire
      state (or (slackit-normalize-get message 'text) "")))
    (appkit-chatbuf-focus-input)))

(defun slackit-actions--delete-success
    (app operation conversation-id ts _body)
  "Converge successful delete OPERATION for message TS."
  (when (slackit-runtime-operation-current-p app operation)
    (slackit-runtime-operation-end app operation)
    (when-let* ((change (slackit-state-delete-message
                         (slackit-runtime-state app) conversation-id ts)))
      (slackit-runtime-publish-changes app (list change)))))

(defun slackit-actions-delete ()
  "Delete the owned message at point after confirmation."
  (interactive)
  (pcase-let* ((`(,app ,conversation-id ,ts ,message)
                 (slackit-actions--context))
                (state (slackit-runtime-state app)))
    (slackit-actions--require-owned state message)
    (when (yes-or-no-p "Delete this Slack message? ")
      (let* ((key (list 'delete conversation-id ts))
             (existing (gethash key (appkit-app-request-table app))))
        (when (slackit-runtime-operation-current-p app existing)
          (user-error "slackit: delete is already in flight"))
        (let ((operation
               (slackit-runtime-operation-begin app key)))
          (slackit-api-delete-message
           app conversation-id ts
           :on-success
           (apply-partially #'slackit-actions--delete-success
                            app operation conversation-id ts)
           :on-error
           (lambda (error-data)
             (when (slackit-runtime-operation-current-p app operation)
               (slackit-runtime-operation-end app operation)
               (message "slackit: delete failed: %s"
                        (or (plist-get error-data :code)
                            "request_failed"))))))))))

(defun slackit-actions-react (name)
  "Toggle text reaction NAME on the message at point."
  (interactive (list (read-string "Reaction name (without colons): ")))
  (unless (and (stringp name)
               (string-match-p "\\`[+[:alnum:]_-]+\\'" name))
    (user-error "slackit: invalid reaction name"))
  (pcase-let ((`(,app ,conversation-id ,ts ,_message)
               (slackit-actions--context)))
    (slackit-reaction-toggle app conversation-id ts name)))

(defun slackit-actions-copy-text ()
  "Copy canonical Slack message text at point."
  (interactive)
  (pcase-let ((`(,_app ,_conversation-id ,_ts ,message)
               (slackit-actions--context)))
    (let ((text (or (slackit-normalize-get message 'text) "")))
      (kill-new text)
      (message "slackit: message text copied"))))

(provide 'slackit-actions)

;;; slackit-actions.el ends here
