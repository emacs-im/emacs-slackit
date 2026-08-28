;;; slackit-reaction.el --- Slack reaction operations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Account-owned, latest-intent reaction writes.  REST receipts settle work;
;; canonical counts and membership change only through Slack snapshots/events.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'slackit-api)
(require 'slackit-normalize)
(require 'slackit-runtime)
(require 'slackit-state)

(defun slackit-reaction--self-present-p (state conversation-id ts name)
  "Return non-nil when current user has reaction NAME on message TS."
  (let* ((message (slackit-state-message state conversation-id ts))
         (self-id (slackit-state-self-id state))
         (reaction
          (seq-find
           (lambda (item)
             (equal name (slackit-normalize-get item 'name)))
           (or (slackit-normalize-get message 'reactions) nil))))
    (and reaction
         (member self-id (slackit-normalize-get reaction 'users)))))

(defun slackit-reaction--dispatch (app operation)
  "Dispatch current desired state for reaction OPERATION in APP."
  (when (slackit-runtime-operation-current-p app operation)
    (let* ((payload (slackit-operation-payload operation))
           (conversation-id (plist-get payload :conversation-id))
           (ts (plist-get payload :ts))
           (name (plist-get payload :name))
           (add-p (plist-get payload :desired-add-p)))
      (setf (plist-get payload :sent-add-p) add-p)
      (cl-labels
          ((success (_body)
             (when (slackit-runtime-operation-current-p app operation)
               (let ((latest (slackit-operation-payload operation)))
                 (if (eq (plist-get latest :desired-add-p)
                         (plist-get latest :sent-add-p))
                     (slackit-runtime-operation-end app operation)
                   (slackit-reaction--dispatch app operation)))))
           (failure (error-data)
             (when (slackit-runtime-operation-current-p app operation)
               (slackit-runtime-operation-end app operation)
               (message "slackit: reaction write failed: %s"
                        (or (plist-get error-data :code) "request_failed")))))
        (if add-p
            (slackit-api-add-reaction
             app conversation-id ts name
             :on-success #'success :on-error #'failure)
          (slackit-api-remove-reaction
           app conversation-id ts name
           :on-success #'success :on-error #'failure))))))

(defun slackit-reaction-toggle (app conversation-id ts name)
  "Toggle reaction NAME on message TS using serialized latest intent."
  (let* ((key (list 'reaction conversation-id ts name))
         (existing (gethash key (appkit-app-request-table app))))
    (if (slackit-runtime-operation-current-p app existing)
        (let* ((payload (slackit-operation-payload existing))
               (desired (plist-get payload :desired-add-p)))
          (setf (plist-get payload :desired-add-p) (not desired))
          existing)
      (let* ((state (slackit-runtime-state app))
             (desired (not (slackit-reaction--self-present-p
                            state conversation-id ts name)))
             (operation
              (slackit-runtime-operation-begin
               app key nil nil
               (list :conversation-id conversation-id
                     :ts ts
                     :name name
                     :desired-add-p desired
                     :sent-add-p nil))))
        (slackit-reaction--dispatch app operation)
        operation))))

(provide 'slackit-reaction)

;;; slackit-reaction.el ends here
