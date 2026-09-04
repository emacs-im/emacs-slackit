;;; slackit-read.el --- Slack conversation read advancement -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Monotonic, coalesced conversations.mark writes.  Receipts never fabricate
;; canonical read state; Slack marked events remain authoritative.

;;; Code:

(require 'cl-lib)
(require 'appkit-core)
(require 'appkit-chat-timeline)
(require 'slackit-api)
(require 'slackit-room)
(require 'slackit-runtime)
(require 'slackit-state)

(defun slackit-read--dispatch (app operation)
  "Dispatch current desired read timestamp for OPERATION in APP."
  (when (slackit-runtime-operation-current-p app operation)
    (let* ((payload (slackit-operation-payload operation))
           (conversation-id (plist-get payload :conversation-id))
           (ts (plist-get payload :desired-ts)))
      (setf (plist-get payload :sent-ts) ts)
      (slackit-api-mark-conversation
       app conversation-id ts
       :on-success
       (lambda (_body)
         (when (slackit-runtime-operation-current-p app operation)
           (let* ((latest (slackit-operation-payload operation))
                  (sent (plist-get latest :sent-ts))
                  (desired (plist-get latest :desired-ts)))
             (if (string< sent desired)
                 (slackit-read--dispatch app operation)
               (slackit-runtime-operation-end app operation)))))
       :on-error
       (lambda (error-data)
         (when (slackit-runtime-operation-current-p app operation)
           (slackit-runtime-operation-end app operation)
           (message "slackit: mark read failed: %s"
                    (or (plist-get error-data :code) "request_failed"))))))))

(defun slackit-read-mark (app conversation-id ts)
  "Coalesce APP CONVERSATION-ID read advancement through TS."
  (let* ((state (slackit-runtime-state app))
         (known (slackit-state-read-ts state conversation-id))
         (key (list 'mark conversation-id))
         (existing (gethash key (slackit-runtime-operations app))))
    (cond
     ((or (null ts) (and known (not (string< known ts)))) nil)
     ((slackit-runtime-operation-current-p app existing)
      (let* ((payload (slackit-operation-payload existing))
             (desired (plist-get payload :desired-ts)))
        (when (or (null desired) (string< desired ts))
          (setf (plist-get payload :desired-ts) ts))
        existing))
     (t
      (let ((operation
             (slackit-runtime-operation-begin
              app key nil nil
              (list :conversation-id conversation-id
                    :desired-ts ts
                    :sent-ts nil))))
        (slackit-read--dispatch app operation)
        operation)))))

(defun slackit-read-mark-at-point ()
  "Mark the current room read through the exact row at point."
  (interactive)
  (let* ((view (or (appkit-current-surface)
                   (user-error "slackit: no live room view")))
         (view-id (appkit-surface-identity view)))
    (unless (eq (car-safe view-id) 'room)
      (user-error "slackit: thread read marking is outside this slice"))
    (let ((ts (slackit-room-message-ts-at-point)))
      (unless (and ts (member ts (appkit-chat-timeline-keys)))
        (user-error "slackit: point has not reached an exact message row"))
      (slackit-read-mark
       (appkit-surface-app view)
       (slackit-room-current-conversation-id)
       ts)
      (message "slackit: marking read through %s" ts))))

(provide 'slackit-read)

;;; slackit-read.el ends here
