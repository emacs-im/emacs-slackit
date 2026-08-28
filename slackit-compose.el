;;; slackit-compose.el --- Slack composer serialization and writes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Structured exact-ID serialization and account-owned send/edit settlement.
;; HTTP callbacks update canonical state and queue view events; only Appkit sync
;; mutates the visible composer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'appkit-chatbuf)
(require 'slackit-api)
(require 'slackit-normalize)
(require 'slackit-runtime)
(require 'slackit-state)

(declare-function slackit-room-current-app "slackit-room" ())
(declare-function slackit-room-current-conversation-id "slackit-room" ())

(defun slackit-compose--escape (text)
  "Escape ordinary composer TEXT for Slack mrkdwn transport."
  (let ((value (substring-no-properties (or text ""))))
    (setq value (replace-regexp-in-string "&" "&amp;" value t t))
    (setq value (replace-regexp-in-string "<" "&lt;" value t t))
    (replace-regexp-in-string ">" "&gt;" value t t)))

(defun slackit-compose--object-wire (object fallback)
  "Return Slack wire text for structured OBJECT or escaped FALLBACK."
  (let ((id (plist-get object :id)))
    (pcase (plist-get object :type)
      ('user (if id (format "<@%s>" id) (slackit-compose--escape fallback)))
      ('channel (if id (format "<#%s>" id) (slackit-compose--escape fallback)))
      (_ (slackit-compose--escape fallback)))))

(defun slackit-compose-serialize (input)
  "Serialize property-preserving Appkit composer INPUT for Slack."
  (let ((position 0)
        (length (length (or input "")))
        parts)
    (while (< position length)
      (let* ((object (get-text-property
                      position appkit-chatbuf-input-object-property input))
             (object-text (get-text-property
                           position appkit-chatbuf-input-object-text-property input))
             (next (appkit-chatbuf-next-input-object-change
                    position input length))
             (text (substring input position next)))
        (push (if object
                  (concat
                   (slackit-compose--object-wire
                    object (or object-text text))
                   (if (and (stringp object-text)
                            (<= (length object-text) (length text)))
                       (slackit-compose--escape
                        (substring text (length object-text)))
                     ""))
                (slackit-compose--escape text))
              parts)
        (setq position next)))
    (apply #'concat (nreverse parts))))

(defun slackit-compose--response-message
    (state conversation-id wire-text thread-ts body old-message)
  "Return canonical message from write BODY and captured semantic facts."
  (let* ((nested (slackit-normalize-get body 'message))
         (ts (or (and nested (slackit-normalize-get nested 'ts))
                 (slackit-normalize-get body 'ts)
                 (and old-message (slackit-normalize-get old-message 'ts))))
         (base (copy-tree (or nested old-message nil))))
    (dolist (key '(channel ts thread_ts text user))
      (setq base (assq-delete-all key base)))
    (slackit-normalize-message
     (append `((channel . ,conversation-id)
               (ts . ,ts)
               (thread_ts . ,thread-ts)
               (text . ,wire-text)
               (user . ,(slackit-state-self-id state)))
             base)
     conversation-id)))

(defun slackit-compose--queue-settlement (view event)
  "Queue composer settlement EVENT for live VIEW."
  (when (appkit-view-live-p view)
    (appkit-view-enqueue-event view event)
    (appkit-request-sync view :part 'composer)))

(defun slackit-compose--operation-key (view revision)
  "Return stable in-flight composer key for VIEW and REVISION."
  (list 'compose (appkit-view-id view) revision))

(defun slackit-compose--existing-operation-p (app key)
  "Return non-nil when APP already owns current operation KEY."
  (when-let* ((operation (gethash key (appkit-app-request-table app))))
    (slackit-runtime-operation-current-p app operation)))

(defun slackit-compose--settle-success
    (app operation view revision input aux conversation-id wire-text
         thread-ts old-message captured-message-revision body)
  "Converge a successful composer OPERATION and queue view settlement."
  (when (slackit-runtime-operation-current-p app operation)
    (let* ((state (slackit-runtime-state app))
           (message (slackit-compose--response-message
                     state conversation-id wire-text thread-ts body old-message))
           (change (slackit-state-merge-write-snapshot
                    state conversation-id message captured-message-revision)))
      (slackit-runtime-operation-end app operation)
      (when change (slackit-runtime-publish-changes app (list change)))
      (slackit-compose--queue-settlement
       view (list :kind 'compose-success
                  :revision revision
                  :input input
                  :aux aux)))))

(defun slackit-compose--settle-failure
    (app operation view revision aux error-data)
  "Retire failed composer OPERATION and queue redacted ERROR-DATA."
  (when (slackit-runtime-operation-current-p app operation)
    (slackit-runtime-operation-end app operation)
    (slackit-compose--queue-settlement
     view (list :kind 'compose-failure
                :revision revision
                :aux aux
                :code (or (plist-get error-data :code) "request_failed")))))

(defun slackit-compose-submit ()
  "Send or edit the current Slackit composer without optimistic rows."
  (interactive)
  (let* ((view (or (appkit-current-view)
                   (user-error "slackit: no live chat view")))
         (app (slackit-room-current-app))
         (conversation-id (slackit-room-current-conversation-id))
         (input (appkit-chatbuf-input-state))
         (wire-text (slackit-compose-serialize input))
         (revision (appkit-chatbuf-composer-revision))
         (aux (copy-tree (appkit-chatbuf-aux-state)))
         (view-id (appkit-view-id view))
         (view-kind (car-safe view-id))
         (root-ts (and (eq view-kind 'thread) (nth 2 view-id)))
         (edit-p (eq (plist-get aux :aux-type) 'edit))
         (message-ts (and edit-p (plist-get aux :message-id)))
         (state (slackit-runtime-state app))
         (old-message (and message-ts
                           (slackit-state-message
                            state conversation-id message-ts)))
         (captured-message-revision
          (and message-ts
               (slackit-state-message-revision
                state conversation-id message-ts)))
         (key (slackit-compose--operation-key view revision)))
    (when (string-empty-p (string-trim wire-text))
      (user-error "slackit: message is empty"))
    (when (slackit-compose--existing-operation-p app key)
      (user-error "slackit: this composer revision is already in flight"))
    (let ((operation
           (slackit-runtime-operation-begin
            app key view revision
            (list :conversation-id conversation-id
                  :thread-ts root-ts
                  :message-ts message-ts))))
      (if edit-p
          (progn
            (unless old-message
              (slackit-runtime-operation-end app operation)
              (user-error "slackit: edited message no longer exists"))
            (slackit-api-update-message
             app conversation-id message-ts wire-text
             :on-success
             (lambda (body)
               (slackit-compose--settle-success
                app operation view revision input aux conversation-id
                wire-text (slackit-normalize-get old-message 'thread_ts)
                old-message captured-message-revision body))
             :on-error
             (lambda (error-data)
               (slackit-compose--settle-failure
                app operation view revision aux error-data))))
        (slackit-api-post-message
         app conversation-id wire-text
         :thread-ts root-ts
         :on-success
         (lambda (body)
           (slackit-compose--settle-success
            app operation view revision input aux conversation-id wire-text
            root-ts nil nil body))
         :on-error
         (lambda (error-data)
           (slackit-compose--settle-failure
            app operation view revision aux error-data)))))))

(defun slackit-compose-apply-settlement (event)
  "Apply composer settlement EVENT during the owning Appkit sync."
  (let ((revision (plist-get event :revision))
        (aux (plist-get event :aux)))
    (when (and (= revision (appkit-chatbuf-composer-revision))
               (equal aux (appkit-chatbuf-aux-state)))
      (pcase (plist-get event :kind)
        ('compose-success
         (appkit-chatbuf-input-history-push (plist-get event :input))
         (appkit-chatbuf-input-set-text "")
         (appkit-chatbuf-aux-reset)
         t)
        ('compose-failure
         (message "slackit: Slack write failed: %s"
                  (plist-get event :code))
         nil)))))

(defun slackit-compose-cancel-context ()
  "Cancel the current reply/edit context and clear its operation draft."
  (interactive)
  (let* ((view (appkit-current-view))
         (view-id (and (appkit-view-live-p view)
                       (appkit-view-id view))))
    (unless (and view-id
                 (eq (appkit-app-kind (appkit-view-app view))
                     'slackit-account)
                 (memq (car-safe view-id) '(room thread)))
      (user-error "slackit: no live room or thread view"))
    (when (appkit-chatbuf-aux-active-p)
      (appkit-chatbuf-input-set-text "")
      (appkit-chatbuf-aux-reset))))

(provide 'slackit-compose)

;;; slackit-compose.el ends here
