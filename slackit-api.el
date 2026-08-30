;;; slackit-api.el --- Typed Slack Web API boundary -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Endpoint names, parameters, cursor adaptation, Slack `ok' validation, and
;; typed asynchronous results.  Transport and credentials remain in
;; `slackit-http'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'slackit-http)
(require 'slackit-normalize)

(defun slackit-api--ok-p (body)
  "Return non-nil when Slack response BODY is successful."
  (eq (slackit-normalize-get body 'ok) t))

(defun slackit-api--error-code (body)
  "Return stable Slack error code from BODY."
  (let ((code (slackit-normalize-get body 'error)))
    (if code (format "%s" code) "invalid_response")))

(cl-defun slackit-api-request
    (app endpoint &key (method 'get) parameters idempotent-p owner
         on-success on-error)
  "Call Slack ENDPOINT for APP and validate its `ok' field."
  (slackit-http-request
   app endpoint
   :method method
   :parameters parameters
   :idempotent-p idempotent-p
   :owner owner
   :on-success
   (lambda (body)
     (if (slackit-api--ok-p body)
         (when on-success (funcall on-success body))
       (when on-error
         (funcall on-error
                  (list :status 200
                        :code (slackit-api--error-code body))))))
   :on-error on-error))

(defun slackit-api--next-cursor (body)
  "Return normalized next cursor from Slack BODY, or nil."
  (let* ((metadata (slackit-normalize-get body 'response_metadata))
         (cursor (slackit-normalize-get metadata 'next_cursor)))
    (and (stringp cursor) (not (string-empty-p cursor)) cursor)))

(cl-defun slackit-api--paginate
    (app endpoint item-key &key parameters owner on-page on-complete on-error)
  "Consume every cursor of read ENDPOINT ITEM-KEY for APP."
  (cl-labels
      ((request-page (cursor)
         (slackit-api-request
          app endpoint
          :method 'get
          :parameters (append parameters
                              (and cursor `((cursor . ,cursor))))
          :idempotent-p t
          :owner (or owner app)
          :on-success
          (lambda (body)
            (let ((items (or (slackit-normalize-get body item-key) nil))
                  (next (slackit-api--next-cursor body)))
              (when on-page (funcall on-page items))
              (if next
                  (request-page next)
                (when on-complete (funcall on-complete)))))
          :on-error on-error)))
    (request-page nil)))

(cl-defun slackit-api-auth-test (app &key on-success on-error)
  "Fetch stable team/self identity for APP."
  (slackit-api-request app "auth.test"
                       :method 'post
                       :on-success on-success
                       :on-error on-error))

(cl-defun slackit-api-emoji-list (app &key on-success on-error)
  "Fetch APP's custom emoji catalog."
  (slackit-api-request
   app "emoji.list"
   :method 'get
   :idempotent-p t
   :on-success on-success
   :on-error on-error))


(cl-defun slackit-api-user-info
    (app user-id &key owner on-success on-error)
  "Fetch exact Slack USER-ID for APP under optional lifecycle OWNER."
  (slackit-api-request
   app "users.info"
   :method 'get
   :parameters `((user . ,user-id))
   :idempotent-p t
   :owner owner
   :on-success on-success
   :on-error on-error))
(cl-defun slackit-api-conversations-open
    (app user-id &key owner on-success on-error)
  "Open or resume APP's direct-message conversation with USER-ID.

OWNER, when non-nil, owns cancellation of this non-retried write."
  (slackit-api-request
   app "conversations.open"
   :method 'post
   :parameters `((users . ,user-id))
   :owner owner
   :on-success on-success
   :on-error on-error))


(cl-defun slackit-api-conversations-list-all
    (app &key on-page on-complete on-error)
  "Fetch every joined-capable Slack conversation page for APP."
  (slackit-api--paginate
   app "conversations.list" 'channels
   :parameters '((limit . 200)
                 (exclude_archived . t)
                 (types . "public_channel,private_channel,mpim,im"))
   :on-page on-page
   :on-complete on-complete
   :on-error on-error))

(cl-defun slackit-api-conversation-history
    (app conversation-id &key cursor limit owner on-success on-error)
  "Fetch one history page for CONVERSATION-ID."
  (slackit-api-request
   app "conversations.history"
   :method 'get
   :parameters `((channel . ,conversation-id)
                 (limit . ,(or limit slackit-history-page-size))
                 ,@(and cursor `((cursor . ,cursor))))
   :idempotent-p t
   :owner (or owner app)
   :on-success on-success
   :on-error on-error))

(cl-defun slackit-api-conversation-replies
    (app conversation-id root-ts &key cursor limit owner on-success on-error)
  "Fetch one reply page for ROOT-TS in CONVERSATION-ID."
  (slackit-api-request
   app "conversations.replies"
   :method 'get
   :parameters `((channel . ,conversation-id)
                 (ts . ,root-ts)
                 (limit . ,(or limit slackit-history-page-size))
                 ,@(and cursor `((cursor . ,cursor))))
   :idempotent-p t
   :owner (or owner app)
   :on-success on-success
   :on-error on-error))

(cl-defun slackit-api-post-message
    (app conversation-id text &key thread-ts on-success on-error)
  "Post TEXT to CONVERSATION-ID, optionally under THREAD-TS."
  (slackit-api-request
   app "chat.postMessage"
   :method 'post
   :parameters `((channel . ,conversation-id)
                 (text . ,text)
                 ,@(and thread-ts `((thread_ts . ,thread-ts))))
   :on-success on-success
   :on-error on-error))

(cl-defun slackit-api-get-upload-url
    (app filename length &key owner on-success on-error)
  "Negotiate one external Slack upload for FILENAME containing LENGTH bytes."
  (slackit-api-request
   app "files.getUploadURLExternal"
   :method 'post
   :parameters `((filename . ,filename) (length . ,length))
   :owner (or owner app)
   :on-success on-success
   :on-error on-error))

(cl-defun slackit-api-complete-upload
    (app files conversation-id
         &key thread-ts initial-comment owner on-success on-error)
  "Finalize uploaded FILES and share them into CONVERSATION-ID.

FILES is an ordered list of alists carrying Slack file `id' and optional
`title'.  INITIAL-COMMENT may be empty for an attachment-only message."
  (slackit-api-request
   app "files.completeUploadExternal"
   :method 'post
   :parameters
   `((files . ,(json-serialize (vconcat files)))
     (channel_id . ,conversation-id)
     ,@(and thread-ts `((thread_ts . ,thread-ts)))
     ,@(and (stringp initial-comment)
            (not (string-empty-p initial-comment))
            `((initial_comment . ,initial-comment))))
   :owner (or owner app)
   :on-success on-success
   :on-error on-error))

(cl-defun slackit-api-update-message
    (app conversation-id ts text &key on-success on-error)
  "Replace message TS in CONVERSATION-ID with TEXT."
  (slackit-api-request
   app "chat.update"
   :method 'post
   :parameters `((channel . ,conversation-id) (ts . ,ts) (text . ,text))
   :on-success on-success
   :on-error on-error))

(cl-defun slackit-api-delete-message
    (app conversation-id ts &key on-success on-error)
  "Delete message TS from CONVERSATION-ID."
  (slackit-api-request
   app "chat.delete"
   :method 'post
   :parameters `((channel . ,conversation-id) (ts . ,ts))
   :on-success on-success
   :on-error on-error))

(cl-defun slackit-api-add-reaction
    (app conversation-id ts name &key on-success on-error)
  "Add reaction NAME to message TS in CONVERSATION-ID."
  (slackit-api-request
   app "reactions.add"
   :method 'post
   :parameters `((channel . ,conversation-id) (timestamp . ,ts) (name . ,name))
   :on-success on-success
   :on-error on-error))

(cl-defun slackit-api-remove-reaction
    (app conversation-id ts name &key on-success on-error)
  "Remove reaction NAME from message TS in CONVERSATION-ID."
  (slackit-api-request
   app "reactions.remove"
   :method 'post
   :parameters `((channel . ,conversation-id) (timestamp . ,ts) (name . ,name))
   :on-success on-success
   :on-error on-error))

(cl-defun slackit-api-mark-conversation
    (app conversation-id ts &key on-success on-error)
  "Mark CONVERSATION-ID read through TS."
  (slackit-api-request
   app "conversations.mark"
   :method 'post
   :parameters `((channel . ,conversation-id) (ts . ,ts))
   :on-success on-success
   :on-error on-error))

(provide 'slackit-api)

;;; slackit-api.el ends here
