;;; slackit-compose.el --- Slack composer serialization and writes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Structured exact-ID serialization and account-owned send/edit settlement.
;; HTTP callbacks update canonical state and queue view events; only Appkit sync
;; mutates the visible composer.

;;; Code:

(require 'cl-lib)
(require 'mailcap)
(require 'subr-x)
(require 'appkit-compose)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'appkit-chatbuf)
(require 'appkit-ui)
(require 'slackit-api)
(require 'slackit-normalize)
(require 'slackit-runtime)
(require 'slackit-upload)
(require 'slackit-state)

(declare-function slackit-room-current-app "slackit-room" ())
(declare-function slackit-room-current-conversation-id "slackit-room" ())

(defvar slackit-compose--attachment-serial 0
  "Process-local serial for opaque composer attachment identities.")

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

(defun slackit-compose--attachment-object-p (object)
  "Return non-nil when OBJECT is a Slackit local-file attachment."
  (and (listp object) (eq (plist-get object :type) 'file)))

(defun slackit-compose-attachments (&optional input)
  "Return ordered Slackit attachment objects from composer INPUT."
  (let* ((value (or input (appkit-chatbuf-input-state)))
         (finish (length value))
         (position 0)
         attachments)
    (while (< position finish)
      (let* ((object
              (get-text-property
               position appkit-chatbuf-input-object-property value))
             (next
              (appkit-chatbuf-next-input-object-change
               position value finish)))
        (when (slackit-compose--attachment-object-p object)
          (push (copy-sequence object) attachments))
        (setq position next)))
    (nreverse attachments)))

(defun slackit-compose-strip-attachments (input)
  "Return INPUT with Slackit attachment spans removed."
  (let ((finish (length (or input "")))
        (position 0)
        pieces)
    (while (< position finish)
      (let* ((object
              (get-text-property
               position appkit-chatbuf-input-object-property input))
             (next
              (appkit-chatbuf-next-input-object-change
               position input finish)))
        (unless (slackit-compose--attachment-object-p object)
          (push (substring input position next) pieces))
        (setq position next)))
    (apply #'concat (nreverse pieces))))

(defun slackit-compose-serialize (input)
  "Serialize property-preserving Appkit composer INPUT for Slack.

Attachment objects are transport plans, not visible Slack message text."
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
        (push
         (cond
          ((slackit-compose--attachment-object-p object) "")
          (object
           (concat
            (slackit-compose--object-wire object (or object-text text))
            (if (and (stringp object-text)
                     (<= (length object-text) (length text)))
                (slackit-compose--escape
                 (substring text (length object-text)))
              "")))
          (t (slackit-compose--escape text)))
         parts)
        (setq position next)))
    (apply #'concat (nreverse parts))))

(defun slackit-compose--attachment-kind (name)
  "Return presentation kind for local attachment NAME."
  (let ((mime
         (mailcap-extension-to-mime
          (downcase (or (file-name-extension name) "")))))
    (cond
     ((and mime (string-prefix-p "image/" mime)) 'image)
     ((and mime (string-prefix-p "video/" mime)) 'video)
     ((and mime (string-prefix-p "audio/" mime)) 'audio)
     (t 'file))))

(defun slackit-compose--attachment-kind-label (kind)
  "Return concise visible label for attachment KIND."
  (pcase kind
    ('image "Image")
    ('video "Video")
    ('audio "Audio")
    (_ "File")))

(defun slackit-compose--attachment-spec (file)
  "Return validated Slackit attachment object for local FILE."
  (let* ((path (expand-file-name file))
         (name (file-name-nondirectory path)))
    (when (file-remote-p path)
      (user-error "slackit: remote attachment paths are unsupported"))
    (unless (and (file-regular-p path) (file-readable-p path))
      (user-error "slackit: attachment is not a readable regular file"))
    (when (or (string-empty-p name)
              (string-match-p "[\0\r\n]" name))
      (user-error "slackit: attachment filename is invalid"))
    (let ((size (file-attribute-size (file-attributes path 'string))))
      (unless (and (integerp size) (> size 0))
        (user-error "slackit: empty attachments cannot be uploaded"))
      (list :type 'file
            :id (format "attachment-%x"
                        (cl-incf slackit-compose--attachment-serial))
            :path path
            :name name
            :size size
            :kind (slackit-compose--attachment-kind name)))))

(defun slackit-compose--attachment-text (attachment)
  "Return safe visible composer text for ATTACHMENT."
  (format "[%s] %s · %s"
          (slackit-compose--attachment-kind-label
           (plist-get attachment :kind))
          (plist-get attachment :name)
          (file-size-human-readable (plist-get attachment :size))))

(defun slackit-compose-upload-active-p ()
  "Return non-nil when Appkit owns the current Slack upload operation."
  (and (bound-and-true-p appkit-compose-session-mode)
       (appkit-compose-operation-active-p)
       (eq (appkit-compose-operation-kind) 'slack-upload)))

(defun slackit-compose-attach-file (file)
  "Attach readable local FILE to the current Slack composer."
  (interactive (list (read-file-name "Attach file: " nil nil t)))
  (when (slackit-compose-upload-active-p)
    (user-error "slackit: wait for or cancel the current upload"))
  (when (eq (appkit-chatbuf-aux-type) 'edit)
    (user-error "slackit: file attachments cannot be added while editing"))
  (let* ((attachment (slackit-compose--attachment-spec file))
         (path (plist-get attachment :path)))
    (when (cl-find path (slackit-compose-attachments)
                   :key (lambda (item) (plist-get item :path))
                   :test #'equal)
      (user-error "slackit: this file is already attached"))
    (appkit-chatbuf-focus-input)
    (appkit-chatbuf-input-insert
     (slackit-compose--attachment-text attachment)
     :object attachment)
    attachment))

(defun slackit-compose--attachment-region (id)
  "Return current input region occupied by attachment ID, or nil."
  (when-let* ((bounds (appkit-chatbuf-input-region-bounds)))
    (let ((position (car bounds))
          (finish (cdr bounds))
          found)
      (while (and (< position finish) (not found))
        (let* ((object
                (get-text-property
                 position appkit-chatbuf-input-object-property))
               (next
                (appkit-chatbuf-next-input-object-change
                 position nil finish)))
          (when (and (slackit-compose--attachment-object-p object)
                     (equal id (plist-get object :id)))
            (setq found (cons position next)))
          (setq position next)))
      found)))

(defun slackit-compose-remove-attachment (&optional attachment)
  "Remove ATTACHMENT from the current Slack composer.

Interactively prefer the attachment at point, otherwise select by safe name."
  (interactive)
  (when (slackit-compose-upload-active-p)
    (user-error "slackit: wait for or cancel the current upload"))
  (let* ((at-point
          (let ((object (appkit-chatbuf-input-object-at-point)))
            (and (slackit-compose--attachment-object-p object) object)))
         (attachments (slackit-compose-attachments))
         (target
          (or attachment at-point
              (when attachments
                (let ((index 0)
                      choices)
                  (dolist (item attachments)
                    (cl-incf index)
                    (push
                     (cons (format "%s (%d)" (plist-get item :name) index)
                           item)
                     choices))
                  (setq choices (nreverse choices))
                  (cdr
                   (assoc
                    (completing-read
                     "Remove attachment: " choices nil t)
                    choices)))))))
    (unless target
      (user-error "slackit: this composer has no attachment"))
    (when-let* ((region
                 (slackit-compose--attachment-region
                  (plist-get target :id))))
      (delete-region (car region) (cdr region))
      (appkit-chatbuf-input-state-sync)
      target)))

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

(defun slackit-compose--appkit-state-changed (_session)
  "Refresh generated composer status after Appkit compose state changes."
  (when-let* ((view (appkit-current-view))
              ((appkit-view-live-p view)))
    (appkit-request-sync view :part 'frame)))

(defun slackit-compose-setup ()
  "Attach Appkit compose ownership to the current Slack chatbuf."
  (appkit-compose-setup
   :snapshot-function #'appkit-chatbuf-input-state
   :source-bounds-function #'appkit-chatbuf-input-region-bounds
   :state-change-function #'slackit-compose--appkit-state-changed))

(defun slackit-compose-upload-card ()
  "Return the Appkit-owned upload status card, or an empty string."
  (if (not (slackit-compose-upload-active-p))
      ""
    (let* ((label (or (appkit-compose-label) "Sending attachments"))
           (progress (appkit-compose-progress))
           (status
            (if (numberp progress)
                (format "%s  %s %d%%"
                        label
                        (appkit-compose-progress-bar progress 10)
                        (round (* 100 progress)))
              label)))
      (appkit-chatbuf-aux-render
       :title status
       :preview (appkit-ui-one-line-preview-create :text "")
       :cancel-action #'slackit-compose-cancel-upload
       :cancel-help "Cancel this Slack file upload"))))

(defun slackit-compose-cancel-upload ()
  "Ask Appkit to cancel the current upload while retaining its draft."
  (interactive)
  (unless (slackit-compose-upload-active-p)
    (user-error "slackit: this composer has no active upload"))
  (appkit-compose-cancel-operation)
  (message "slackit: upload canceled"))

(defun slackit-compose--attachment-readable-size (attachment)
  "Return current positive byte size for ATTACHMENT, or signal a user error."
  (let* ((path (plist-get attachment :path))
         (attributes
          (and (stringp path)
               (not (file-remote-p path))
               (file-regular-p path)
               (file-readable-p path)
               (file-attributes path 'string)))
         (size (and attributes (file-attribute-size attributes))))
    (unless (and (integerp size) (> size 0))
      (user-error "slackit: an attachment is no longer readable"))
    size))

(defun slackit-compose--submit-upload
    (app view revision input aux conversation-id thread-ts wire-text attachments)
  "Submit ATTACHMENTS and captured composer values through one Appkit effect."
  (let* ((buffer (current-buffer))
         (generation (appkit-compose-generation))
         (items (mapcar #'copy-sequence attachments))
         (count (length items))
         (index 0)
         uploaded
         current-http
         current-transfer
         owner
         canceled)
    (cl-labels
        ((owned-p ()
           (and owner
                (buffer-live-p buffer)
                (with-current-buffer buffer
                  (appkit-compose-operation-current-p owner))))
         (current-p ()
           (and (not canceled)
                (appkit-view-live-p view)
                (owned-p)))
         (update (label progress)
           (when (current-p)
             (with-current-buffer buffer
               (appkit-compose-operation-update
                owner :label label :progress progress))))
         (finish (kind &optional error-data)
           (when (current-p)
             (with-current-buffer buffer
               (appkit-compose-operation-finish owner))
             (slackit-compose--queue-settlement
              view
              (append
               (list :kind kind
                     :revision revision
                     :input (slackit-compose-strip-attachments input)
                     :aux aux)
               (and error-data
                    (list
                     :code
                     (or (plist-get error-data :code) "upload_failed")
                     :unknown-outcome
                     (and (plist-get error-data :unknown-outcome) t)))))))
         (fail (error-data)
           (finish 'compose-failure error-data))
         (cancel ()
           (unless canceled
             (setq canceled t)
             (when current-http
               (slackit-http-cancel current-http)
               (setq current-http nil))
             (when current-transfer
               (slackit-upload-transfer-cancel current-transfer)
               (setq current-transfer nil))
             (when (owned-p)
               (with-current-buffer buffer
                 (appkit-compose-operation-finish owner)))))
         (file-label (verb attachment)
           (format "%s %s%s"
                   verb
                   (plist-get attachment :name)
                   (if (> count 1)
                       (format " %d/%d" (1+ index) count)
                     "")))
         (upload-progress (_transfer progress)
           (when (current-p)
             (update
              (file-label "Uploading" (nth index items))
              (/ (+ index (max 0.0 (min 1.0 progress)))
                 (float count)))))
         (start-transfer (attachment upload-url file-id)
           (let ((callback-ran-p nil)
                 transfer)
             (update
              (file-label "Uploading" attachment)
              (/ index (float count)))
             (setq
              transfer
              (slackit-upload-file
               app view upload-url (plist-get attachment :path)
               :on-success
               (lambda (_transfer)
                 (setq callback-ran-p t
                       current-transfer nil)
                 (when (current-p)
                   (setq uploaded
                         (append
                          uploaded
                          (list
                           `((id . ,file-id)
                             (title . ,(plist-get attachment :name)))))
                         index (1+ index))
                   (next)))
               :on-error
               (lambda (_transfer error-data)
                 (setq callback-ran-p t
                       current-transfer nil)
                 (when (current-p)
                   (fail error-data)))
               :on-progress #'upload-progress))
             (when (and (not callback-ran-p)
                        (current-p)
                        (not (slackit-upload-transfer-settled-p transfer)))
               (setq current-transfer transfer))))
         (negotiate (attachment)
           (let ((size (slackit-compose--attachment-readable-size attachment))
                 (callback-ran-p nil)
                 request)
             (update
              (file-label "Preparing" attachment)
              (/ index (float count)))
             (setq
              request
              (slackit-api-get-upload-url
               app (plist-get attachment :name) size
               :owner view
               :on-success
               (lambda (body)
                 (setq callback-ran-p t
                       current-http nil)
                 (when (current-p)
                   (let ((upload-url
                          (slackit-normalize-get body 'upload_url))
                         (file-id
                          (slackit-normalize-get body 'file_id)))
                     (if (and (slackit-upload-url-p upload-url)
                              (stringp file-id)
                              (not (string-empty-p file-id)))
                         (start-transfer attachment upload-url file-id)
                       (fail '(:code "invalid_upload_response"))))))
               :on-error
               (lambda (error-data)
                 (setq callback-ran-p t
                       current-http nil)
                 (when (current-p)
                   (fail error-data)))))
             (when (and (not callback-ran-p) (current-p))
               (setq current-http request))))
         (complete ()
           (let ((callback-ran-p nil)
                 request)
             (update
              (format "Sharing %d file%s"
                      count (if (= count 1) "" "s"))
              1.0)
             (setq
              request
              (slackit-api-complete-upload
               app uploaded conversation-id
               :thread-ts thread-ts
               :initial-comment wire-text
               :owner view
               :on-success
               (lambda (_body)
                 (setq callback-ran-p t
                       current-http nil)
                 (when (current-p)
                   (finish 'compose-success)))
               :on-error
               (lambda (error-data)
                 (setq callback-ran-p t
                       current-http nil)
                 (when (current-p)
                   (fail
                    (list
                     :code
                     (or (plist-get error-data :code)
                         "complete_upload_failed")
                     :unknown-outcome t))))))
             (when (and (not callback-ran-p) (current-p))
               (setq current-http request))))
         (next ()
           (when (current-p)
             (if (< index count)
                 (negotiate (nth index items))
               (complete)))))
      (setq
       owner
       (appkit-compose-operation-begin
        'slack-upload
        :generation generation
        :label
        (if (> count 1)
            (format "Preparing attachments 1/%d" count)
          "Preparing attachment")
        :progress 0.0
        :cancel-function #'cancel))
      (condition-case error-data
          (next)
        ((error quit)
         (cancel)
         (signal (car error-data) (cdr error-data))))
      owner)))

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
  "Send or edit the current Slackit composer without optimistic rows.

Local attachments run through Slack's external upload workflow and are shared
with the captured text as one completion write."
  (interactive)
  (let* ((view (or (appkit-current-view)
                   (user-error "slackit: no live chat view")))
         (app (slackit-room-current-app))
         (conversation-id (slackit-room-current-conversation-id))
         (input (appkit-chatbuf-input-state))
         (attachments (slackit-compose-attachments input))
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
    (when (and (null attachments)
               (string-empty-p (string-trim wire-text)))
      (user-error "slackit: message is empty"))
    (when (slackit-compose-upload-active-p)
      (user-error "slackit: this composer already owns an upload"))
    (when (and edit-p attachments)
      (user-error "slackit: file attachments cannot be added while editing"))
    (if attachments
        (slackit-compose--submit-upload
         app view revision input aux conversation-id root-ts wire-text attachments)
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
              app operation view revision aux error-data))))))))

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
         (if (plist-get event :unknown-outcome)
             (message
              "slackit: upload completion outcome is unknown; check Slack before retrying")
           (message "slackit: Slack write failed: %s"
                    (plist-get event :code)))
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

(defun slackit-compose-cancel-dwim ()
  "Cancel an upload first, otherwise clear reply/edit composer context."
  (interactive)
  (cond
   ((slackit-compose-upload-active-p)
    (slackit-compose-cancel-upload))
   ((appkit-chatbuf-aux-active-p)
    (slackit-compose-cancel-context))
   (t
    (user-error "slackit: this composer has nothing to cancel"))))

(provide 'slackit-compose)

;;; slackit-compose.el ends here
