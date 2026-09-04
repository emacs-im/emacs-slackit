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
(require 'appkit-compose-edit)
(require 'appkit-core)
(require 'appkit-surface)
(require 'appkit-chatbuf)
(require 'appkit-ui)
(require 'slackit-api)
(require 'slackit-customize)
(require 'slackit-code)
(require 'slackit-decode)
(require 'slackit-runtime)
(require 'slackit-upload)
(require 'slackit-state)

(declare-function slackit-room-current-app "slackit-room" ())
(declare-function slackit-room-current-conversation-id "slackit-room" ())
(declare-function slackit-room-current-view "slackit-room" ())

(defvar slackit-compose--attachment-serial 0
  "Process-local serial for opaque composer attachment identities.")

(defconst slackit-compose--snippet-size-limit (* 1024 1024)
  "Slack's documented external-upload limit for code snippets.")

(defun slackit-compose--escape (text)
  "Escape ordinary composer TEXT for Slack mrkdwn transport."
  (let ((value (substring-no-properties (or text ""))))
    (setq value (replace-regexp-in-string "&" "&amp;" value t t))
    (setq value (replace-regexp-in-string "<" "&lt;" value t t))
    (replace-regexp-in-string ">" "&gt;" value t t)))

(defun slackit-compose--code-block-object-p (object)
  "Return non-nil when OBJECT is a Slackit code block."
  (and (listp object) (eq (plist-get object :type) 'code-block)))

(defun slackit-compose--code-block-wire (code)
  "Return escaped Slack mrkdwn for CODE."
  (format "```\n%s\n```"
          (slackit-compose--escape
           (string-trim (or code "") "\n+" "\n+"))))

(defun slackit-compose--object-wire (object fallback)
  "Return Slack wire text for structured OBJECT or escaped FALLBACK."
  (let ((id (plist-get object :id)))
    (pcase (plist-get object :type)
      ('user (if id (format "<@%s>" id) (slackit-compose--escape fallback)))
      ('channel (if id (format "<#%s>" id) (slackit-compose--escape fallback)))
      ('code-block
       (slackit-compose--code-block-wire (plist-get object :code)))
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

(defun slackit-compose--rich-section-block (text)
  "Return one Slack mrkdwn section block containing TEXT."
  (when (> (length text) 3000)
    (user-error
     "slackit: text adjacent to a code block exceeds Slack's section limit"))
  `((type . "section")
    (text . ((type . "mrkdwn") (text . ,text)))))

(defun slackit-compose--rich-code-block (object)
  "Return one language-declared Slack rich-text block for code OBJECT."
  (let ((language (slackit-compose--language-token
                   (plist-get object :language)))
        (code (plist-get object :code)))
    (unless language
      (user-error "slackit: a code block requires an explicit language"))
    (slackit-compose--validate-code-block code)
    (list
     (cons 'type "rich_text")
     (cons
      'elements
      (vector
       (list
        (cons 'type "rich_text_preformatted")
        (cons 'elements
              (vector (list (cons 'type "text") (cons 'text code))))
        (cons 'border 0)
        (cons 'language language)))))))

(defun slackit-compose-blocks (input)
  "Compile INPUT containing code objects to ordered Slack Block Kit blocks.

Return nil when INPUT has no code block, preserving the ordinary text-only API
path.  Non-code spans retain Slack mrkdwn semantics through section blocks;
code spans become language-declared `rich_text_preformatted' elements."
  (let ((position 0)
        (finish (length (or input "")))
        ordinary
        blocks
        code-seen-p)
    (cl-labels
        ((flush-ordinary
           ()
           (let ((text (apply #'concat (nreverse ordinary))))
             (setq ordinary nil)
             (when (string-match-p "\\S-" text)
               (push (slackit-compose--rich-section-block text) blocks)))))
      (while (< position finish)
        (let* ((object
                (get-text-property
                 position appkit-chatbuf-input-object-property input))
               (next
                (appkit-chatbuf-next-input-object-change
                 position input finish))
               (span (substring input position next)))
          (if (slackit-compose--code-block-object-p object)
              (progn
                (flush-ordinary)
                (push (slackit-compose--rich-code-block object) blocks)
                (setq code-seen-p t))
            (push (slackit-compose-serialize span) ordinary))
          (setq position next)))
      (flush-ordinary))
    (when code-seen-p
      (setq blocks (nreverse blocks))
      (when (> (length blocks) 50)
        (user-error "slackit: code-rich message exceeds Slack's 50-block limit"))
      blocks)))

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
    ('snippet "Snippet")
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

(defun slackit-compose--attachment-actions ()
  "Return currently available configured attachment action names and commands."
  (cl-loop
   for (name predicate command) in slackit-compose-attach-commands
   when
   (and (stringp name)
        (commandp command)
        (or (null predicate)
            (and (functionp predicate)
                 (condition-case nil (funcall predicate) (error nil)))))
   collect (cons name command)))

(defun slackit-compose-attach ()
  "Choose an available attachment action, then invoke its command.

This is the Telega-style generic attachment entry point.  Direct local
media/file selection remains available through `slackit-compose-attach-file'."
  (interactive)
  (let ((actions (slackit-compose--attachment-actions)))
    (unless actions
      (user-error "slackit: no attachment actions are available"))
    (let* ((name (completing-read "Attach: " actions nil t))
           (command (alist-get name actions nil nil #'equal)))
      (call-interactively command))))

(defun slackit-compose--ensure-file-attachable ()
  "Reject composer states that cannot accept a Slack file object."
  (when (slackit-compose-upload-active-p)
    (user-error "slackit: wait for or cancel the current upload"))
  (when (eq (appkit-chatbuf-aux-type) 'edit)
    (user-error "slackit: file attachments cannot be added while editing")))

(defun slackit-compose--insert-attachment (attachment)
  "Insert validated ATTACHMENT atomically into the current composer."
  (slackit-compose--ensure-file-attachable)
  (let ((path (plist-get attachment :path)))
    (when (cl-find path (slackit-compose-attachments)
                   :key (lambda (item) (plist-get item :path))
                   :test #'equal)
      (user-error "slackit: this file is already attached")))
  (appkit-chatbuf-focus-input)
  (appkit-chatbuf-input-insert
   (slackit-compose--attachment-text attachment)
   :object attachment)
  attachment)

(defun slackit-compose--language-token (value)
  "Return validated optional Slack code-snippet language token VALUE."
  (let ((token (string-trim (or value ""))))
    (cond
     ((string-empty-p token) nil)
     ((not (string-match-p "\\`[[:alnum:]+#._-]+\\'" token))
      (user-error "slackit: language must be one token"))
     (t token))))

(defun slackit-compose--read-code-language (&optional default)
  "Read an explicit native code editing language, defaulting to DEFAULT."
  (let ((languages
         (sort
          (delete-dups
           (mapcar (lambda (entry) (format "%s" (car entry)))
                   org-src-lang-modes))
          #'string<)))
    (or
     (slackit-compose--language-token
      (completing-read
       "Code language: " languages nil nil nil nil default))
     (user-error "slackit: a code block requires an explicit language"))))

(defun slackit-compose--code-edit-mode (language)
  "Return native, remapped editing mode for explicit LANGUAGE."
  (let* ((native (and language
                      (slackit-code-mode-for-language language)))
         (remapped
          (and native
               (boundp 'major-mode-remap-alist)
               (alist-get native major-mode-remap-alist))))
    (if (commandp (or remapped native))
        (or remapped native)
      #'text-mode)))

(defun slackit-compose--validate-code-block (code)
  "Reject CODE that cannot be represented as one Slack mrkdwn block."
  (when (string-empty-p (string-trim code))
    (user-error "slackit: code block is empty"))
  (when (string-match-p "```" code)
    (user-error "slackit: fenced code belongs in a code snippet"))
  t)

(defun slackit-compose--code-block-presentation (object)
  "Return one-line composer card for code-block OBJECT."
  (let* ((code (or (plist-get object :code) ""))
         (lines (1+ (cl-count ?\n code)))
         (characters (length code)))
    (format "[Code block%s] %d line%s · %d char%s"
            (if-let* ((language (plist-get object :language)))
                (format " · %s" language)
              "")
            lines (if (= lines 1) "" "s")
            characters (if (= characters 1) "" "s"))))

(defun slackit-compose--insert-code-block-object (object existing)
  "Insert code-block OBJECT, replacing EXISTING when non-nil."
  (if existing
      (let ((region
             (slackit-compose--input-object-region
              (plist-get existing :id)
              #'slackit-compose--code-block-object-p)))
        (unless region
          (user-error "slackit: the code block changed while editing"))
        (goto-char (car region))
        (delete-region (car region) (cdr region)))
    (appkit-chatbuf-focus-input)
    (let ((current (appkit-chatbuf-input-state)))
      (unless (or (string-empty-p current)
                  (string-suffix-p "\n" current))
        (appkit-chatbuf-input-insert "\n"))))
  (appkit-chatbuf-input-insert
   (slackit-compose--code-block-presentation object)
   :object object
   :properties '(font-lock-face fixed-pitch))
  object)

(defun slackit-compose-insert-code-block (language)
  "Edit a Slack code block in a separate native-mode buffer, then insert it.

When point is on an existing Slackit code-block object, edit and replace that
object atomically.  Cancellation leaves the exact room/thread draft unchanged."
  (interactive
   (let ((object (appkit-chatbuf-input-object-at-point)))
     (list
      (slackit-compose--read-code-language
       (and (slackit-compose--code-block-object-p object)
            (plist-get object :language))))))
  (let* ((language
          (or (slackit-compose--language-token language)
              (user-error
               "slackit: a code block requires an explicit language")))
         (view (slackit-room-current-view))
         (composer-buffer (current-buffer))
         (existing
          (let ((object (appkit-chatbuf-input-object-at-point)))
            (and (slackit-compose--code-block-object-p object) object)))
         (code
          (appkit-compose-edit-buffer
           view (or (plist-get existing :code) "")
           :mode (slackit-compose--code-edit-mode language)
           :buffer-name "*Slackit Code Block*"
           :display-action slackit-compose-code-edit-display-buffer-action
           :validation-function #'slackit-compose--validate-code-block)))
    (when code
      (unless (and (appkit-surface-live-p view)
                   (buffer-live-p composer-buffer))
        (user-error "slackit: the originating composer is no longer live"))
      (with-current-buffer composer-buffer
        (unless (eq view (appkit-current-surface))
          (user-error "slackit: the originating composer changed"))
        (slackit-compose--insert-code-block-object
         (list :type 'code-block
               :id (or (plist-get existing :id)
                       (format "code-block-%x"
                               (cl-incf slackit-compose--attachment-serial)))
               :language language
               :code code)
         existing)))))

(defun slackit-compose-attach-file (file)
  "Attach readable local media or FILE to the current Slack composer."
  (interactive (list (read-file-name "Attach media/file: " nil nil t)))
  (slackit-compose--insert-attachment
   (slackit-compose--attachment-spec file)))

(defun slackit-compose--alt-text (value)
  "Return validated optional Slack image alternative text VALUE."
  (let ((text (string-trim (or value ""))))
    (cond
     ((string-empty-p text) nil)
     ((> (length text) 1000)
      (user-error "slackit: image description exceeds 1000 characters"))
     ((string-match-p "\0" text)
      (user-error "slackit: image description is invalid"))
     (t text))))

(defun slackit-compose-attach-image (file alt-text)
  "Attach local image FILE with optional screen-reader ALT-TEXT."
  (interactive
   (list (read-file-name "Attach image: " nil nil t)
         (read-string "Image description (optional): ")))
  (let ((attachment (slackit-compose--attachment-spec file))
        (description (slackit-compose--alt-text alt-text)))
    (unless (eq (plist-get attachment :kind) 'image)
      (user-error "slackit: selected file is not a recognized image"))
    (when description
      (setq attachment (plist-put attachment :alt-text description)))
    (slackit-compose--insert-attachment attachment)))

(defun slackit-compose-attach-code-snippet (file snippet-type)
  "Attach local FILE as a Slack code snippet of optional SNIPPET-TYPE."
  (interactive
   (list (read-file-name "Attach code snippet: " nil nil t)
         (read-string "Snippet language (optional): ")))
  (let ((attachment (slackit-compose--attachment-spec file))
        (language (slackit-compose--language-token snippet-type)))
    (when (> (plist-get attachment :size)
             slackit-compose--snippet-size-limit)
      (user-error "slackit: code snippets cannot exceed 1 MiB"))
    (setq attachment (plist-put attachment :kind 'snippet))
    (when language
      (setq attachment (plist-put attachment :snippet-type language)))
    (slackit-compose--insert-attachment attachment)))

(defun slackit-compose-clipboard-image-available-p ()
  "Return non-nil when this frame can acquire graphical clipboard data."
  (and (display-graphic-p) (fboundp 'gui-get-selection)))

(defun slackit-compose--clipboard-image-data ()
  "Return (SUFFIX . BYTES) for the first supported clipboard image."
  (or
   (cl-loop
    for (mime . suffix) in '((image/png . ".png") (image/jpeg . ".jpg"))
    for data =
    (let ((selection-coding-system 'no-conversion))
      (condition-case nil
          (gui-get-selection 'CLIPBOARD mime)
        (error nil)))
    when (and (stringp data) (> (length data) 0))
    return (cons suffix data))
   (user-error "slackit: the clipboard has no PNG or JPEG image")))

(defun slackit-compose--delete-temporary-file (state)
  "Delete private clipboard staging file and directory in STATE."
  (ignore-errors (delete-file (plist-get state :path)))
  (ignore-errors (delete-directory (plist-get state :directory))))

(defun slackit-compose--stage-clipboard-image (suffix bytes)
  "Stage clipboard image BYTES with SUFFIX in a private temporary directory."
  (let* ((directory (make-temp-file "slackit-compose-" t))
         (path (expand-file-name (concat "clipboard" suffix) directory))
         succeeded)
    (unwind-protect
        (condition-case nil
            (let ((coding-system-for-write 'no-conversion))
              (set-file-modes directory #o700)
              (write-region bytes nil path nil 'silent)
              (set-file-modes path #o600)
              (setq succeeded t)
              path)
          (error
           (user-error "slackit: could not stage the clipboard image")))
      (unless succeeded
        (slackit-compose--delete-temporary-file
         (list :path path :directory directory))))))

(defun slackit-compose-attach-clipboard-image (alt-text)
  "Attach a PNG or JPEG clipboard image with optional ALT-TEXT."
  (interactive (list (read-string "Image description (optional): ")))
  (slackit-compose--ensure-file-attachable)
  (pcase-let* ((`(,suffix . ,bytes) (slackit-compose--clipboard-image-data))
               (path (slackit-compose--stage-clipboard-image suffix bytes))
               (state (list :path path :directory (file-name-directory
                                                   (directory-file-name path))))
               (handle
                (appkit-register-handle
                 (slackit-room-current-view)
                 'slackit-compose-temporary-file state
                 #'slackit-compose--delete-temporary-file))
               (attachment (slackit-compose--attachment-spec path))
               (description (slackit-compose--alt-text alt-text)))
    (setq attachment (plist-put attachment :temporary-handle handle))
    (when description
      (setq attachment (plist-put attachment :alt-text description)))
    (condition-case error-data
        (slackit-compose--insert-attachment attachment)
      ((error quit)
       (appkit-cancel-handle handle)
       (signal (car error-data) (cdr error-data))))))

(defun slackit-compose--input-object-region (id predicate)
  "Return input region for object ID satisfying PREDICATE, or nil."
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
          (when (and (funcall predicate object)
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
                 (slackit-compose--input-object-region
                  (plist-get target :id)
                  #'slackit-compose--attachment-object-p)))
      (delete-region (car region) (cdr region))
      (appkit-chatbuf-input-state-sync)
      (when-let* ((handle (plist-get target :temporary-handle)))
        (appkit-cancel-handle handle))
      target)))

(defun slackit-compose--response-message
    (state conversation-id wire-text thread-ts body old-message)
  "Return canonical message from write BODY and captured semantic facts."
  (let* ((nested (alist-get 'message body))
         (ts (or (and nested (alist-get 'ts nested))
                 (alist-get 'ts body)
                 (and old-message (alist-get 'ts old-message))))
         (base (copy-tree (or nested old-message nil))))
    (dolist (key '(channel ts thread_ts text user))
      (setq base (assq-delete-all key base)))
    (slackit-decode-message
     (append `((channel . ,conversation-id)
               (ts . ,ts)
               (thread_ts . ,thread-ts)
               (text . ,wire-text)
               (user . ,(slackit-state-self-id state)))
             base)
     conversation-id)))

(defun slackit-compose--queue-settlement (view event)
  "Queue composer settlement EVENT for live VIEW."
  (when (appkit-surface-live-p view)
    (slackit-runtime-deliver view event)
    (slackit-runtime-render view (appkit-projection-change-create :full-p t :frame-p t))))

(defun slackit-compose--operation-key (view revision)
  "Return stable in-flight composer key for VIEW and REVISION."
  (list 'compose (appkit-surface-identity view) revision))

(defun slackit-compose--existing-operation-p (app key)
  "Return non-nil when APP already owns current operation KEY."
  (when-let* ((operation (gethash key (slackit-runtime-operations app))))
    (slackit-runtime-operation-current-p app operation)))

(defun slackit-compose--appkit-state-changed (_session)
  "Refresh generated composer status after Appkit compose state changes."
  (when-let* ((view (appkit-current-surface))
              ((appkit-surface-live-p view)))
    (slackit-runtime-render view (appkit-projection-change-create :full-p t :frame-p t))))

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
                (appkit-surface-live-p view)
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
                     :aux aux
                     :temporary-handles
                     (delq nil
                           (mapcar
                            (lambda (item)
                              (plist-get item :temporary-handle))
                            items)))
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
               :snippet-type (plist-get attachment :snippet-type)
               :alt-text (plist-get attachment :alt-text)
               :owner view
               :on-success
               (lambda (body)
                 (setq callback-ran-p t
                       current-http nil)
                 (when (current-p)
                   (let ((upload-url
                          (alist-get 'upload_url body))
                         (file-id
                          (alist-get 'file_id body)))
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
  (let* ((view (or (appkit-current-surface)
                   (user-error "slackit: no live chat view")))
         (app (slackit-room-current-app))
         (conversation-id (slackit-room-current-conversation-id))
         (input (appkit-chatbuf-input-state))
         (attachments (slackit-compose-attachments input))
         (wire-text (slackit-compose-serialize input))
         (blocks (slackit-compose-blocks input))
         (revision (appkit-chatbuf-composer-revision))
         (aux (copy-tree (appkit-chatbuf-aux-state)))
         (view-id (appkit-surface-identity view))
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
    (when (and attachments blocks)
      (user-error
       "slackit: send language-declared code blocks separately from files"))
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
               :blocks blocks
               :on-success
               (lambda (body)
                 (slackit-compose--settle-success
                  app operation view revision input aux conversation-id
                  wire-text (alist-get 'thread_ts old-message)
                  old-message captured-message-revision body))
               :on-error
               (lambda (error-data)
                 (slackit-compose--settle-failure
                  app operation view revision aux error-data))))
          (slackit-api-post-message
           app conversation-id wire-text
           :thread-ts root-ts
           :blocks blocks
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
         (dolist (handle (plist-get event :temporary-handles))
           (appkit-cancel-handle handle))
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
  (let* ((view (appkit-current-surface))
         (view-id (and (appkit-surface-live-p view)
                       (appkit-surface-identity view))))
    (unless (and view-id
                 (eq (appkit-app-type-name (appkit-app-type (appkit-surface-app view)))
                     'slackit-account)
                 (memq (car-safe view-id) '(room thread)))
      (user-error "slackit: no live room or thread view"))
    (when (appkit-chatbuf-aux-active-p)
      (let ((attachments (slackit-compose-attachments)))
        (appkit-chatbuf-input-set-text "")
        (appkit-chatbuf-aux-reset)
        (dolist (attachment attachments)
          (when-let* ((handle (plist-get attachment :temporary-handle)))
            (appkit-cancel-handle handle)))))))

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
