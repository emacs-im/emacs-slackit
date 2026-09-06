;;; slackit-code.el --- Protocol-led Slack code highlighting -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Display-only inline/fenced code styling and native Font Lock projection.
;; Slack-provided language metadata is authoritative.  Unknown or absent
;; languages remain fixed-pitch and never trigger content inference.

;;; Code:

(require 'cl-lib)
(require 'org-src)
(require 'subr-x)
(require 'appkit-core)

(defgroup slackit-code nil
  "Display-only syntax highlighting for Slack code."
  :group 'slackit
  :prefix "slackit-code-")

(defcustom slackit-code-fontify-blocks t
  "When non-nil, fontify code blocks with an authoritative language mode."
  :type 'boolean
  :group 'slackit-code)

(defcustom slackit-code-cache-limit 256
  "Maximum number of fontified code blocks retained per account."
  :type 'integer
  :group 'slackit-code)

(defface slackit-inline-code
  '((t :inherit (fixed-pitch font-lock-constant-face)))
  "Face for inline Slack code spans."
  :group 'slackit-code)

(defface slackit-code-block
  '((t :inherit fixed-pitch))
  "Base face appended to fenced Slack code blocks."
  :group 'slackit-code)

(cl-defstruct (slackit-code-descriptor
               (:constructor slackit-code-descriptor-create))
  "One ordered code block descriptor extracted from Slack Block Kit."
  text
  language
  source-kind
  border)

(cl-defstruct (slackit-code-cache
               (:constructor slackit-code-cache-create))
  "One account-owned bounded code presentation cache."
  app
  entries
  handle)

(defvar slackit-code--app-caches (make-hash-table :test #'eq)
  "Live Slackit app to its account-owned code cache.")

(defvar slackit-code--fontification-buffers (make-hash-table :test #'eq)
  "Major mode to an empty, explicitly owned Font Lock scratch buffer.")

(defvar-local slackit-code--fontification-owner-p nil
  "Non-nil only in a Slackit-owned code fontification buffer.")

(defvar-local slackit-code--fontification-mode nil
  "Major mode identity owned by the current fontification buffer.")

(defun slackit-code--present-string (value)
  "Return non-empty string VALUE, or nil."
  (and (stringp value) (not (string-empty-p value)) value))

(defun slackit-code--normalize-language (language)
  "Return safe normalized Slack LANGUAGE, or nil."
  (when language
    (let ((value (downcase (string-trim (format "%s" language)))))
      (and (<= (length value) 64)
           (string-match-p "\\`[[:alnum:]+#._-]+\\'" value)
           value))))

(defun slackit-code--element-text (element)
  "Return display text contributed by one preformatted ELEMENT."
  (pcase (alist-get 'type element)
    ("text" (or (alist-get 'text element) ""))
    ("link" (or (alist-get 'text element)
                (alist-get 'url element)
                ""))
    (_ "")))

(defun slackit-code--preformatted-descriptor (element)
  "Return a code descriptor for rich-text preformatted ELEMENT."
  (slackit-code-descriptor-create
   :text (mapconcat #'slackit-code--element-text
                    (or (alist-get 'elements element) nil)
                    "")
   :language
   (slackit-code--normalize-language
    (alist-get 'language element))
   :source-kind 'rich-text-preformatted
   :border (alist-get 'border element)))

(defun slackit-code--markdown-descriptors (text)
  "Return language-tagged fenced code descriptors parsed from Markdown TEXT."
  (let ((source (or text ""))
        (position 0)
        result)
    (while (string-match
            "```[ \t]*\\([[:alnum:]+#._-]+\\)[ \t]*\r?\n"
            source position)
      (let* ((language (slackit-code--normalize-language (match-string 1 source)))
             (content-start (match-end 0))
             (close
              (string-match "\n```[ \t]*\\(?:\r?\n\\|\\'\\)"
                            source content-start)))
        (if (null close)
            (setq position (length source))
          (let ((content-end
                 (if (eq (aref source close) ?\n) close (match-beginning 0))))
            (push
             (slackit-code-descriptor-create
              :text (substring source content-start content-end)
              :language language
              :source-kind 'markdown
              :border nil)
             result))
          (setq position (match-end 0)))))
    (nreverse result)))

(defun slackit-code-message-descriptors (message)
  "Return ordered code descriptors retained in normalized Slack MESSAGE."
  (cl-labels
      ((collect (value)
         (cond
          ((and (listp value) (assq 'type value))
           (pcase (alist-get 'type value)
             ("rich_text_preformatted"
              (list (slackit-code--preformatted-descriptor value)))
             ("markdown"
              (slackit-code--markdown-descriptors
               (alist-get 'text value)))
             (_ (collect (alist-get 'elements value)))))
          ((listp value) (mapcan #'collect value))
          (t nil))))
    (collect (alist-get 'blocks message))))

(defun slackit-code-consume-descriptor (descriptors text)
  "Match exact TEXT in DESCRIPTORS.

Return a plist with `:descriptor' and `:remaining'.  Only the matched instance
is consumed, so duplicate code blocks retain their distinct ordered metadata."
  (let ((tail descriptors)
        prefix
        match)
    (while (and tail (null match))
      (if (equal text (slackit-code-descriptor-text (car tail)))
          (setq match (car tail))
        (push (car tail) prefix)
        (setq tail (cdr tail))))
    (list :descriptor match
          :remaining
          (if match
              (append (nreverse prefix) (cdr tail))
            descriptors))))

(defun slackit-code-mode-for-language (language)
  "Return Emacs's source major mode for Slack LANGUAGE, or nil."
  (when-let* ((name (slackit-code--normalize-language language)))
    (org-src-get-lang-mode-if-bound name)))

(defun slackit-code--fontification-buffer (mode)
  "Return Slackit's empty, explicitly owned scratch buffer for MODE."
  (let ((buffer (gethash mode slackit-code--fontification-buffers)))
    (unless (and (buffer-live-p buffer)
                 (buffer-local-value 'slackit-code--fontification-owner-p
                                     buffer)
                 (eq mode
                     (buffer-local-value 'slackit-code--fontification-mode
                                         buffer)))
      (setq buffer
            (generate-new-buffer
             (format " *slackit-code-fontification:%s*" mode)))
      (with-current-buffer buffer
        (setq-local slackit-code--fontification-owner-p t)
        (setq-local slackit-code--fontification-mode mode)
        (setq-local buffer-undo-list t))
      (puthash mode buffer slackit-code--fontification-buffers))
    buffer))

(defun slackit-code--next-face-change (text position length)
  "Return next face-property boundary in TEXT after POSITION before LENGTH."
  (min (or (next-single-property-change position 'face text) length)
       (or (next-single-property-change position 'font-lock-face text) length)))

(defun slackit-code--sanitize-font-lock (text)
  "Return TEXT retaining only native face presentation properties."
  (let* ((source (or text ""))
         (result (substring-no-properties source))
         (length (length source))
         (position 0))
    (while (< position length)
      (let* ((next (slackit-code--next-face-change source position length))
             (face (or (get-text-property position 'face source)
                       (get-text-property position 'font-lock-face source))))
        (when face
          (add-text-properties position next (list 'face face) result))
        (setq position next)))
    result))

(defun slackit-code--fontify-native (mode text)
  "Return TEXT fontified with MODE, or nil on a contained failure."
  (condition-case nil
      (let ((buffer (slackit-code--fontification-buffer mode)))
        (with-current-buffer buffer
          (unwind-protect
              (progn
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert text " "))
                (unless (eq major-mode mode)
                  (let ((inhibit-message t))
                    (delay-mode-hooks (funcall mode))))
                (unless (bound-and-true-p font-lock-mode)
                  (font-lock-mode 1))
                (when (fboundp 'font-lock-flush)
                  (font-lock-flush (point-min) (point-max)))
                (when (fboundp 'font-lock-ensure)
                  (font-lock-ensure (point-min) (point-max)))
                (slackit-code--sanitize-font-lock
                 (buffer-substring (point-min) (1- (point-max)))))
            (let ((inhibit-read-only t))
              (erase-buffer)))))
    (error nil)))

(defun slackit-code--cancel-cache (cache)
  "Clear and retire account-owned code CACHE."
  (when (slackit-code-cache-p cache)
    (when-let* ((entries (slackit-code-cache-entries cache)))
      (clrhash entries))
    (remhash (slackit-code-cache-app cache) slackit-code--app-caches)
    (setf (slackit-code-cache-handle cache) nil)))

(defun slackit-code--app-cache (app)
  "Return or create APP's lifecycle-owned code cache."
  (or (gethash app slackit-code--app-caches)
      (let* ((cache
              (slackit-code-cache-create
               :app app :entries (make-hash-table :test #'equal)))
             (handle
              (appkit-register-handle
               app 'slackit-code-cache cache #'slackit-code--cancel-cache)))
        (setf (slackit-code-cache-handle cache) handle)
        (puthash app cache slackit-code--app-caches)
        cache)))

(defun slackit-code--cache-key (mode language text)
  "Return an opaque presentation cache key for MODE LANGUAGE and TEXT."
  (list mode language
        (secure-hash 'sha256 text)
        slackit-code-fontify-blocks))

(defun slackit-code--cache-get (cache key)
  "Return a detached cached value from CACHE for KEY."
  (when-let* ((value (gethash key (slackit-code-cache-entries cache))))
    (copy-sequence value)))

(defun slackit-code--cache-put (cache key value)
  "Store detached VALUE under KEY in bounded account CACHE."
  (let ((entries (slackit-code-cache-entries cache)))
    (puthash key (copy-sequence value) entries)
    (when (> (hash-table-count entries) (max 1 slackit-code-cache-limit))
      (clrhash entries)
      (puthash key (copy-sequence value) entries)))
  value)

(defun slackit-code-inline-string (text)
  "Return display-only inline code TEXT."
  (let ((payload (copy-sequence (or text ""))))
    (when (> (length payload) 0)
      (add-face-text-property 0 (length payload) 'slackit-inline-code
                              'append payload)
      (add-text-properties 0 (length payload)
                           '(slackit-code-kind inline)
                           payload))
    payload))

(defun slackit-code-block-string (app text &optional language)
  "Return display-only code block TEXT for APP and optional Slack LANGUAGE."
  (let* ((source (or text ""))
         (normalized-language (slackit-code--normalize-language language))
         (mode
          (and slackit-code-fontify-blocks
               (slackit-code-mode-for-language normalized-language)))
         (cache (and mode (appkit-app-live-p app)
                     (slackit-code--app-cache app)))
         (key (and cache
                   (slackit-code--cache-key
                    mode normalized-language source)))
         (cached (and cache (slackit-code--cache-get cache key)))
         (native (or cached
                     (and mode
                          (slackit-code--fontify-native mode source))))
         (effective-mode (and native mode))
         (payload (copy-sequence (or native source))))
    (when (and cache key native (null cached))
      (slackit-code--cache-put cache key native))
    (when (> (length payload) 0)
      (add-face-text-property 0 (length payload) 'slackit-code-block
                              'append payload)
      (add-text-properties
       0 (length payload)
       (append (list 'slackit-code-kind 'block)
               (and normalized-language
                    (list 'slackit-code-language normalized-language))
               (and effective-mode
                    (list 'slackit-code-mode effective-mode)))
       payload))
    payload))

(defun slackit-code-clear-cache (&optional app)
  "Clear fontified code entries for APP, or for every live code cache."
  (if app
      (when-let* ((cache (gethash app slackit-code--app-caches)))
        (clrhash (slackit-code-cache-entries cache)))
    (maphash
     (lambda (_app cache)
       (clrhash (slackit-code-cache-entries cache)))
     slackit-code--app-caches)))

(provide 'slackit-code)

;;; slackit-code.el ends here
