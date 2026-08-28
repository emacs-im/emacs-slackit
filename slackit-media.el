;;; slackit-media.el --- Slack media cards and public posters -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Adapt normalized Slack Block Kit media and file metadata to Appkit cards.
;; Only credential-free public Block Kit image sources are acquired.  Slack
;; file URLs and thumbnails remain browser-link metadata and never enter the
;; media transfer runtime.

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'appkit-core)
(require 'appkit-chat-ins)
(require 'appkit-media)
(require 'appkit-ui)
(require 'slackit-customize)
(require 'slackit-normalize)
(require 'slackit-runtime)

(defcustom slackit-media-cache-directory
  (locate-user-emacs-file "slackit/media/")
  "Directory containing account-isolated public Slack media posters."
  :type 'directory
  :group 'slackit)

(defcustom slackit-media-retry-delay 60
  "Seconds before a failed public Slack poster may be requested again."
  :type 'number
  :group 'slackit)

(defcustom slackit-media-failure-limit 128
  "Maximum number of public poster failures retained in memory."
  :type 'integer
  :group 'slackit)

(cl-defstruct (slackit-media-fetch
               (:constructor slackit-media-fetch-create))
  app
  generation
  resource-key
  transfer
  handle)

(defvar slackit-media--image-cache (make-hash-table :test #'equal)
  "Decoded poster records keyed by opaque media resource identity.")

(defvar slackit-media--fetches (make-hash-table :test #'equal)
  "Current account-owned poster fetches keyed by resource identity.")

(defvar slackit-media--failures (make-hash-table :test #'equal)
  "Bounded poster failure timestamps keyed by resource identity.")

(defvar slackit-media--prepared-cache-directory nil
  "Expanded private media cache directory prepared in this Emacs session.")

(defun slackit-media--non-empty-string (value)
  "Return trimmed VALUE when it is a non-empty string, otherwise nil."
  (and (stringp value)
       (let ((trimmed (string-trim value)))
         (and (not (string-empty-p trimmed)) trimmed))))

(defun slackit-media--account-scope (app)
  "Return opaque stable account scope for APP."
  (secure-hash 'sha256 (prin1-to-string (appkit-app-id app))))

(defun slackit-media--resource-key (app kind identity)
  "Return an opaque account-scoped resource key for KIND and IDENTITY."
  (list :slackit-media
        (slackit-media--account-scope app)
        kind
        (secure-hash 'sha256 (prin1-to-string identity))))

(defun slackit-media--private-address-p (host)
  "Return non-nil when HOST is a syntactically private network address."
  (let ((host (downcase (or host ""))))
    (or (member host '("localhost" "localhost.localdomain" "::1" "[::1]"))
        (string-suffix-p ".local" host)
        (string-suffix-p ".internal" host)
        (string-match-p
         (concat "\\`\\(?:0\\|10\\|127\\|169\\.254\\|192\\.168\\|"
                 "172\\.\\(?:1[6-9]\\|2[0-9]\\|3[01]\\)\\|"
                 "100\\.\\(?:6[4-9]\\|[78][0-9]\\|9[0-9]\\|"
                 "1[01][0-9]\\|12[0-7]\\)\\)\\.")
         host)
        (string-match-p "\\`\\(?:fc\\|fd\\|fe[89ab]\\)" host))))

(defun slackit-media--credential-query-p (url)
  "Return non-nil when URL carries a credential-like query parameter."
  (let ((case-fold-search t))
    (string-match-p
     (concat "[?&]\\(?:access[_-]?token\\|auth\\|authorization\\|"
             "bearer\\|credential\\|key\\|secret\\|sig\\|signature\\|"
             "token\\|x-amz-[^=&]*\\|x-goog-[^=&]*\\)=")
     url)))

(defun slackit-media--slack-private-media-url-p (parsed)
  "Return non-nil when PARSED names a private Slack media endpoint."
  (let ((host (downcase (or (url-host parsed) "")))
        (path (downcase (or (url-filename parsed) ""))))
    (or (string-equal host "files.slack.com")
        (string-suffix-p ".files.slack.com" host)
        (string-equal host "slack-files.com")
        (string-suffix-p ".slack-files.com" host)
        (string-match-p "/files-\\(?:pri\\|tmb\\)/" path))))

(defun slackit-media--public-source-p (url)
  "Return non-nil when URL is a credential-free public HTTPS image source."
  (and (slackit-media--non-empty-string url)
       (not (string-match-p "[[:space:]\"\\\\]" url))
       (not (slackit-media--credential-query-p url))
       (condition-case nil
           (let* ((parsed (url-generic-parse-url url))
                  (host (url-host parsed)))
             (and (string-equal (url-type parsed) "https")
                  (stringp host)
                  (not (string-empty-p host))
                  (null (url-user parsed))
                  (null (url-password parsed))
                  (not (slackit-media--private-address-p host))
                  (not (slackit-media--slack-private-media-url-p parsed))))
         (error nil))))

(defun slackit-media--browser-page-url-p (url)
  "Return non-nil when URL is a safe exact HTTPS browser page action."
  (and (slackit-media--non-empty-string url)
       (not (string-match-p "[[:space:]\"\\\\]" url))
       (not (slackit-media--credential-query-p url))
       (condition-case nil
           (let ((parsed (url-generic-parse-url url)))
             (and (string-equal (url-type parsed) "https")
                  (slackit-media--non-empty-string (url-host parsed))
                  (null (url-user parsed))
                  (null (url-password parsed))))
         (error nil))))

(defun slackit-media--text-object-string (value)
  "Return display text from normalized Slack text object VALUE."
  (or (slackit-media--non-empty-string value)
      (and (listp value)
           (slackit-media--non-empty-string
            (slackit-normalize-get value 'text)))))

(defun slackit-media--block-title (block fallback)
  "Return BLOCK title, using FALLBACK when its title is absent."
  (or (slackit-media--text-object-string
       (slackit-normalize-get block 'title))
      (slackit-media--non-empty-string
       (slackit-normalize-get block 'alt_text))
      fallback))

(defun slackit-media--block-item (app block path)
  "Return one supported media item for APP BLOCK at PATH, or nil."
  (pcase (slackit-normalize-get block 'type)
    ("image"
     (let* ((source (slackit-normalize-get block 'image_url))
            (public-source
             (and (slackit-media--public-source-p source) source)))
       (list :class 'block
             :kind 'photo
             :resource-key
             (slackit-media--resource-key
              app 'image
              (or source
                  (list path
                        (slackit-normalize-get block 'alt_text))))
             :source public-source
             :page-url public-source
             :title (slackit-media--block-title block "Image")
             :meta (slackit-media--non-empty-string
                    (slackit-normalize-get block 'alt_text)))))
    ("video"
     (let* ((source (slackit-normalize-get block 'thumbnail_url))
            (page-candidate (slackit-normalize-get block 'title_url)))
       (list :class 'block
             :kind 'video
             :resource-key
             (slackit-media--resource-key
              app 'video
              (or source
                  (list path
                        (slackit-media--block-title block "Video"))))
             :source (and (slackit-media--public-source-p source) source)
             :page-url
             (and (slackit-media--browser-page-url-p page-candidate)
                  page-candidate)
             :title (slackit-media--block-title block "Video")
             :meta
             (or (slackit-media--text-object-string
                  (slackit-normalize-get block 'description))
                 (slackit-media--non-empty-string
                  (slackit-normalize-get block 'provider_name))
                 (slackit-media--non-empty-string
                  (slackit-normalize-get block 'author_name))))))))

(defun slackit-media--collect-block-items (app node path)
  "Collect supported media items recursively from APP NODE at PATH."
  (when (listp node)
    (if-let* ((item (slackit-media--block-item app node path)))
        (list item)
      (let* ((accessory (slackit-normalize-get node 'accessory))
             (elements (slackit-normalize-get node 'elements))
             (sequence
              (cond
               ((vectorp elements) (append elements nil))
               ((listp elements) elements))))
        (append
         (and accessory
              (slackit-media--collect-block-items
               app accessory (append path '(accessory))))
         (cl-loop
          for element in sequence
          for index from 0
          append
          (slackit-media--collect-block-items
           app element (append path (list 'elements index)))))))))

(defun slackit-media--block-items (app message)
  "Return deterministic image and video item plists from MESSAGE for APP."
  (let* ((blocks (slackit-normalize-get message 'blocks))
         (sequence
          (cond
           ((vectorp blocks) (append blocks nil))
           ((listp blocks) blocks))))
    (cl-loop
     for block in sequence
     for index from 0
     append
     (slackit-media--collect-block-items
      app block (list 'blocks index)))))

(defun slackit-media--file-kind (file)
  "Return typed Appkit card kind for normalized Slack FILE metadata."
  (let ((mime (downcase (or (slackit-normalize-get file 'mimetype) "")))
        (type (downcase (or (slackit-normalize-get file 'filetype) ""))))
    (cond
     ((or (string-prefix-p "image/" mime)
          (member type '("png" "jpg" "jpeg" "gif" "webp" "bmp" "svg")))
      'photo)
     ((or (string-prefix-p "video/" mime)
          (member type '("mp4" "mov" "mkv" "webm" "avi" "m4v")))
      'video)
     ((or (string-prefix-p "audio/" mime)
          (member type '("mp3" "m4a" "wav" "ogg" "flac")))
      'audio)
     (t 'document))))

(defun slackit-media--file-title (file)
  "Return a useful title for normalized Slack FILE metadata."
  (or (slackit-media--non-empty-string
       (slackit-normalize-get file 'title))
      (slackit-media--non-empty-string
       (slackit-normalize-get file 'name))
      "Slack file"))

(defun slackit-media--file-page-url (file)
  "Return FILE's exact non-capability browser permalink, or nil."
  (let ((url (slackit-normalize-get file 'permalink)))
    (and (slackit-media--browser-page-url-p url) url)))

(defun slackit-media--file-meta (file)
  "Return safe compact metadata strings for normalized Slack FILE."
  (let ((type (or (slackit-media--non-empty-string
                   (slackit-normalize-get file 'pretty_type))
                  (slackit-media--non-empty-string
                   (slackit-normalize-get file 'mimetype))
                  (slackit-media--non-empty-string
                   (slackit-normalize-get file 'filetype))))
        (size (slackit-normalize-get file 'size)))
    (delq nil
          (list type
                (and (numberp size)
                     (file-size-human-readable (max 0 size)))))))

(defun slackit-media--file-items (app message)
  "Return deterministic rich file metadata item plists from MESSAGE for APP."
  (cl-loop
   for file in (or (slackit-normalize-get message 'files) nil)
   for index from 0
   when (listp file)
   collect
   (let* ((id (slackit-normalize-get file 'id))
          (identity
           (if (slackit-media--non-empty-string id)
               (list 'id id)
             (list 'index index
                   (slackit-normalize-get file 'name)
                   (slackit-normalize-get file 'mimetype)
                   (slackit-normalize-get file 'size)))))
     (list :class 'file
           :kind (slackit-media--file-kind file)
           :resource-key (slackit-media--resource-key app 'file identity)
           :title (slackit-media--file-title file)
           :meta (slackit-media--file-meta file)
           :page-url (slackit-media--file-page-url file)))))

(defun slackit-media--message-items (app message)
  "Return deterministic media card item plists for APP and MESSAGE."
  (when (and (appkit-app-p app) (listp message))
    (append (slackit-media--block-items app message)
            (slackit-media--file-items app message))))

(defun slackit-media-message-media-only-p (message)
  "Return non-nil when MESSAGE blocks are entirely supported media blocks."
  (let ((blocks (slackit-normalize-get message 'blocks)))
    (and (listp blocks)
         blocks
         (cl-every
          (lambda (block)
            (member (slackit-normalize-get block 'type)
                    '("image" "video")))
          blocks))))

(defun slackit-media-message-resource-keys (app message)
  "Return stable opaque media resource keys for APP's normalized MESSAGE."
  (delete-dups
   (mapcar (lambda (item) (plist-get item :resource-key))
           (slackit-media--message-items app message))))

(defun slackit-media--prepare-cache-directory ()
  "Prepare and return the private public-poster cache directory."
  (let ((directory (expand-file-name slackit-media-cache-directory)))
    (unless (and (equal directory slackit-media--prepared-cache-directory)
                 (file-directory-p directory))
      (make-directory directory t)
      (unless (memq system-type '(ms-dos windows-nt cygwin))
        (set-file-modes directory #o700))
      (setq slackit-media--prepared-cache-directory directory))
    directory))

(defun slackit-media--cache-base (resource-key)
  "Return a private cache base path for opaque RESOURCE-KEY."
  (expand-file-name
   (secure-hash 'sha256 (prin1-to-string resource-key))
   slackit-media-cache-directory))

(defun slackit-media--cached-image (resource-key)
  "Return RESOURCE-KEY's decoded cached poster without starting acquisition."
  (let* ((cache-base (slackit-media--cache-base resource-key))
         (file (appkit-media-image-cache-existing-file cache-base))
         (record (gethash resource-key slackit-media--image-cache))
         (attributes (and file (file-attributes file)))
         (mtime (and attributes
                     (file-attribute-modification-time attributes))))
    (cond
     ((and record file
           (equal file (nth 0 record))
           (equal mtime (nth 1 record)))
      (let ((image (nth 2 record)))
        (and (not (eq image :invalid)) image)))
     (file
      (let ((image (appkit-media-preview-image-from-file file)))
        (puthash resource-key (list file mtime (or image :invalid))
                 slackit-media--image-cache)
        image))
     (t
      (remhash resource-key slackit-media--image-cache)
      nil))))

(defun slackit-media--owner-current-p (owner)
  "Return non-nil when poster fetch OWNER still owns publication."
  (let ((app (slackit-media-fetch-app owner))
        (key (slackit-media-fetch-resource-key owner)))
    (and (appkit-app-live-p app)
         (slackit-runtime-current-p
          app (slackit-media-fetch-generation owner))
         (eq owner (gethash key slackit-media--fetches)))))

(defun slackit-media--cancel-fetch (owner)
  "Cancel account-owned poster fetch OWNER without publishing."
  (let ((key (slackit-media-fetch-resource-key owner)))
    (when (eq owner (gethash key slackit-media--fetches))
      (remhash key slackit-media--fetches))
    (when-let* ((transfer (slackit-media-fetch-transfer owner)))
      (setf (slackit-media-fetch-transfer owner) nil)
      (when (appkit-media-transfer-p transfer)
        (appkit-media-cancel-transfer transfer)))))

(defun slackit-media--retire-fetch (owner)
  "Retire completed poster fetch OWNER and its Appkit handle."
  (let ((key (slackit-media-fetch-resource-key owner)))
    (when (eq owner (gethash key slackit-media--fetches))
      (remhash key slackit-media--fetches))
    (setf (slackit-media-fetch-transfer owner) nil)
    (when-let* ((handle (slackit-media-fetch-handle owner)))
      (when (appkit-handle-alive-p handle)
        (appkit-retire-handle handle)))))

(defun slackit-media--trim-failures ()
  "Keep the in-memory public poster failure table within its configured bound."
  (let ((limit (max 1 slackit-media-failure-limit)))
    (while (> (hash-table-count slackit-media--failures) limit)
      (let (oldest-key oldest-time)
        (maphash
         (lambda (key time)
           (when (or (null oldest-time) (< time oldest-time))
             (setq oldest-key key oldest-time time)))
         slackit-media--failures)
        (if oldest-key
            (remhash oldest-key slackit-media--failures)
          (clrhash slackit-media--failures))))))

(defun slackit-media--record-failure (resource-key)
  "Record bounded terminal failure state for RESOURCE-KEY."
  (puthash resource-key (float-time) slackit-media--failures)
  (slackit-media--trim-failures))

(defun slackit-media--fetch-success (owner file)
  "Settle poster fetch OWNER from private cache FILE."
  (let* ((current-p (slackit-media--owner-current-p owner))
         (app (slackit-media-fetch-app owner))
         (key (slackit-media-fetch-resource-key owner))
         (image (and current-p
                     (file-regular-p file)
                     (appkit-media-preview-image-from-file file))))
    (when (file-regular-p file)
      (unless (memq system-type '(ms-dos windows-nt cygwin))
        (set-file-modes file #o600)))
    (when current-p
      (remhash key slackit-media--image-cache)
      (if image
          (progn
            (remhash key slackit-media--failures)
            (let* ((attributes (file-attributes file))
                   (mtime (file-attribute-modification-time attributes)))
              (puthash key (list file mtime image)
                       slackit-media--image-cache)))
        (when (file-exists-p file)
          (ignore-errors (delete-file file)))
        (slackit-media--record-failure key)))
    (slackit-media--retire-fetch owner)
    (when current-p
      (slackit-runtime-publish-resource app key))))

(defun slackit-media--fetch-failure (owner _reason)
  "Settle failed poster fetch OWNER without exposing remote details."
  (let ((current-p (slackit-media--owner-current-p owner))
        (app (slackit-media-fetch-app owner))
        (key (slackit-media-fetch-resource-key owner)))
    (when current-p
      (slackit-media--record-failure key))
    (slackit-media--retire-fetch owner)
    (when current-p
      (slackit-runtime-publish-resource app key))))

(defun slackit-media--retry-due-p (resource-key)
  "Return non-nil when RESOURCE-KEY has no active failure backoff."
  (let ((failed-at (gethash resource-key slackit-media--failures)))
    (or (not (numberp failed-at))
        (>= (- (float-time) failed-at)
            (max 0 slackit-media-retry-delay)))))

(defun slackit-media--ensure-item (app item)
  "Start one deduplicated public poster acquisition for APP and ITEM."
  (let ((source (plist-get item :source))
        (key (plist-get item :resource-key)))
    (when (and source key
               (not (slackit-media--cached-image key))
               (not (gethash key slackit-media--fetches))
               (slackit-media--retry-due-p key))
      (slackit-media--prepare-cache-directory)
      (let ((old-file (appkit-media-image-cache-existing-file
                       (slackit-media--cache-base key))))
        (when old-file
          (ignore-errors (delete-file old-file))
          (remhash key slackit-media--image-cache)))
      (let* ((owner (slackit-media-fetch-create
                     :app app
                     :generation (slackit-runtime-generation app)
                     :resource-key key))
             (handle (appkit-register-handle
                      app 'slackit-media owner
                      #'slackit-media--cancel-fetch)))
        (setf (slackit-media-fetch-handle owner) handle)
        (puthash key owner slackit-media--fetches)
        (slackit-runtime-publish-resource app key)
        (condition-case nil
            (let ((transfer
                   (appkit-media-cache-image-resource-async
                    (appkit-media-resource-create
                     :url source :name "slack-poster.img"
                     :mime-type "image/*")
                    (slackit-media--cache-base key)
                    (apply-partially #'slackit-media--fetch-success owner)
                    (apply-partially #'slackit-media--fetch-failure owner))))
              (if (slackit-media--owner-current-p owner)
                  (setf (slackit-media-fetch-transfer owner) transfer)
                (when (appkit-media-transfer-p transfer)
                  (appkit-media-cancel-transfer transfer))))
          (error (slackit-media--fetch-failure owner nil)))
        owner))))

(defun slackit-media-ensure-message (app message)
  "Start deduplicated public poster acquisition for APP's normalized MESSAGE.

This function only acquires resources.  Rendering remains a separate,
deterministic operation in `slackit-media-insert-message-cards'."
  (when (and (appkit-app-live-p app)
             (appkit-media-inline-image-rendering-available-p))
    (dolist (item (slackit-media--block-items app message))
      (slackit-media--ensure-item app item)))
  nil)

(defun slackit-media--browser-action (url)
  "Return a zero-argument exact browser action for URL, or nil."
  (and url (apply-partially #'browse-url url)))

(defun slackit-media--safe-context-payload (item)
  "Return non-capability metadata payload for media card ITEM."
  (list :resource-key (plist-get item :resource-key)
        :kind (plist-get item :kind)
        :title (plist-get item :title)))

(defun slackit-media--item-context (item)
  "Return backend-neutral card context for ITEM."
  (let ((action (slackit-media--browser-action
                 (plist-get item :page-url))))
    (appkit-media-card-context-create
     :payload (slackit-media--safe-context-payload item)
     :kind (plist-get item :kind)
     :title (plist-get item :title)
     :open-action action)))

(defun slackit-media--insert-poster (item context prefix-state)
  "Insert ITEM's cached poster or fallback using CONTEXT and PREFIX-STATE."
  (let* ((key (plist-get item :resource-key))
         (image (and key (slackit-media--cached-image key)))
         (video-p (eq (plist-get item :kind) 'video))
         (display-image
          (if (and image video-p)
              (or (appkit-media-video-preview-display-image image 'slackit)
                  image)
            image))
         (action (plist-get context :open-action))
         (start (point)))
    (cond
     (display-image
      (condition-case nil
          (appkit-media-insert-image-slices
           display-image action nil
           (if video-p "[video]" "[image]")
           (if action "Open in browser" "Media preview"))
        (error (insert "[preview unavailable]")))
      (insert "\n"))
     ((gethash key slackit-media--fetches)
      (insert "[loading preview]\n"))
     (t
      (insert "[preview unavailable]\n")))
    (appkit-ui-apply-line-prefix start (point) prefix-state)
    (unless display-image
      (appkit-ui-append-face start (point) 'shadow))))

(defun slackit-media--insert-item-card (item prefix properties)
  "Insert one deterministic media ITEM card using PREFIX and PROPERTIES."
  (let* ((context (slackit-media--item-context item))
         (block-p (eq (plist-get item :class) 'block))
         (meta (plist-get item :meta)))
    (appkit-chat-ins-insert-media-card
     :kind (plist-get item :kind)
     :title (plist-get item :title)
     :meta meta
     :prefix prefix
     :title-face 'bold
     :meta-face 'shadow
     :properties properties
     :context context
     :open-help-echo (if (plist-get context :open-action)
                         "Open exact Slack media page in browser"
                       "Media page unavailable")
     :body-inserter
     (and block-p
          (lambda (prefix-state)
            (slackit-media--insert-poster item context prefix-state))))))

(defun slackit-media-insert-message-cards (app message &optional prefix properties)
  "Insert deterministic rich media cards for APP's normalized MESSAGE.

PREFIX and PROPERTIES are forwarded to Appkit's shared card inserter.  This
function only observes memory and disk cache state; it never starts transfer."
  (dolist (item (slackit-media--message-items app message))
    (slackit-media--insert-item-card item prefix properties)))

(defun slackit-media-clear-memory-cache ()
  "Clear decoded public poster images and bounded failure state."
  (clrhash slackit-media--image-cache)
  (clrhash slackit-media--failures)
  (appkit-media-clear-video-decoration-cache 'slackit))

(provide 'slackit-media)

;;; slackit-media.el ends here
