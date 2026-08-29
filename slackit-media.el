;;; slackit-media.el --- Slack media cards and image previews -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Adapt normalized Slack Block Kit media and file metadata to Appkit cards.
;; Public sources remain credential-free.  Exact files.slack.com image
;; thumbnails use only their owning account's bearer token and d cookie through
;; a no-redirect, account-owned transfer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'plz)
(require 'appkit-core)
(require 'appkit-chat-ins)
(require 'appkit-media)
(require 'appkit-ui)
(require 'slackit-customize)
(require 'slackit-normalize)
(require 'slackit-runtime)

(defcustom slackit-media-cache-directory
  (locate-user-emacs-file "slackit/media/")
  "Directory containing account-isolated cached Slack media images."
  :type 'directory
  :group 'slackit)

(defcustom slackit-media-retry-delay 60
  "Seconds before a failed Slack media image may be requested again."
  :type 'number
  :group 'slackit)

(defcustom slackit-media-failure-limit 128
  "Maximum number of media image failures retained in memory."
  :type 'integer
  :group 'slackit)

(defcustom slackit-media-audio-player-command
  (cond
   ((executable-find "mpv")
    '("mpv" "--no-video" "--force-window=no"
      "--keep-open=no" "--idle=no"))
   ((executable-find "ffplay") '("ffplay" "-nodisp" "-autoexit"))
   ((executable-find "vlc") '("vlc" "--play-and-exit"))
   (t nil))
  "Command used to play downloaded Slack audio files."
  :type '(choice
          (const :tag "No audio player" nil)
          string
          (repeat string))
  :group 'slackit)

(cl-defstruct (slackit-media-audio
               (:constructor slackit-media-audio-create))
  app
  generation
  resource-key
  process
  handle
  status)

(cl-defstruct (slackit-media-fetch
               (:constructor slackit-media-fetch-create))
  app
  generation
  resource-key
  purpose
  kind
  transfer
  cache-file
  handle
  success-function)

(cl-defstruct (slackit-media-open-spec
               (:constructor slackit-media-open-spec-create))
  app
  generation
  kind
  source
  name
  mime-type
  size
  private-source-p)

(defvar slackit-media--image-cache (make-hash-table :test #'equal)
  "Decoded poster records keyed by opaque media resource identity.")

(defvar slackit-media--fetches (make-hash-table :test #'equal)
  "Current account-owned media fetches keyed by opaque resource identity.")

(defvar slackit-media--failures (make-hash-table :test #'equal)
  "Bounded media failure timestamps keyed by opaque resource identity.")

(defvar slackit-media--open-specs
  (make-hash-table :test #'equal :weakness 'key)
  "Opaque content keys to account-owned media specifications.")

(defvar slackit-media--audio-states (make-hash-table :test #'equal)
  "Account-owned audio playback states keyed by content resource identity.")

(defvar slackit-media--prepared-cache-directory nil
  "Expanded private media cache directory prepared in this Emacs session.")

(defun slackit-media--non-empty-string (value)
  "Return trimmed VALUE when it is a non-empty string, otherwise nil."
  (and (stringp value)
       (let ((trimmed (string-trim value)))
         (and (not (string-empty-p trimmed)) trimmed))))

(defun slackit-media--register-content-spec
    (app identity kind source name mime-type size)
  "Register APP media content and return its opaque key.

IDENTITY is secret-free.  SOURCE must be an accepted private Slack or public
media source."
  (let ((private-source-p (slackit-media--private-source-p source)))
    (when (or private-source-p (slackit-media--public-source-p source))
      (let ((key (slackit-media--resource-key app 'content identity)))
        (puthash
         key
         (slackit-media-open-spec-create
          :app app
          :generation (slackit-runtime-generation app)
          :kind kind
          :source source
          :name name
          :mime-type mime-type
          :size size
          :private-source-p private-source-p)
         slackit-media--open-specs)
        key))))

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

(defun slackit-media--private-source-p (url)
  "Return non-nil when URL is an exact authenticated Slack file image source."
  (and (slackit-media--non-empty-string url)
       (not (string-match-p "[[:space:]\"\\\\]" url))
       (condition-case nil
           (let* ((parsed (url-generic-parse-url url))
                  (host (downcase (or (url-host parsed) "")))
                  (path (or (url-filename parsed) ""))
                  (port (url-port parsed)))
             (and (string-equal (url-type parsed) "https")
                  (string-equal host "files.slack.com")
                  (or (null port) (= port 443))
                  (null (url-user parsed))
                  (null (url-password parsed))
                  (string-match-p
                   "\\`/files-\\(?:tmb\\|pri\\)/[^/]+/.+\\'" path)))
         (error nil))))

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
            (title (slackit-media--block-title block "Image"))
            (identity
             (list 'block path
                   (slackit-normalize-get block 'block_id)
                   (slackit-normalize-get block 'alt_text)))
            (public-source
             (and (slackit-media--public-source-p source) source))
            (content-key
             (slackit-media--register-content-spec
              app identity 'photo public-source title nil nil)))
       (list :class 'block
             :kind 'photo
             :resource-key
             (slackit-media--resource-key app 'preview identity)
             :content-resource-key content-key
             :source public-source
             :title title
             :meta (slackit-media--non-empty-string
                    (slackit-normalize-get block 'alt_text)))))
    ("video"
     (let* ((preview-source
             (slackit-normalize-get block 'thumbnail_url))
            (content-source
             (slackit-normalize-get block 'video_url))
            (title (slackit-media--block-title block "Video"))
            (identity
             (list 'block path
                   (slackit-normalize-get block 'block_id)
                   title))
            (content-key
             (slackit-media--register-content-spec
              app identity 'video content-source title "video/*" nil)))
       (list :class 'block
             :kind 'video
             :resource-key
             (slackit-media--resource-key app 'preview identity)
             :content-resource-key content-key
             :source
             (and (slackit-media--public-source-p preview-source)
                  preview-source)
             :title title
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


(defconst slackit-media--file-preview-fields
  '(thumb_1024 thumb_960 thumb_720 thumb_480 thumb_360
    thumb_160 thumb_80 thumb_64)
  "Slack file image fields in preferred preview order.")

(defun slackit-media--file-preview-source (file kind)
  "Return FILE preview source for media KIND, or nil."
  (when (memq kind '(photo video))
    (or
     (and (eq kind 'video)
          (slackit-media--non-empty-string
           (slackit-normalize-get file 'thumb_video)))
     (cl-loop for field in slackit-media--file-preview-fields
              for value = (slackit-normalize-get file field)
              when (slackit-media--non-empty-string value)
              return value)
     (and (eq kind 'photo)
          (slackit-media--non-empty-string
           (slackit-normalize-get file 'url_private))))))

(defun slackit-media--file-open-source (file _kind)
  "Return FILE's original content source."
  (or (slackit-media--non-empty-string
       (slackit-normalize-get file 'url_private_download))
      (slackit-media--non-empty-string
       (slackit-normalize-get file 'url_private))))

(defun slackit-media--file-meta (file)
  "Return safe compact metadata strings for normalized Slack FILE."
  (let ((type (or (slackit-media--non-empty-string
                   (slackit-normalize-get file 'pretty_type))
                  (slackit-media--non-empty-string
                   (slackit-normalize-get file 'mimetype))
                  (slackit-media--non-empty-string
                   (slackit-normalize-get file 'filetype))))
        (size (slackit-normalize-get file 'size))
        (duration-ms (slackit-normalize-get file 'duration_ms)))
    (delq nil
          (list type
                (and (numberp size)
                     (file-size-human-readable (max 0 size)))
                (and (numberp duration-ms)
                     (appkit-chat-ins-format-duration
                      (/ (max 0 duration-ms) 1000.0)))))))

(defun slackit-media--file-items (app message)
  "Return deterministic rich file metadata item plists from MESSAGE for APP."
  (cl-loop
   for file in (or (slackit-normalize-get message 'files) nil)
   for index from 0
   when (listp file)
   collect
   (let* ((id (slackit-normalize-get file 'id))
          (kind (slackit-media--file-kind file))
          (source (slackit-media--file-preview-source file kind))
          (content-source (slackit-media--file-open-source file kind))
          (name (slackit-normalize-get file 'name))
          (mime-type (slackit-normalize-get file 'mimetype))
          (size (slackit-normalize-get file 'size))
          (identity
           (if (slackit-media--non-empty-string id)
               (list 'id id)
             (list 'index index name mime-type size)))
          (private-source-p (slackit-media--private-source-p source))
          (content-key
           (slackit-media--register-content-spec
            app (list 'file identity) kind content-source
            name mime-type size)))
     (list :class 'file
           :kind kind
           :resource-key (slackit-media--resource-key app 'preview identity)
           :content-resource-key content-key
           :source (and (or private-source-p
                            (slackit-media--public-source-p source))
                        source)
           :private-source-p private-source-p
           :cache-name name
           :title (slackit-media--file-title file)
           :meta (slackit-media--file-meta file)
           :duration-ms (slackit-normalize-get file 'duration_ms)))))

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
   (delq
    nil
    (cl-loop
     for item in (slackit-media--message-items app message)
     append
     (list (plist-get item :resource-key)
           (plist-get item :content-resource-key))))))

(defun slackit-media--prepare-cache-directory ()
  "Prepare and return the private media cache directory."
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

(defun slackit-media-resource-key (app kind identity)
  "Return an opaque account-scoped media key for APP, KIND, and IDENTITY."
  (slackit-media--resource-key app kind identity))

(defun slackit-media-cached-image (resource-key)
  "Return RESOURCE-KEY's decoded image without starting acquisition."
  (slackit-media--cached-image resource-key))

(defun slackit-media--cached-file (resource-key)
  "Return RESOURCE-KEY's existing local cache file, or nil."
  (and resource-key
       (appkit-media-image-cache-existing-file
        (slackit-media--cache-base resource-key))))

(defun slackit-media--open-local-image-file (file)
  "Open local image FILE through Appkit's browser-free media runtime."
  (unless (appkit-media-file-present-p file)
    (user-error "slackit: cached image is unavailable"))
  (appkit-media-open-resource
   (appkit-media-resource-create :file file)
   :kind 'image
   :client-label "slackit"))

(defun slackit-media--open-cached-image (resource-key)
  "Open RESOURCE-KEY's cached image locally inside Emacs."
  (slackit-media--open-local-image-file
   (slackit-media--cached-file resource-key)))

(defun slackit-media--safe-extension (name)
  "Return NAME's safe lowercase extension, or nil."
  (when (stringp name)
    (let ((extension
           (downcase
            (or (file-name-extension
                 (car (split-string name "[?#]")))
                ""))))
      (and (string-match-p "\\`[[:alnum:]]\\{1,8\\}\\'" extension)
           extension))))

(defun slackit-media--source-extension (source &optional name)
  "Return a safe image extension inferred from NAME or SOURCE."
  (or (slackit-media--safe-extension name)
      (condition-case nil
          (let* ((parsed (url-generic-parse-url source))
                 (path (or (url-filename parsed) "")))
            (slackit-media--safe-extension path))
        (error nil))
      "img"))

(defun slackit-media--private-cache-file (resource-key source &optional name)
  "Return private cache filename for RESOURCE-KEY, SOURCE, and optional NAME."
  (concat (slackit-media--cache-base resource-key)
          "."
          (slackit-media--source-extension source name)))

(defun slackit-media--existing-content-file
    (resource-key source &optional name)
  "Return existing content file for RESOURCE-KEY, SOURCE, and optional NAME."
  (let ((file (slackit-media--private-cache-file resource-key source name)))
    (and (file-regular-p file) file)))

(defun slackit-media--html-file-p (file)
  "Return non-nil when FILE begins with an HTML document."
  (when (file-regular-p file)
    (let ((prefix
           (with-temp-buffer
             (set-buffer-multibyte nil)
             (insert-file-contents-literally file nil 0 2048)
             (buffer-string))))
      (string-match-p
       "<[[:space:]]*\\(?:!doctype[[:space:]]+html\\|html\\|head\\|body\\)"
       (downcase (decode-coding-string prefix 'utf-8 t))))))

(defun slackit-media--content-file-valid-p (kind file)
  "Return non-nil when local FILE is valid media content of KIND."
  (and (file-regular-p file)
       (> (file-attribute-size (file-attributes file)) 0)
       (not (slackit-media--html-file-p file))
       (if (eq kind 'photo)
           (appkit-media-preview-image-from-file file)
         t)))

(defun slackit-media--private-headers (app kind purpose)
  "Return account-local browser headers for APP media KIND and PURPOSE."
  (let* ((credential (slackit-runtime-credential app))
         (token (and credential (slackit-credential-token credential)))
         (d-cookie (slackit-runtime-credential-cookie-value app "d"))
         (accept
          (if (eq purpose 'preview)
              "image/avif,image/webp,image/apng,image/svg+xml,image/*,*/*;q=0.8"
            (pcase kind
              ('photo "image/*,application/octet-stream;q=0.8,*/*;q=0.1")
              ('video "video/*,application/octet-stream;q=0.8,*/*;q=0.1")
              ('audio "audio/*,application/octet-stream;q=0.8,*/*;q=0.1")
              (_ "application/octet-stream,*/*;q=0.8")))))
    (unless (and (stringp token) (not (string-empty-p token))
                 (stringp d-cookie) (not (string-empty-p d-cookie)))
      (error "slackit: authenticated media credential is unavailable"))
    `(("User-Agent" . ,slackit-browser-user-agent)
      ("Accept" . ,accept)
      ("Accept-Language" . "en-US,en;q=0.9")
      ("Referer" . "https://app.slack.com/")
      ("Sec-Fetch-Site" . "same-site")
      ("Sec-Fetch-Mode" . "no-cors")
      ("Sec-Fetch-Dest" . ,(if (eq purpose 'preview) "image" "empty"))
      ("Cache-Control" . "no-cache")
      ("Pragma" . "no-cache")
      ("Priority" . "i")
      ("Authorization" . ,(concat "Bearer " token))
      ("Cookie" . ,(concat "d=" d-cookie)))))

(defun slackit-media--cancel-transfer (transfer)
  "Cancel Appkit or plz media TRANSFER."
  (cond
   ((appkit-media-transfer-p transfer)
    (appkit-media-cancel-transfer transfer))
   ((and (processp transfer) (process-live-p transfer))
    (delete-process transfer))))

(defun slackit-media--private-transfer (owner item)
  "Start exact authenticated Slack file transfer for OWNER and ITEM."
  (let* ((app (slackit-media-fetch-app owner))
         (source (plist-get item :source))
         (key (slackit-media-fetch-resource-key owner))
         (cache-file
          (slackit-media--private-cache-file
           key source
           (and (eq 'content (slackit-media-fetch-purpose owner))
                (plist-get item :cache-name))))
         (plz-curl-default-args
          (remove "--location" plz-curl-default-args)))
    (unless (slackit-media--private-source-p source)
      (error "slackit: rejected authenticated media URL"))
    (when (file-exists-p cache-file)
      (delete-file cache-file))
    (setf (slackit-media-fetch-cache-file owner) cache-file)
    (plz 'get source
      :headers
      (slackit-media--private-headers
       app
       (slackit-media-fetch-kind owner)
       (slackit-media-fetch-purpose owner))
      :as `(file ,cache-file)
      :decode nil
      :timeout slackit-http-timeout
      :connect-timeout slackit-http-timeout
      :then (apply-partially #'slackit-media--fetch-success owner)
      :else (apply-partially #'slackit-media--fetch-failure owner))))

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
  "Cancel account-owned image fetch OWNER without publishing."
  (let ((key (slackit-media-fetch-resource-key owner)))
    (when (eq owner (gethash key slackit-media--fetches))
      (remhash key slackit-media--fetches))
    (setf (slackit-media-fetch-success-function owner) nil)
    (when-let* ((transfer (slackit-media-fetch-transfer owner)))
      (setf (slackit-media-fetch-transfer owner) nil)
      (slackit-media--cancel-transfer transfer))
    (when-let* ((file (slackit-media-fetch-cache-file owner)))
      (setf (slackit-media-fetch-cache-file owner) nil)
      (when (file-exists-p file)
        (ignore-errors (delete-file file))))))

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
  "Keep the in-memory image failure table within its configured bound."
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
  "Settle media fetch OWNER from completed local FILE."
  (let* ((current-p (slackit-media--owner-current-p owner))
         (app (slackit-media-fetch-app owner))
         (key (slackit-media-fetch-resource-key owner))
         (purpose (slackit-media-fetch-purpose owner))
         (kind (slackit-media-fetch-kind owner))
         (content-p (eq purpose 'content))
         (image
          (and current-p
               (file-regular-p file)
               (or (not content-p) (eq kind 'photo))
               (appkit-media-preview-image-from-file file)))
         (valid-p
          (and current-p
               (if content-p
                   (if (eq kind 'photo)
                       image
                     (slackit-media--content-file-valid-p kind file))
                 image)))
         (success-function
          (slackit-media-fetch-success-function owner)))
    (when (file-regular-p file)
      (unless (memq system-type '(ms-dos windows-nt cygwin))
        (set-file-modes file #o600)))
    (when current-p
      (remhash key slackit-media--image-cache)
      (if valid-p
          (progn
            (remhash key slackit-media--failures)
            (when image
              (let* ((attributes (file-attributes file))
                     (mtime (file-attribute-modification-time attributes)))
                (puthash key (list file mtime image)
                         slackit-media--image-cache))))
        (when (file-exists-p file)
          (ignore-errors (delete-file file)))
        (slackit-media--record-failure key)))
    (unless current-p
      (when (file-exists-p file)
        (ignore-errors (delete-file file))))
    (setf (slackit-media-fetch-cache-file owner) nil
          (slackit-media-fetch-success-function owner) nil)
    (slackit-media--retire-fetch owner)
    (when current-p
      (slackit-runtime-publish-resource app key))
    (when (and valid-p (functionp success-function))
      (condition-case nil
          (funcall success-function file)
        (error
         (message "slackit: failed to handle downloaded media"))))))

(defun slackit-media--fetch-failure (owner _reason)
  "Settle failed media fetch OWNER without exposing remote details."
  (let ((current-p (slackit-media--owner-current-p owner))
        (app (slackit-media-fetch-app owner))
        (key (slackit-media-fetch-resource-key owner)))
    (when-let* ((file (slackit-media-fetch-cache-file owner)))
      (setf (slackit-media-fetch-cache-file owner) nil)
      (when (file-exists-p file)
        (ignore-errors (delete-file file))))
    (setf (slackit-media-fetch-success-function owner) nil)
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

(defun slackit-media--compose-success-functions (first second file)
  "Call FIRST and SECOND success functions with local FILE."
  (funcall first file)
  (funcall second file))

(defun slackit-media--ensure-item (app item &optional success-function)
  "Start one deduplicated media acquisition for APP and ITEM.

SUCCESS-FUNCTION receives the validated local file after acquisition."
  (let* ((source (plist-get item :source))
         (key (plist-get item :resource-key))
         (purpose (or (plist-get item :purpose) 'preview))
         (kind (or (plist-get item :kind) 'photo))
         (content-p (eq purpose 'content))
         (cached-file
          (if content-p
              (slackit-media--existing-content-file
               key source (plist-get item :cache-name))
            (slackit-media--cached-file key)))
         (cached-image
          (and cached-file
               (or (not content-p) (eq kind 'photo))
               (slackit-media--cached-image key)))
         (cached-valid-p
          (and cached-file
               (if content-p
                   (if (eq kind 'photo)
                       cached-image
                     (slackit-media--content-file-valid-p kind cached-file))
                 cached-image)))
         (active (and key (gethash key slackit-media--fetches))))
    (cond
     (cached-valid-p
      (when (functionp success-function)
        (funcall success-function cached-file))
      nil)
     (active
      (when (functionp success-function)
        (let ((old (slackit-media-fetch-success-function active)))
          (setf (slackit-media-fetch-success-function active)
                (if (functionp old)
                    (apply-partially
                     #'slackit-media--compose-success-functions
                     old success-function)
                  success-function))))
      active)
     ((and source key (slackit-media--retry-due-p key))
      (slackit-media--prepare-cache-directory)
      (when cached-file
        (ignore-errors (delete-file cached-file))
        (remhash key slackit-media--image-cache))
      (let* ((owner
              (slackit-media-fetch-create
               :app app
               :generation (slackit-runtime-generation app)
               :resource-key key
               :purpose purpose
               :kind kind
               :success-function success-function))
             (handle
              (appkit-register-handle
               app 'slackit-media owner
               #'slackit-media--cancel-fetch)))
        (setf (slackit-media-fetch-handle owner) handle)
        (puthash key owner slackit-media--fetches)
        (slackit-runtime-publish-resource app key)
        (condition-case nil
            (let ((transfer
                   (cond
                    ((plist-get item :private-source-p)
                     (slackit-media--private-transfer owner item))
                    (content-p
                     (let ((target
                            (slackit-media--private-cache-file
                             key source (plist-get item :cache-name))))
                       (setf (slackit-media-fetch-cache-file owner) target)
                       (appkit-media-copy-or-download-resource-async
                        (appkit-media-resource-create
                         :url source
                         :name (plist-get item :cache-name)
                         :mime-type (plist-get item :mime-type))
                        target
                        (apply-partially
                         #'slackit-media--fetch-success owner)
                        (apply-partially
                         #'slackit-media--fetch-failure owner))))
                    (t
                     (appkit-media-cache-image-resource-async
                      (appkit-media-resource-create
                       :url source :name "slack-image.img"
                       :mime-type "image/*")
                      (slackit-media--cache-base key)
                      (apply-partially #'slackit-media--fetch-success owner)
                      (apply-partially #'slackit-media--fetch-failure owner))))))
              (if (slackit-media--owner-current-p owner)
                  (setf (slackit-media-fetch-transfer owner) transfer)
                (slackit-media--cancel-transfer transfer)))
          (error (slackit-media--fetch-failure owner nil)))
        owner)))))

(defun slackit-media--content-spec-current (content-key)
  "Return current account-owned media specification for CONTENT-KEY."
  (let* ((spec (gethash content-key slackit-media--open-specs))
         (app (and spec (slackit-media-open-spec-app spec))))
    (and spec
         (appkit-app-live-p app)
         (slackit-runtime-current-p
          app (slackit-media-open-spec-generation spec))
         spec)))

(defun slackit-media--content-cached-file (content-key spec)
  "Return validated CONTENT-KEY local file described by SPEC."
  (let* ((file
          (and spec
               (slackit-media--existing-content-file
                content-key
                (slackit-media-open-spec-source spec)
                (slackit-media-open-spec-name spec))))
         (kind (and spec (slackit-media-open-spec-kind spec))))
    (and file
         (if (eq kind 'photo)
             (slackit-media--cached-image content-key)
           (slackit-media--content-file-valid-p kind file))
         file)))

(defun slackit-media--content-state (content-key)
  "Return normalized transfer state for CONTENT-KEY."
  (let* ((spec (slackit-media--content-spec-current content-key))
         (file (and spec
                    (slackit-media--content-cached-file content-key spec)))
         (active (gethash content-key slackit-media--fetches)))
    (cond
     (active
      (list :status 'downloading
            :bytes-total
            (and spec (slackit-media-open-spec-size spec))))
     (file (list :status 'downloaded :path file))
     ((gethash content-key slackit-media--failures)
      (list :status 'error :error "request_failed"))
     (t (list :status 'not-downloaded)))))

(defun slackit-media--cancel-content (content-key)
  "Cancel CONTENT-KEY's current account-owned download."
  (let ((owner (gethash content-key slackit-media--fetches)))
    (unless owner
      (user-error "slackit: media is not downloading"))
    (if-let* ((handle (slackit-media-fetch-handle owner)))
        (appkit-cancel-handle handle)
      (slackit-media--cancel-fetch owner))
    (when-let* ((spec (slackit-media--content-spec-current content-key)))
      (slackit-runtime-publish-resource
       (slackit-media-open-spec-app spec) content-key))))

(defun slackit-media--download-content
    (content-key &optional success-function)
  "Ensure CONTENT-KEY is local, then call SUCCESS-FUNCTION with its file."
  (let* ((spec (slackit-media--content-spec-current content-key))
         (app (and spec (slackit-media-open-spec-app spec))))
    (unless spec
      (user-error "slackit: media content is unavailable"))
    (or
     (slackit-media--ensure-item
      app
      (list :source (slackit-media-open-spec-source spec)
            :resource-key content-key
            :purpose 'content
            :kind (slackit-media-open-spec-kind spec)
            :private-source-p
            (slackit-media-open-spec-private-source-p spec)
            :cache-name (slackit-media-open-spec-name spec)
            :mime-type (slackit-media-open-spec-mime-type spec))
      success-function)
     (slackit-media--content-cached-file content-key spec)
     (user-error "slackit: media content is temporarily unavailable"))))

(defun slackit-media--audio-finished (state process _event)
  "Settle audio playback STATE when PROCESS exits."
  (when (and (eq state
                 (gethash
                  (slackit-media-audio-resource-key state)
                  slackit-media--audio-states))
             (eq process (slackit-media-audio-process state))
             (not (process-live-p process)))
    (setf (slackit-media-audio-process state) nil
          (slackit-media-audio-status state) 'finished)
    (when-let* ((handle (slackit-media-audio-handle state)))
      (when (appkit-handle-alive-p handle)
        (appkit-retire-handle handle))
      (setf (slackit-media-audio-handle state) nil))
    (when (slackit-runtime-current-p
           (slackit-media-audio-app state)
           (slackit-media-audio-generation state))
      (slackit-runtime-publish-resource
       (slackit-media-audio-app state)
       (slackit-media-audio-resource-key state)))))

(defun slackit-media--cancel-audio (state)
  "Cancel account-owned audio playback STATE."
  (when-let* ((process (slackit-media-audio-process state)))
    (set-process-sentinel process nil)
    (when (process-live-p process)
      (delete-process process)))
  (setf (slackit-media-audio-process state) nil
        (slackit-media-audio-handle state) nil
        (slackit-media-audio-status state) 'idle)
  (when (eq state
            (gethash
             (slackit-media-audio-resource-key state)
             slackit-media--audio-states))
    (remhash
     (slackit-media-audio-resource-key state)
     slackit-media--audio-states)))

(defun slackit-media--audio-command-arguments ()
  "Return audio player arguments with bounded MPV lifecycle options."
  (let ((arguments
         (appkit-media-command-arguments
          slackit-media-audio-player-command)))
    (when (and arguments
               (equal "mpv"
                      (file-name-nondirectory (car arguments))))
      (dolist (option '("--keep-open=no" "--idle=no"))
        (unless (member option arguments)
          (setq arguments (append arguments (list option))))))
    arguments))


(defun slackit-media--start-audio-file (content-key file)
  "Start or stop local audio FILE playback for CONTENT-KEY."
  (let* ((spec (slackit-media--content-spec-current content-key))
         (app (and spec (slackit-media-open-spec-app spec)))
         (arguments (slackit-media--audio-command-arguments))
         (existing (gethash content-key slackit-media--audio-states)))
    (unless (and spec
                 arguments
                 (appkit-media-command-runnable-p
                  slackit-media-audio-player-command))
      (user-error
       "slackit: audio player is unavailable; customize `slackit-media-audio-player-command'"))
    (if (and existing
             (process-live-p (slackit-media-audio-process existing)))
        (progn
          (appkit-cancel-handle (slackit-media-audio-handle existing))
          (slackit-runtime-publish-resource app content-key)
          nil)
      (let ((state
             (slackit-media-audio-create
              :app app
              :generation (slackit-runtime-generation app)
              :resource-key content-key
              :status 'playing))
            handle
            process
            constructor-returned-p)
        (puthash content-key state slackit-media--audio-states)
        (unwind-protect
            (progn
              (setq handle
                    (appkit-register-handle
                     app 'process state #'slackit-media--cancel-audio))
              (setf (slackit-media-audio-handle state) handle)
              (setq process
                    (make-process
                     :name "slackit-media-audio-player"
                     :buffer nil
                     :command (append arguments (list file))
                     :noquery t
                     :sentinel
                     (apply-partially
                      #'slackit-media--audio-finished state)))
              (setf (slackit-media-audio-process state) process)
              (setq constructor-returned-p t))
          (unless constructor-returned-p
            (if handle
                (appkit-cancel-handle handle)
              (remhash content-key slackit-media--audio-states))))
        (when (and process (not (process-live-p process)))
          (slackit-media--audio-finished state process "finished"))
        (slackit-runtime-publish-resource app content-key)
        process))))

(defun slackit-media--open-content-file (content-key file)
  "Open or play local FILE according to CONTENT-KEY's media kind."
  (let* ((spec (slackit-media--content-spec-current content-key))
         (kind (and spec (slackit-media-open-spec-kind spec)))
         (app (and spec (slackit-media-open-spec-app spec))))
    (unless spec
      (user-error "slackit: media content is unavailable"))
    (pcase kind
      ('photo (slackit-media--open-local-image-file file))
      ('video (appkit-media-play-video-file file "slackit" :owner app))
      ('audio (slackit-media--start-audio-file content-key file))
      (_ (appkit-media-open-file file)))))

(defun slackit-media--open-content (content-key)
  "Download CONTENT-KEY when needed, then open or play it locally."
  (slackit-media--download-content
   content-key
   (apply-partially #'slackit-media--open-content-file content-key)))

(defun slackit-media--save-content-file (content-key file)
  "Save local CONTENT-KEY FILE to a user-selected destination."
  (let* ((spec (slackit-media--content-spec-current content-key))
         (name (or (and spec (slackit-media-open-spec-name spec))
                   (file-name-nondirectory file)))
         (target
          (read-file-name
           "Save Slack media as: "
           nil nil nil
           (appkit-media-sanitize-filename name))))
    (copy-file file target t)
    (message "slackit: media saved")))

(defun slackit-media--save-content (content-key)
  "Download CONTENT-KEY when needed, then save a local copy."
  (slackit-media--download-content
   content-key
   (apply-partially #'slackit-media--save-content-file content-key)))

(defun slackit-media--content-transfer (content-key)
  "Return Appkit transfer presentation for CONTENT-KEY."
  (let* ((state (slackit-media--content-state content-key))
         (status (plist-get state :status)))
    (pcase status
      ('downloading
       (list :direction 'download
             :state 'active
             :bytes-total (plist-get state :bytes-total)
             :action
             (apply-partially #'slackit-media--cancel-content content-key)))
      ('error
       (list :direction 'download
             :state 'failed
             :action
             (apply-partially #'slackit-media--download-content content-key)))
      ('not-downloaded
       (list :direction 'download
             :state 'idle
             :action
             (apply-partially #'slackit-media--download-content content-key))))))

(defun slackit-media-ensure-message (app message)
  "Start deduplicated image acquisition for APP's normalized MESSAGE.

This function only acquires resources.  Rendering remains a separate,
deterministic operation in `slackit-media-insert-message-cards'."
  (when (and (appkit-app-live-p app)
             (appkit-media-inline-image-rendering-available-p))
    (dolist (item (slackit-media--message-items app message))
      (slackit-media--ensure-item app item)))
  nil)

(defun slackit-media-ensure-public-image (app resource-key source)
  "Ensure credential-free public image SOURCE for APP and RESOURCE-KEY."
  (when (slackit-media--public-source-p source)
    (slackit-media--ensure-item
     app (list :source source :resource-key resource-key))))


(defun slackit-media--safe-context-payload (item)
  "Return non-capability metadata payload for media card ITEM."
  (list :resource-key (plist-get item :resource-key)
        :content-resource-key (plist-get item :content-resource-key)
        :kind (plist-get item :kind)
        :title (plist-get item :title)))

(defun slackit-media--item-context (item)
  "Return backend-neutral card context for ITEM."
  (let* ((kind (plist-get item :kind))
         (preview-key (plist-get item :resource-key))
         (content-key (plist-get item :content-resource-key))
         (state (and content-key
                     (slackit-media--content-state content-key)))
         (status (plist-get state :status))
         (open-action
          (cond
           (content-key
            (apply-partially #'slackit-media--open-content content-key))
           ((and (eq kind 'photo)
                 (slackit-media--cached-file preview-key))
            (apply-partially
             #'slackit-media--open-cached-image preview-key)))))
    (appkit-media-card-context-create
     :payload (slackit-media--safe-context-payload item)
     :kind kind
     :title (plist-get item :title)
     :open-action open-action
     :download-action
     (and content-key
          (not (memq status '(downloading downloaded)))
          (apply-partially
           #'slackit-media--download-content content-key))
     :cancel-action
     (and content-key
          (eq status 'downloading)
          (apply-partially
           #'slackit-media--cancel-content content-key))
     :save-as-action
     (and content-key
          (apply-partially #'slackit-media--save-content content-key)))))

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
           nil)
        (error (insert "[preview unavailable]")))
      (insert "\n"))
     ((gethash key slackit-media--fetches)
      (insert "[loading preview]\n"))
     (t
      (insert "[preview unavailable]\n")))
    (appkit-ui-apply-line-prefix start (point) prefix-state)
    (unless display-image
      (appkit-ui-append-face start (point) 'shadow))))

(defun slackit-media--audio-control-state (content-key)
  "Return Appkit voice-note state for audio CONTENT-KEY."
  (let ((audio (and content-key
                    (gethash content-key slackit-media--audio-states))))
    (cond
     ((and audio
           (process-live-p (slackit-media-audio-process audio)))
      'playing)
     ((eq (plist-get (slackit-media--content-state content-key) :status)
          'downloading)
      'preparing)
     ((eq (plist-get (slackit-media--content-state content-key) :status)
          'error)
      'failed)
     ((and audio (eq (slackit-media-audio-status audio) 'finished))
      'finished)
     (t 'idle))))

(defun slackit-media--insert-item-body (item context prefix-state)
  "Insert ITEM preview or audio control through CONTEXT and PREFIX-STATE."
  (let ((kind (plist-get item :kind))
        (content-key (plist-get item :content-resource-key)))
    (when (and (plist-get item :source)
               (memq kind '(photo video)))
      (slackit-media--insert-poster item context prefix-state))
    (when (eq kind 'audio)
      (appkit-chat-ins-insert-voice-note
       :state (slackit-media--audio-control-state content-key)
       :duration-seconds
       (and (numberp (plist-get item :duration-ms))
            (/ (max 0 (plist-get item :duration-ms)) 1000.0))
       :prefix prefix-state
       :face 'shadow
       :action (plist-get context :open-action)))))

(defun slackit-media--insert-item-card (item prefix properties)
  "Insert one deterministic media ITEM card using PREFIX and PROPERTIES."
  (let* ((context (slackit-media--item-context item))
         (content-key (plist-get item :content-resource-key))
         (state (and content-key
                     (slackit-media--content-state content-key))))
    (appkit-chat-ins-insert-media-card
     :kind (plist-get item :kind)
     :title (plist-get item :title)
     :meta (plist-get item :meta)
     :transfer (and content-key
                    (slackit-media--content-transfer content-key))
     :status
     (and state
          (appkit-chat-ins-media-transfer-status-text state))
     :prefix prefix
     :title-face 'bold
     :meta-face 'shadow
     :properties properties
     :context context
     :body-inserter
     (lambda (prefix-state)
       (slackit-media--insert-item-body item context prefix-state)))))

(defun slackit-media-insert-message-cards (app message &optional prefix properties)
  "Insert deterministic rich media cards for APP's normalized MESSAGE.

PREFIX and PROPERTIES are forwarded to Appkit's shared card inserter.  This
function only observes memory and disk cache state; it never starts transfer."
  (dolist (item (slackit-media--message-items app message))
    (slackit-media--insert-item-card item prefix properties)))

(defun slackit-media-clear-memory-cache ()
  "Clear decoded media images and bounded failure state."
  (clrhash slackit-media--image-cache)
  (clrhash slackit-media--failures)
  (appkit-media-clear-video-decoration-cache 'slackit))

(provide 'slackit-media)

;;; slackit-media.el ends here
