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
(require 'slackit-runtime)

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
  success-function
  failure-function)

(cl-defstruct (slackit-media-open-spec
               (:constructor slackit-media-open-spec-create))
  app
  generation
  kind
  source
  name
  mime-type
  size
  duration-ms
  private-source-p)

(defun slackit-media--pending-input (key)
  "Return the exact current Surface's pending acquisition for KEY."
  (when-let* ((surface (appkit-current-surface))
              ((appkit-surface-live-p surface)))
    (cdr (assoc key (plist-get (appkit-surface-model surface) :media-inputs)))))

(defun slackit-media--commit-content-file (input file)
  "Commit INPUT's independently acquired FILE into its account-private cache."
  (unless (slackit-media--input-current-p input)
    (error "slackit: stale content acquisition"))
  (let* ((key (plist-get input :key))
         (fetch-key (plist-get input :fetch-key))
         (spec (plist-get input :spec))
         (target (slackit-media--private-cache-file
                  key (slackit-media-open-spec-source spec) (slackit-media-open-spec-name spec))))
    (unless (equal file target) (rename-file file target t))
    (remhash fetch-key slackit-media--content-files)
    (remhash fetch-key slackit-media--image-cache)
    (remhash key slackit-media--failures)
    (puthash key target slackit-media--content-files)
    (when (eq (slackit-media-open-spec-kind spec) 'photo)
      (puthash key (list target (file-attribute-modification-time (file-attributes target))
                         (appkit-media-preview-image-from-file target))
               slackit-media--image-cache))
    target))

(defvar slackit-media--content-files (make-hash-table :test #'equal)
  "Validated local content files discovered only by acquisition.")

(defun slackit-media--cached-file (key)
  "Return KEY's acquired local file without filesystem discovery."
  (or (gethash key slackit-media--content-files)
      (car (gethash key slackit-media--image-cache))))

(defun slackit-media--cached-image (key)
  "Return KEY's acquired image descriptor without filesystem discovery."
  (let ((image (nth 2 (gethash key slackit-media--image-cache))))
    (and (not (eq image :invalid)) image)))

(defun slackit-media--audio-source-start (_context input emit closed)
  "Start real local playback owned by INPUT's initiating Surface Source."
  (condition-case nil
      (progn
        (unless (slackit-media--input-current-p input)
          (error "slackit: stale audio Surface"))
        (let* ((key (plist-get input :key))
               (spec (plist-get input :spec))
               (previous (gethash key slackit-media--audio-sessions))
               session)
          (when (and previous (not (appkit-media-player-session-finalized-p previous)))
            (appkit-media-player-stop previous))
          (setq session
                (appkit-media-player-start-file
                 (plist-get input :file) :kind 'audio :owner (plist-get input :surface)
                 :duration-seconds
                 (let ((duration (slackit-media-open-spec-duration-ms spec)))
                   (and (numberp duration) (/ (max 0 duration) 1000.0)))
                 :on-change (lambda (session) (funcall emit (appkit-media-player-status session)))
                 :on-finalize (lambda (_session) (funcall closed))))
          (setf (plist-get input :session) session)
          (puthash key session slackit-media--audio-sessions)
          (appkit-source-cancellation-create
           :kind 'transport :cancel (lambda () (appkit-media-player-stop session)))))
    (error (funcall closed 'player-unavailable) nil)))

(defun slackit-media--audio-source-outbound (_context input _payload _settled)
  "Toggle the exact player captured by INPUT's live Source."
  (if-let* ((session (plist-get input :session)))
      (progn (appkit-media-player-toggle session) 'accepted)
    'closed))

(defun slackit-media--audio-intent-result (_input outcome)
  "Map a playback command OUTCOME without publishing reentrantly."
  (list 'slackit-audio-intent outcome))

(defun slackit-media-sources (model)
  "Describe MODEL's physically owned local audio streams."
  (mapcar
   (lambda (entry)
     (let ((input (cdr entry)))
       (appkit-source-spec-create
        :key (list 'audio (car entry))
        :identity (list (plist-get input :generation) (car entry))
        :input input :start #'slackit-media--audio-source-start
        :outbound #'slackit-media--audio-source-outbound :outbound-pending-limit 4
        :event (lambda (input status) (list 'slackit-audio-state input status))
        :closed (lambda (input &rest reason) (list 'slackit-audio-closed input (car reason)))
        :emission-policy 'latest :pending-limit 1 :cancellation-requirement 'transport)))
   (plist-get model :audio-inputs)))

(defun slackit-media-audio-update (model message)
  "Commit playback MESSAGE and an exact media redraw."
  (let* ((input (cadr message))
         (key (and (listp input) (plist-get input :key)))
         (inputs (plist-get model :audio-inputs)))
    (when (and (eq (car message) 'slackit-audio-closed)
               (eq input (cdr (assoc key inputs))))
      (setq model (plist-put (copy-sequence model) :audio-inputs
                             (assoc-delete-all key (copy-sequence inputs)))))
    (when (and (eq (car message) 'slackit-audio-closed) (caddr message))
      (setq model (plist-put (copy-sequence model) :media
                             (list :phase 'failed :key key :error (caddr message)))))
    (appkit-next :model model
                 :render (appkit-projection-change-create
                          :frame-p t :full-p t :resources (and key (list key))))))

(defun slackit-media--request (key action)
  "Acquire KEY under the initiating Surface before committed ACTION."
  (let* ((surface (appkit-current-surface))
         (spec (slackit-media--content-spec-current key)))
    (unless (and (appkit-surface-live-p surface) spec
                 (eq (appkit-surface-app surface) (slackit-media-open-spec-app spec)))
      (user-error "slackit: media requires its live account Surface"))
    (appkit-surface-send
     surface
     (list 'slackit-media (if (eq action 'cancel) 'cancel 'acquire)
           (list :surface surface :app (appkit-surface-app surface)
                 :model (appkit-app-model (appkit-surface-app surface))
                 :generation (slackit-runtime-generation (appkit-surface-app surface))
                 :identity (copy-tree (appkit-surface-identity surface))
                 :key key :spec spec :action action)))))

(defun slackit-media--input-current-p (input)
  "Whether INPUT still owns its exact initiating Surface and account."
  (let ((surface (plist-get input :surface)) (app (plist-get input :app)))
    (and (appkit-surface-live-p surface)
         (eq app (appkit-surface-app surface))
         (eq (plist-get input :model) (appkit-app-model app))
         (equal (plist-get input :identity) (appkit-surface-identity surface))
         (slackit-runtime-current-p app (plist-get input :generation)))))

(defun slackit-media--acquire-start (_context input _observe resolve reject)
  "Start INPUT's real Surface-owned acquisition."
  (unless (slackit-media--input-current-p input)
    (error "slackit: stale media Surface"))
  (if-let* ((file (plist-get input :local-file)))
      (progn
        (if (file-regular-p file) (funcall resolve file) (funcall reject 'missing-file))
        nil)
    (let* ((spec (plist-get input :spec))
           (settled nil)
           (fetch
            (slackit-media--ensure-item
             (plist-get input :app)
             (list :source (slackit-media-open-spec-source spec)
                   :resource-key (plist-get input :fetch-key) :cache-key (plist-get input :key) :purpose 'content
                   :kind (slackit-media-open-spec-kind spec)
                   :private-source-p (slackit-media-open-spec-private-source-p spec)
                   :cache-name (slackit-media-open-spec-name spec)
                   :mime-type (slackit-media-open-spec-mime-type spec))
             (lambda (file)
               (setq settled t)
               (condition-case nil
                   (funcall resolve (slackit-media--commit-content-file input file))
                 (error (funcall reject 'cache-error))))
             (lambda (reason)
               (setq settled t)
               (when (slackit-media--input-current-p input)
                 (slackit-media--record-failure (plist-get input :key)))
               (remhash (plist-get input :fetch-key) slackit-media--failures)
               (funcall reject reason))
             (plist-get input :surface))))
      (cond
       ((slackit-media-fetch-p fetch)
        (appkit-cancellation-create
         :kind 'transport
         :cancel (lambda () (appkit-cancel-handle (slackit-media-fetch-handle fetch)))))
       (settled nil)
       (t (funcall reject 'temporarily-unavailable) nil)))))

(defun slackit-media--present-start (_context input _observe resolve reject)
  "Perform committed local presentation for INPUT's exact Surface."
  (if (not (slackit-media--input-current-p input))
      (funcall reject 'stale)
    (condition-case nil
        (progn
          (with-current-buffer (appkit-surface-buffer (plist-get input :surface))
            (pcase (plist-get input :action)
              ('open (if (plist-get input :local-file)
                         (appkit-media-open-file (plist-get input :file))
                       (slackit-media--open-content-file
                        (plist-get input :key) (plist-get input :file)
                        (plist-get input :surface))))
              ('save (slackit-media--save-content-file
                      (plist-get input :key) (plist-get input :file)))))
          (funcall resolve t))
      (error (funcall reject 'presentation-failed))))
  nil)

(defun slackit-media-update (_context model message)
  "Return finite acquisition and presentation Effects for media MESSAGE."
  (pcase-let ((`(slackit-media ,phase ,input . ,payload) message))
    (if (not (slackit-media--input-current-p input))
        (appkit-next :model model :render appkit-render-none)
      (let (effect commands)
        (when (slackit-media--input-current-p input)
          (pcase phase
            ('acquire
             (setq input (plist-put (copy-sequence input) :fetch-key
                                    (list (plist-get input :key)
                                          (cl-incf slackit-runtime--operation-nonce))))
             (setq effect
                   (appkit-effect-create
                    :key (list 'media (plist-get input :key)) :input input
                    :start #'slackit-media--acquire-start :cancellation-requirement 'transport
                    :success (lambda (input file) (list 'slackit-media 'acquired input file))
                    :failure (lambda (input reason) (list 'slackit-media 'failed input reason)))))
            ('cancel
             (push (appkit-command-cancel-effect (list 'media (plist-get input :key))) commands)
             (push (appkit-command-cancel-effect (list 'media-present (plist-get input :key))) commands))
            ('acquired
             (if (and (eq (plist-get input :action) 'open)
                      (eq (slackit-media-open-spec-kind (plist-get input :spec)) 'audio))
                 (let* ((key (plist-get input :key))
                        (existing (cdr (assoc key (plist-get model :audio-inputs)))))
                   (if existing
                       (push (appkit-command-source-intent
                              :key (list 'audio key)
                              :expected-identity (list (plist-get existing :generation) key)
                              :payload 'toggle :result-mapper #'slackit-media--audio-intent-result)
                             commands)
                     (let ((audio-input (append (copy-sequence input)
                                                (list :file (car payload) :session nil))))
                       (setq model (plist-put (copy-sequence model) :audio-inputs
                                              (cons (cons key audio-input)
                                                    (plist-get model :audio-inputs)))))))
               (when (memq (plist-get input :action) '(open save))
                 (setq effect
                       (appkit-effect-create
                        :key (list 'media-present (plist-get input :key))
                        :input (plist-put (copy-sequence input) :file (car payload))
                        :start #'slackit-media--present-start
                        :success (lambda (input &rest _) (list 'slackit-media 'presented input))
                        :failure (lambda (input reason) (list 'slackit-media 'failed input reason)))))))))
        (setq model
              (plist-put (copy-sequence model) :media-inputs
                         (let* ((key (plist-get input :key))
                                (entries (assoc-delete-all key (copy-sequence (plist-get model :media-inputs)))))
                           (if (eq phase 'acquire) (cons (cons key input) entries) entries))))
        (appkit-next
         :model (plist-put (copy-sequence model) :media
                           (list :phase phase :key (plist-get input :key)
                                 :error (and (eq phase 'failed) (car payload))))
         :render (appkit-projection-change-create :full-p t :frame-p t)
         :commands (append commands (and effect (list (appkit-command-start-effect effect)))))))))

(defun slackit-media-demand (app item)
  "Describe one private ITEM acquisition without starting transport."
  (let* ((key (plist-get item :resource-key))
         (source (plist-get item :source))
         (input (list :resource-key key :source source
                      :private-source-p (plist-get item :private-source-p)
                      :purpose (plist-get item :purpose) :kind (plist-get item :kind)
                      :cache-name (plist-get item :cache-name) :mime-type (plist-get item :mime-type))))
    (when source
      (appkit-resource-demand-create
       :key key :input (list app (slackit-runtime-generation app) input)
       :loader #'slackit-media--load
       :acquisition-identity (list key (secure-hash 'sha256 source))
       :sharing-policy 'app-private :cache-policy 'while-interested))))

(defun slackit-media--load (_context input resolve reject)
  "Acquire declared INPUT with the existing validated transport."
  (pcase-let ((`(,app ,generation ,item) input))
    (unless (slackit-runtime-current-p app generation)
      (error "slackit: retired media account"))
    (let* ((settled nil)
           (fetch (slackit-media--ensure-item
                   app item
                   (lambda (file) (setq settled t) (funcall resolve file))
                   (lambda (reason) (setq settled t) (funcall reject reason)))))
      (cond
       ((slackit-media-fetch-p fetch)
        (appkit-cancellation-create
         :kind 'transport
         :cancel (lambda () (appkit-cancel-handle (slackit-media-fetch-handle fetch)))))
       (settled nil)
       (t (funcall reject 'temporarily-unavailable) nil)))))

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

(defvar slackit-media--image-cache (make-hash-table :test #'equal)
  "Decoded poster records keyed by opaque media resource identity.")

(defvar slackit-media--fetches (make-hash-table :test #'equal)
  "Current account-owned media fetches keyed by opaque resource identity.")

(defvar slackit-media--failures (make-hash-table :test #'equal)
  "Bounded media failure timestamps keyed by opaque resource identity.")

(defvar slackit-media--open-specs (make-hash-table :test #'equal)
  "Opaque content keys to account-owned media specifications.")

(defvar slackit-media--audio-sessions (make-hash-table :test #'equal)
  "Appkit audio sessions keyed by content resource identity.")

(defvar slackit-media--prepared-cache-directory nil
  "Expanded private media cache directory prepared in this Emacs session.")

(defun slackit-media--non-empty-string (value)
  "Return trimmed VALUE when it is a non-empty string, otherwise nil."
  (and (stringp value)
       (let ((trimmed (string-trim value)))
         (and (not (string-empty-p trimmed)) trimmed))))

(defun slackit-media--clear-app-specs (app)
  "Remove every private media specification and session owned by APP."
  (let (keys)
    (maphash
     (lambda (key spec)
       (when (eq app (slackit-media-open-spec-app spec))
         (push key keys)))
     slackit-media--open-specs)
    (dolist (key keys)
      (remhash key slackit-media--open-specs)
      (remhash key slackit-media--content-files)
      (remhash key slackit-media--audio-sessions))))

(defun slackit-media--register-content-spec
    (app identity kind source name mime-type size &optional duration-ms)
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
          :duration-ms duration-ms
          :private-source-p private-source-p)
         slackit-media--open-specs)
        key))))

(defun slackit-media--account-scope (app)
  "Return opaque stable account scope for APP."
  (secure-hash 'sha256 (prin1-to-string (appkit-app-identity app))))

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
            (alist-get 'text value)))))

(defun slackit-media--block-title (block fallback)
  "Return BLOCK title, using FALLBACK when its title is absent."
  (or (slackit-media--text-object-string
       (alist-get 'title block))
      (slackit-media--non-empty-string
       (alist-get 'alt_text block))
      fallback))

(defun slackit-media--block-item (app block path)
  "Return one supported media item for APP BLOCK at PATH, or nil."
  (pcase (alist-get 'type block)
    ("image"
     (let* ((source (alist-get 'image_url block))
            (title (slackit-media--block-title block "Image"))
            (identity
             (list 'block path
                   (alist-get 'block_id block)
                   (alist-get 'alt_text block)))
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
                    (alist-get 'alt_text block)))))
    ("video"
     (let* ((preview-source
             (alist-get 'thumbnail_url block))
            (content-source
             (alist-get 'video_url block))
            (title (slackit-media--block-title block "Video"))
            (identity
             (list 'block path
                   (alist-get 'block_id block)
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
                  (alist-get 'description block))
                 (slackit-media--non-empty-string
                  (alist-get 'provider_name block))
                 (slackit-media--non-empty-string
                  (alist-get 'author_name block))))))))

(defun slackit-media--collect-block-items (app node path)
  "Collect supported media items recursively from APP NODE at PATH."
  (when (listp node)
    (if-let* ((item (slackit-media--block-item app node path)))
        (list item)
      (let* ((accessory (alist-get 'accessory node))
             (elements (alist-get 'elements node))
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
  (let* ((blocks (alist-get 'blocks message))
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
  (let ((mime (downcase (or (alist-get 'mimetype file) "")))
        (type (downcase (or (alist-get 'filetype file) ""))))
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
       (alist-get 'title file))
      (slackit-media--non-empty-string
       (alist-get 'name file))
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
           (alist-get 'thumb_video file)))
     (cl-loop for field in slackit-media--file-preview-fields
              for value = (alist-get field file)
              when (slackit-media--non-empty-string value)
              return value)
     (and (eq kind 'photo)
          (slackit-media--non-empty-string
           (alist-get 'url_private file))))))

(defun slackit-media--file-open-source (file _kind)
  "Return FILE's original content source."
  (or (slackit-media--non-empty-string
       (alist-get 'url_private_download file))
      (slackit-media--non-empty-string
       (alist-get 'url_private file))))

(defun slackit-media--file-meta (file)
  "Return safe compact metadata strings for normalized Slack FILE."
  (let ((type (or (slackit-media--non-empty-string
                   (alist-get 'pretty_type file))
                  (slackit-media--non-empty-string
                   (alist-get 'mimetype file))
                  (slackit-media--non-empty-string
                   (alist-get 'filetype file))))
        (size (alist-get 'size file))
        (duration-ms (alist-get 'duration_ms file)))
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
   for file in (or (alist-get 'files message) nil)
   for index from 0
   when (listp file)
   collect
   (let* ((id (alist-get 'id file))
          (kind (slackit-media--file-kind file))
          (source (slackit-media--file-preview-source file kind))
          (content-source (slackit-media--file-open-source file kind))
          (name (alist-get 'name file))
          (mime-type (alist-get 'mimetype file))
          (size (alist-get 'size file))
          (identity
           (if (slackit-media--non-empty-string id)
               (list 'id id)
             (list 'index index name mime-type size)))
          (private-source-p (slackit-media--private-source-p source))
          (content-key
           (slackit-media--register-content-spec
            app (list 'file identity) kind content-source
            name mime-type size
            (alist-get 'duration_ms file))))
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
           :duration-ms (alist-get 'duration_ms file)))))

(defun slackit-media--message-items (app message)
  "Return deterministic media card item plists for APP and MESSAGE."
  (when (and (appkit-app-p app) (listp message))
    (append (slackit-media--block-items app message)
            (slackit-media--file-items app message))))

(defun slackit-media-message-media-only-p (message)
  "Return non-nil when MESSAGE blocks are entirely supported media blocks."
  (let ((blocks (alist-get 'blocks message)))
    (and (listp blocks)
         blocks
         (cl-every
          (lambda (block)
            (member (alist-get 'type block)
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

(defun slackit-media--discover-cache-file (resource-key)
  "Return RESOURCE-KEY's existing local cache file, or nil."
  (and resource-key
       (appkit-media-image-cache-existing-file
        (slackit-media--cache-base resource-key))))

(defun slackit-media--open-local-image-file (file)
  "Open local image FILE through Appkit's browser-free media runtime."
  (unless (appkit-media-file-present-p file)
    (user-error "slackit: cached image is unavailable"))
  (appkit-media-open-file file))

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

(defun slackit-media--discover-image (resource-key)
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
    (setf (slackit-media-fetch-success-function owner) nil
          (slackit-media-fetch-failure-function owner) nil)
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
          (slackit-media-fetch-success-function owner))
         (failure-function (slackit-media-fetch-failure-function owner)))
    (when (file-regular-p file)
      (unless (memq system-type '(ms-dos windows-nt cygwin))
        (set-file-modes file #o600)))
    (when current-p
      (remhash key slackit-media--image-cache)
      (if valid-p
          (progn
            (remhash key slackit-media--failures)
            (when content-p (puthash key file slackit-media--content-files))
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
    (when (and current-p (not valid-p) failure-function)
      (funcall failure-function 'invalid-media))
    (when (and valid-p (functionp success-function))
      (condition-case nil
          (funcall success-function file)
        (error
         (message "slackit: failed to handle downloaded media"))))))

(defun slackit-media--fetch-failure (owner _reason)
  "Settle failed media fetch OWNER without exposing remote details."
  (let ((current-p (slackit-media--owner-current-p owner))
        (key (slackit-media-fetch-resource-key owner))
        (failure-function (slackit-media-fetch-failure-function owner)))
    (when-let* ((file (slackit-media-fetch-cache-file owner)))
      (setf (slackit-media-fetch-cache-file owner) nil)
      (when (file-exists-p file)
        (ignore-errors (delete-file file))))
    (setf (slackit-media-fetch-success-function owner) nil)
    (when current-p
      (slackit-media--record-failure key))
    (slackit-media--retire-fetch owner)
    (when (and current-p failure-function)
      (funcall failure-function 'request-failed))))

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

(defun slackit-media--ensure-item (app item &optional success-function failure-function lifecycle-owner)
  "Start one deduplicated media acquisition for APP and ITEM.

SUCCESS-FUNCTION receives the validated local file after acquisition."
  (let* ((source (plist-get item :source))
         (key (plist-get item :resource-key))
         (cache-key (or (plist-get item :cache-key) key))
         (purpose (or (plist-get item :purpose) 'preview))
         (kind (or (plist-get item :kind) 'photo))
         (content-p (eq purpose 'content))
         (cached-file
          (if content-p
              (slackit-media--existing-content-file
               cache-key source (plist-get item :cache-name))
            (slackit-media--discover-cache-file key)))
         (cached-image
          (and cached-file
               (or (not content-p) (eq kind 'photo))
               (slackit-media--discover-image cache-key)))
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
      (when content-p (puthash key cached-file slackit-media--content-files))
      (when (functionp success-function)
        (funcall success-function cached-file))
      nil)
     (active
      (when (functionp failure-function)
        (let ((old (slackit-media-fetch-failure-function active)))
          (setf (slackit-media-fetch-failure-function active)
                (if (functionp old)
                    (apply-partially #'slackit-media--compose-success-functions old failure-function)
                  failure-function))))
      (when (functionp success-function)
        (let ((old (slackit-media-fetch-success-function active)))
          (setf (slackit-media-fetch-success-function active)
                (if (functionp old)
                    (apply-partially
                     #'slackit-media--compose-success-functions
                     old success-function)
                  success-function))))
      active)
     ((and source key (slackit-media--retry-due-p cache-key))
      (slackit-media--prepare-cache-directory)
      (remhash key slackit-media--content-files)
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
               :success-function success-function :failure-function failure-function))
             (handle
              (appkit-register-handle
               (or lifecycle-owner app) 'slackit-media owner
               #'slackit-media--cancel-fetch)))
        (setf (slackit-media-fetch-handle owner) handle)
        (puthash key owner slackit-media--fetches)
        (condition-case nil
            (let ((transfer
                   (cond
                    ((plist-get item :private-source-p)
                     (slackit-media--private-transfer owner item))
                    (content-p
                     (let ((target
                            (slackit-media--private-cache-file
                             cache-key source (plist-get item :cache-name))))
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
  "Return acquired CONTENT-KEY's local file while SPEC remains available."
  (and spec (gethash content-key slackit-media--content-files)))

(defun slackit-media--content-state (content-key)
  "Return normalized transfer state for CONTENT-KEY."
  (let* ((spec (slackit-media--content-spec-current content-key))
         (file (and spec
                    (slackit-media--content-cached-file content-key spec)))
         (active (or (slackit-media--pending-input content-key)
                     (gethash content-key slackit-media--fetches))))
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
  "Cancel only the initiating Surface's CONTENT-KEY Effect."
  (unless (slackit-media--pending-input content-key)
    (user-error "slackit: media is not downloading in this Surface"))
  (slackit-media--request content-key 'cancel))

(defun slackit-media--download-content (content-key)
  "Acquire CONTENT-KEY under the initiating live Surface."
  (slackit-media--request content-key 'download))

(defun slackit-media--open-content-file (content-key file &optional owner)
  "Open or play local FILE according to CONTENT-KEY's media kind."
  (let* ((spec (slackit-media--content-spec-current content-key))
         (kind (and spec (slackit-media-open-spec-kind spec)))
         (app (and spec (slackit-media-open-spec-app spec))))
    (unless spec
      (user-error "slackit: media content is unavailable"))
    (pcase kind
      ('photo (slackit-media--open-local-image-file file))
      ('video (appkit-media-play-video-file file "slackit" :owner (or owner app)))
      (_ (appkit-media-open-file file)))))

(defun slackit-media--open-content (content-key)
  "Acquire CONTENT-KEY under the initiating Surface, then present it."
  (slackit-media--request content-key 'open))

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
  "Acquire CONTENT-KEY under the initiating Surface, then save a copy."
  (slackit-media--request content-key 'save))

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
  (let* ((session
          (and content-key
               (gethash content-key slackit-media--audio-sessions)))
         (session-status
          (and (appkit-media-player-session-p session)
               (appkit-media-player-status session)))
         (transfer-status
          (plist-get (slackit-media--content-state content-key) :status)))
    (cond
     ((eq transfer-status 'downloading) 'preparing)
     ((eq transfer-status 'error) 'failed)
     ((null session-status) 'idle)
     ((eq session-status 'starting) 'preparing)
     ((memq session-status '(playing paused finished failed))
      session-status)
     (t 'idle))))

(defun slackit-media--audio-played-seconds (content-key)
  "Return current Appkit playback progress for audio CONTENT-KEY."
  (when-let* ((session
               (gethash content-key slackit-media--audio-sessions))
              ((appkit-media-player-session-p session)))
    (appkit-media-player-played-seconds session)))

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
       :played-seconds
       (slackit-media--audio-played-seconds content-key)
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
