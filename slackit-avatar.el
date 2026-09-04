;;; slackit-avatar.el --- Slackit profile image resources -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Account-owned acquisition and persistent byte caching for public Slack
;; profile images.  User metadata remains canonical, session-local account
;; state; only image bytes cross Emacs restarts.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'url-parse)
(require 'appkit-core)
(require 'appkit-media)
(require 'slackit-customize)
(require 'slackit-runtime)

(cl-defstruct (slackit-avatar-fetch
               (:constructor slackit-avatar-fetch-create))
  app
  generation
  resource-key
  cache-base
  transfer
  handle)

(defvar slackit-avatar--image-cache (make-hash-table :test #'equal)
  "Decoded images keyed by resource identity, pixel size, and source mtime.")

(defvar slackit-avatar--sources (make-hash-table :test #'equal)
  "Ready local avatar sources keyed by opaque resource identity.")

(defvar slackit-avatar--fetches (make-hash-table :test #'equal)
  "Current account-owned avatar fetches keyed by opaque resource identity.")

(defvar slackit-avatar--failures (make-hash-table :test #'equal)
  "Last failure time for each opaque avatar resource identity.")

(defvar slackit-avatar--prepared-cache-directory nil
  "Expanded avatar cache directory already prepared in this Emacs session.")

(defun slackit-avatar--profile-url (user)
  "Return USER's preferred Slack profile image URL, or nil."
  (let ((profile (alist-get 'profile user)))
    (seq-some
     (lambda (key)
       (let ((value (alist-get key profile)))
         (and (stringp value) (not (string-empty-p value)) value)))
     '(image_72 image_48 image_32 image_24))))

(defun slackit-avatar--valid-url-p (url)
  "Return non-nil when URL is an allowed public Slack profile image URL."
  (and (stringp url)
       (not (string-match-p "[[:space:]\"\\\\]" url))
       (condition-case nil
           (let ((parsed (url-generic-parse-url url)))
             (and (string-equal (url-type parsed) "https")
                  (stringp (url-host parsed))
                  (string-match-p slackit-avatar-host-regexp
                                  (downcase (url-host parsed)))
                  (null (url-user parsed))
                  (null (url-password parsed))))
         (error nil))))

(defun slackit-avatar-resource-key (app user)
  "Return opaque profile image resource identity for USER owned by APP."
  (let ((user-id (alist-get 'id user))
        (url (slackit-avatar--profile-url user)))
    (when (and (appkit-app-p app)
               (stringp user-id)
               (not (string-empty-p user-id))
               (slackit-avatar--valid-url-p url))
      (list :avatar
            (format "%s" (appkit-app-id app))
            user-id
            (secure-hash 'sha256 url)))))

(defun slackit-avatar--cache-scope (resource-key)
  "Return account/user cache scope hash for RESOURCE-KEY."
  (secure-hash 'sha256
               (prin1-to-string (seq-take resource-key 3))))

(defun slackit-avatar--cache-base (resource-key)
  "Return versioned private cache base path for opaque RESOURCE-KEY."
  (expand-file-name
   (format "%s-%s"
           (slackit-avatar--cache-scope resource-key)
           (or (nth 3 resource-key) "unknown"))
   slackit-avatar-cache-directory))

(defun slackit-avatar--discard-derived-images (resource-key)
  "Discard every decoded image derived from RESOURCE-KEY."
  (let (keys)
    (maphash
     (lambda (key _image)
       (when (equal (car-safe key) resource-key)
         (push key keys)))
     slackit-avatar--image-cache)
    (dolist (key keys)
      (remhash key slackit-avatar--image-cache))))

(defun slackit-avatar--forget-source (resource-key)
  "Forget RESOURCE-KEY's ready source and all derived images."
  (remhash resource-key slackit-avatar--sources)
  (slackit-avatar--discard-derived-images resource-key))

(defun slackit-avatar--remember-source (resource-key file)
  "Remember FILE as RESOURCE-KEY's ready source, returning its record.

The record contains only the private local path and its modification time.
Changing either invalidates every decoded size derived from the old source."
  (when (stringp file)
    (let* ((attributes (file-attributes file))
           (mtime
            (and attributes
                 (null (file-attribute-type attributes))
                 (file-attribute-modification-time attributes)))
           (record (and mtime (list file mtime))))
      (when record
        (unless (equal record (gethash resource-key slackit-avatar--sources))
          (slackit-avatar--discard-derived-images resource-key))
        (puthash resource-key record slackit-avatar--sources)
        record))))

(defun slackit-avatar--delete-stale-cache-files (resource-key keep-file)
  "Delete old profile image versions for RESOURCE-KEY except KEEP-FILE."
  (when (file-directory-p slackit-avatar-cache-directory)
    (let ((regexp
           (concat "\\`"
                   (regexp-quote
                    (concat (slackit-avatar--cache-scope resource-key) "-")))))
      (dolist (file
               (directory-files slackit-avatar-cache-directory t regexp))
        (when (and (file-regular-p file)
                   (not (file-equal-p file keep-file)))
          (ignore-errors (delete-file file)))))))

(defun slackit-avatar--prepare-cache-directory ()
  "Prepare and return the private avatar cache directory."
  (let ((directory (expand-file-name slackit-avatar-cache-directory)))
    (unless (and (equal directory
                        slackit-avatar--prepared-cache-directory)
                 (file-directory-p directory))
      (make-directory directory t)
      (unless (memq system-type '(ms-dos windows-nt cygwin))
        (set-file-modes directory #o700))
      (setq slackit-avatar--prepared-cache-directory directory))
    directory))

(defun slackit-avatar--owner-current-p (owner)
  "Return non-nil when avatar fetch OWNER still owns publication."
  (let ((app (slackit-avatar-fetch-app owner))
        (resource-key (slackit-avatar-fetch-resource-key owner)))
    (and (appkit-app-live-p app)
         (slackit-runtime-current-p
          app (slackit-avatar-fetch-generation owner))
         (eq owner (gethash resource-key slackit-avatar--fetches)))))

(defun slackit-avatar--cancel-fetch (owner)
  "Cancel account-owned avatar fetch OWNER without publishing."
  (let ((resource-key (slackit-avatar-fetch-resource-key owner)))
    (when (eq owner (gethash resource-key slackit-avatar--fetches))
      (remhash resource-key slackit-avatar--fetches))
    (when-let* ((transfer (slackit-avatar-fetch-transfer owner)))
      (setf (slackit-avatar-fetch-transfer owner) nil)
      (when (appkit-media-transfer-p transfer)
        (appkit-media-cancel-transfer transfer)))))

(defun slackit-avatar--retire-fetch (owner)
  "Retire completed avatar fetch OWNER and its Appkit handle."
  (let ((resource-key (slackit-avatar-fetch-resource-key owner)))
    (when (eq owner (gethash resource-key slackit-avatar--fetches))
      (remhash resource-key slackit-avatar--fetches))
    (setf (slackit-avatar-fetch-transfer owner) nil)
    (when-let* ((handle (slackit-avatar-fetch-handle owner)))
      (when (appkit-handle-alive-p handle)
        (appkit-retire-handle handle)))))

(defun slackit-avatar--fetch-success (owner file)
  "Settle avatar fetch OWNER with private cache FILE."
  (let* ((current-p (slackit-avatar--owner-current-p owner))
         (app (slackit-avatar-fetch-app owner))
         (resource-key (slackit-avatar-fetch-resource-key owner))
         source)
    (when (and (file-regular-p file)
               (not (memq system-type '(ms-dos windows-nt cygwin))))
      (set-file-modes file #o600))
    (when current-p
      (slackit-avatar--delete-stale-cache-files resource-key file)
      (setq source (slackit-avatar--remember-source resource-key file))
      (remhash resource-key slackit-avatar--failures))
    (slackit-avatar--retire-fetch owner)
    (when (and current-p source)
      (slackit-runtime-publish-resource app resource-key))))

(defun slackit-avatar--fetch-failure (owner _error)
  "Settle failed avatar fetch OWNER without exposing remote details."
  (when (slackit-avatar--owner-current-p owner)
    (puthash (slackit-avatar-fetch-resource-key owner)
             (float-time)
             slackit-avatar--failures))
  (slackit-avatar--retire-fetch owner))

(defun slackit-avatar--retry-due-p (resource-key)
  "Return non-nil when RESOURCE-KEY has no active failure backoff."
  (let ((failed-at (gethash resource-key slackit-avatar--failures)))
    (or (not (numberp failed-at))
        (>= (- (float-time) failed-at)
            (max 0 slackit-avatar-retry-delay)))))

(defun slackit-avatar--ensure-fetch (app user resource-key)
  "Start one deduplicated profile image fetch for APP USER RESOURCE-KEY."
  (unless (or (gethash resource-key slackit-avatar--fetches)
              (not (slackit-avatar--retry-due-p resource-key)))
    (let* ((url (slackit-avatar--profile-url user))
           (cache-base (slackit-avatar--cache-base resource-key))
           (owner (slackit-avatar-fetch-create
                   :app app
                   :generation (slackit-runtime-generation app)
                   :resource-key resource-key
                   :cache-base cache-base))
           (handle (appkit-register-handle
                    app 'slackit-avatar owner
                    #'slackit-avatar--cancel-fetch)))
      (setf (slackit-avatar-fetch-handle owner) handle)
      (puthash resource-key owner slackit-avatar--fetches)
      (condition-case nil
          (let ((transfer
                 (appkit-media-cache-image-resource-async
                  (appkit-media-resource-create
                   :url url :name "slack-avatar.png"
                   :mime-type "image/png")
                  cache-base
                  (apply-partially #'slackit-avatar--fetch-success owner)
                  (apply-partially #'slackit-avatar--fetch-failure owner))))
            (when (slackit-avatar--owner-current-p owner)
              (setf (slackit-avatar-fetch-transfer owner) transfer)))
        (error (slackit-avatar--fetch-failure owner nil)))
      owner)))

(defun slackit-avatar--normalize-pixel-size (pixel-size)
  "Return PIXEL-SIZE rounded to a positive integer, or nil."
  (when (and (numberp pixel-size) (> pixel-size 0))
    (max 1 (round pixel-size))))

(defun slackit-avatar--source-image (file pixel-size)
  "Decode FILE as a circular PIXEL-SIZE image, with a square fallback."
  (or (appkit-media-circular-image-from-file file pixel-size)
      (ignore-errors
        (create-image file nil nil
                      :width pixel-size
                      :height pixel-size
                      :ascent 'center))))

(defun slackit-avatar-cached-image (app user pixel-size)
  "Return APP USER's ready avatar at PIXEL-SIZE, or nil.

This lookup is deterministic and never discovers or acquires a source.  In
particular, a resource absent from `slackit-avatar--sources' causes no file
or network operation.  Call `slackit-avatar-ensure' separately to discover
an existing private disk entry or start its account-owned acquisition."
  (when-let* ((resource-key (slackit-avatar-resource-key app user))
              (size (slackit-avatar--normalize-pixel-size pixel-size))
              (source (gethash resource-key slackit-avatar--sources))
              (file (car source))
              (mtime (cadr source)))
    (let ((cache-key (list resource-key size mtime)))
      (or (gethash cache-key slackit-avatar--image-cache)
          (when-let* ((image (slackit-avatar--source-image file size)))
            (puthash cache-key image slackit-avatar--image-cache)
            image)))))
(defun slackit-avatar-cached-file (app user)
  "Return APP USER's validated local avatar file, or nil."
  (when-let* ((resource-key (slackit-avatar-resource-key app user))
              (source (gethash resource-key slackit-avatar--sources))
              (file (car source))
              ((file-regular-p file)))
    file))

(defun slackit-avatar-open (app user)
  "Open APP USER's cached avatar locally through Appkit."
  (let ((file (slackit-avatar-cached-file app user)))
    (unless file
      (user-error "slackit: avatar is not available locally"))
    (appkit-media-open-file file)))

(defun slackit-avatar-ensure (app user)
  "Ensure APP USER's avatar source is ready or being acquired.

Disk discovery, private-cache preparation, retry backoff, and deduplicated
network transfer all belong to this acquisition boundary."
  (when (appkit-app-live-p app)
    (when-let* ((resource-key (slackit-avatar-resource-key app user)))
      (or (and (gethash resource-key slackit-avatar--sources)
               resource-key)
          (gethash resource-key slackit-avatar--fetches)
          (progn
            (slackit-avatar--prepare-cache-directory)
            (let* ((cache-base (slackit-avatar--cache-base resource-key))
                   (file
                    (appkit-media-image-cache-existing-file cache-base)))
              (if file
                  (let ((source
                         (slackit-avatar--remember-source resource-key file)))
                    (if source
                        (progn
                          (unless
                              (memq system-type
                                    '(ms-dos windows-nt cygwin))
                            (set-file-modes file #o600))
                          (slackit-avatar--delete-stale-cache-files
                           resource-key file)
                          (slackit-runtime-publish-resource
                           app resource-key)
                          resource-key)
                      (slackit-avatar--forget-source resource-key)
                      (slackit-avatar--ensure-fetch
                       app user resource-key)))
                (slackit-avatar--forget-source resource-key)
                (slackit-avatar--ensure-fetch
                 app user resource-key))))))))

(defun slackit-avatar-clear-memory-cache ()
  "Clear every decoded Slack avatar descriptor and retry state."
  (clrhash slackit-avatar--image-cache)
  (clrhash slackit-avatar--failures))

(provide 'slackit-avatar)

;;; slackit-avatar.el ends here
