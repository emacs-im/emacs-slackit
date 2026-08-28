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
(require 'slackit-normalize)
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
  "Decoded image records keyed by opaque avatar resource identity.")

(defvar slackit-avatar--fetches (make-hash-table :test #'equal)
  "Current account-owned avatar fetches keyed by opaque resource identity.")

(defvar slackit-avatar--failures (make-hash-table :test #'equal)
  "Last failure time for each opaque avatar resource identity.")

(defvar slackit-avatar--prepared-cache-directory nil
  "Expanded avatar cache directory already prepared in this Emacs session.")

(defun slackit-avatar--profile-url (user)
  "Return USER's preferred Slack profile image URL, or nil."
  (let ((profile (slackit-normalize-get user 'profile)))
    (seq-some
     (lambda (key)
       (let ((value (slackit-normalize-get profile key)))
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
  (let ((user-id (slackit-normalize-get user 'id))
        (url (slackit-avatar--profile-url user)))
    (when (and (appkit-app-p app)
               (stringp user-id)
               (not (string-empty-p user-id))
               (slackit-avatar--valid-url-p url))
      (list :avatar
            (format "%s" (appkit-app-id app))
            user-id
            (secure-hash 'sha256 url)))))

(defun slackit-avatar--cache-base (resource-key)
  "Return private cache base path for opaque RESOURCE-KEY."
  (expand-file-name
   (secure-hash 'sha256 (prin1-to-string resource-key))
   slackit-avatar-cache-directory))

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
  (when (slackit-avatar--owner-current-p owner)
    (unless (memq system-type '(ms-dos windows-nt cygwin))
      (set-file-modes file #o600))
    (remhash (slackit-avatar-fetch-resource-key owner)
             slackit-avatar--failures)
    (let ((app (slackit-avatar-fetch-app owner))
          (resource-key (slackit-avatar-fetch-resource-key owner)))
      (slackit-avatar--retire-fetch owner)
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

(defun slackit-avatar--cached-image (resource-key cache-base)
  "Return decoded cached image for RESOURCE-KEY below CACHE-BASE."
  (let* ((record (gethash resource-key slackit-avatar--image-cache))
         (record-file (car-safe record))
         (record-mtime (cadr record))
         (record-image (caddr record))
         (record-attributes
          (and record-file (file-attributes record-file))))
    (if (and record-attributes
             (equal record-mtime
                    (file-attribute-modification-time record-attributes)))
        record-image
      (when-let* ((file
                   (appkit-media-image-cache-existing-file cache-base))
                  (attributes (file-attributes file))
                  (mtime (file-attribute-modification-time attributes))
                  (image
                   (ignore-errors
                     (create-image file nil nil :ascent 'center))))
        (puthash resource-key (list file mtime image)
                 slackit-avatar--image-cache)
        image))))

(defun slackit-avatar-image (app user)
  "Return cached profile image for APP USER and start acquisition if absent."
  (when (and slackit-show-avatars
             (display-graphic-p)
             (appkit-app-live-p app))
    (when-let* ((resource-key (slackit-avatar-resource-key app user)))
      (slackit-avatar--prepare-cache-directory)
      (let* ((cache-base (slackit-avatar--cache-base resource-key))
             (image (slackit-avatar--cached-image resource-key cache-base)))
        (unless image
          (slackit-avatar--ensure-fetch app user resource-key))
        image))))

(defun slackit-avatar-clear-memory-cache ()
  "Clear decoded Slack avatar images and retry state for this Emacs session."
  (clrhash slackit-avatar--image-cache)
  (clrhash slackit-avatar--failures))

(provide 'slackit-avatar)

;;; slackit-avatar.el ends here
