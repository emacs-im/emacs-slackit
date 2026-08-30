;;; slackit-emoji.el --- Slack emoji catalog and display -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Render every standard Slack shortname from the bundled iamcal-derived table.
;; Account custom emoji aliases resolve through canonical state; image-backed
;; custom emoji use credential-free media resources.  Canonical message and
;; reaction names are never rewritten.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'appkit-chat-avatar)
(require 'appkit-chat-completion)
(require 'slackit-api)
(require 'slackit-emoji-data)
(require 'slackit-media)
(require 'slackit-state)

(defconst slackit-emoji--token-regexp
  ":\\([+[:alnum:]_'-]+\\(?:::skin-tone-[2-6]\\)?\\):"
  "Regexp matching one complete Slack emoji token.")

(defconst slackit-emoji--max-alias-hops 10
  "Maximum custom emoji alias links followed during display.")

(defvar slackit-emoji--glyph-table nil
  "Private immutable cache of Slack standard shortnames to Unicode glyphs.")

(defvar slackit-emoji--standard-candidates nil
  "Cached completion candidates for every standard Slack shortname.")

(defun slackit-emoji--glyph-table ()
  "Return the lazily initialized standard Slack glyph table."
  (or slackit-emoji--glyph-table
      (let ((table (make-hash-table :test #'equal)))
        (mapc (lambda (entry)
                (puthash (car entry) (cdr entry) table))
              slackit-emoji-standard-data)
        (setq slackit-emoji--glyph-table table))))

(defun slackit-emoji--completion-candidate (name display group)
  "Return a Slack emoji completion candidate for NAME, DISPLAY, and GROUP."
  (let ((token (format ":%s:" name)))
    (appkit-chat-completion-candidate-create
     :label token
     :insert token
     :prefix (concat (or display "□") " ")
     :search-terms (list name token)
     :group group
     :value (list :kind 'slack-emoji :name name))))

(defun slackit-emoji--standard-candidates ()
  "Return cached completion candidates for the complete Slack standard table."
  (or slackit-emoji--standard-candidates
      (setq slackit-emoji--standard-candidates
            (mapcar
             (lambda (entry)
               (slackit-emoji--completion-candidate
                (car entry) (cdr entry) "Slack standard"))
             (append slackit-emoji-standard-data nil)))))

(defun slackit-emoji-completion-candidates (app)
  "Return complete standard and account custom emoji candidates for APP."
  (let* ((customs
          (and (appkit-app-live-p app)
               (slackit-account-state-emojis
                (slackit-runtime-state app))))
         custom-candidates)
    (when (hash-table-p customs)
      (maphash
       (lambda (name _value)
         (push
          (slackit-emoji--completion-candidate
           name (slackit-emoji--resolve app name) "Workspace custom")
          custom-candidates))
       customs))
    (append
     (sort custom-candidates
           (lambda (left right)
             (string-lessp
              (appkit-chat-completion-candidate-label left)
              (appkit-chat-completion-candidate-label right))))
     (if (or (not (hash-table-p customs)) (= 0 (hash-table-count customs)))
         (slackit-emoji--standard-candidates)
       (seq-filter
        (lambda (candidate)
          (not
           (gethash
            (plist-get
             (appkit-chat-completion-candidate-value candidate) :name)
            customs)))
        (slackit-emoji--standard-candidates))))))

(defun slackit-emoji-resource-key (app)
  "Return APP's opaque custom emoji catalog resource key."
  (list :slackit-emoji-catalog
        (secure-hash 'sha256 (prin1-to-string (appkit-app-id app)))))

(defun slackit-emoji--custom-value (app name)
  "Return APP custom emoji value for NAME, or nil."
  (and (appkit-app-live-p app)
       (slackit-state-emoji (slackit-runtime-state app) name)))

(defun slackit-emoji--custom-url (app name &optional seen hops)
  "Resolve APP custom emoji NAME to an image URL, or nil.

SEEN and HOPS guard malformed alias cycles."
  (when (< (or hops 0) slackit-emoji--max-alias-hops)
    (let ((value (slackit-emoji--custom-value app name)))
      (cond
       ((not (stringp value)) nil)
       ((string-prefix-p "alias:" value)
        (let ((target (substring value (length "alias:"))))
          (unless (member target seen)
            (slackit-emoji--custom-url
             app target (cons name seen) (1+ (or hops 0))))))
       (t value)))))

(defun slackit-emoji--image-resource-key (app name url)
  "Return opaque APP custom emoji resource key for NAME and URL."
  (slackit-media-resource-key app 'emoji (list name url)))

(defun slackit-emoji--inline-image (app name url)
  "Return one line-sized image display string for APP NAME at URL, or nil."
  (let* ((key (slackit-emoji--image-resource-key app name url))
         (image (slackit-media-cached-image key))
         (size (and image (appkit-chat-avatar-line-pixel-height)))
         (resized
          (and image size
               (appkit-chat-avatar-resize-image image size))))
    (when resized
      (propertize
       " "
       'display resized
       'help-echo (format ":%s:" name)
       'rear-nonsticky '(display help-echo)))))

(defun slackit-emoji--standard-glyph (name)
  "Return the Unicode glyph for Slack standard or skin-tone NAME."
  (or (gethash name (slackit-emoji--glyph-table))
      (when (string-match
             "\\`\\(.+\\)::\\(skin-tone-[2-6]\\)\\'" name)
        (when-let* ((base
                     (gethash
                      (match-string 1 name)
                      (slackit-emoji--glyph-table)))
                    (modifier
                     (gethash
                      (match-string 2 name)
                      (slackit-emoji--glyph-table))))
          (concat base modifier)))))

(defun slackit-emoji--resolve (app name &optional seen hops)
  "Resolve APP Slack shortname NAME to a Unicode or image display string."
  (let ((custom (slackit-emoji--custom-value app name)))
    (cond
     ((and (stringp custom) (string-prefix-p "alias:" custom))
      (let ((target (substring custom (length "alias:"))))
        (unless (or (member target seen)
                    (>= (or hops 0) slackit-emoji--max-alias-hops))
          (slackit-emoji--resolve
           app target (cons name seen) (1+ (or hops 0))))))
     ((stringp custom)
      (slackit-emoji--inline-image app name custom))
     (t (slackit-emoji--standard-glyph name)))))

(defun slackit-emoji-display-string (app name)
  "Return a detached display string for APP Slack shortname NAME, or nil."
  (when-let* ((glyph (and (stringp name)
                          (slackit-emoji--resolve app name))))
    (copy-sequence glyph)))

(defun slackit-emoji-substitute (app text)
  "Return display TEXT with recognized APP Slack emoji tokens replaced once.

Unknown tokens are preserved byte-for-byte, and TEXT itself is never modified."
  (replace-regexp-in-string
   slackit-emoji--token-regexp
   (lambda (token)
     (save-match-data
       (or (slackit-emoji--resolve app (substring token 1 -1))
           token)))
   text t t))

(defun slackit-emoji--message-names (message)
  "Return deduplicated Slack emoji names referenced by MESSAGE."
  (let ((text (or (alist-get 'text message) ""))
        names
        (position 0))
    (while (string-match slackit-emoji--token-regexp text position)
      (push (substring (match-string 0 text) 1 -1) names)
      (setq position (match-end 0)))
    (dolist (reaction (alist-get 'reactions message))
      (when-let* ((name (alist-get 'name reaction)))
        (push name names)))
    (delete-dups names)))

(defun slackit-emoji-message-resource-keys (app message)
  "Return catalog and custom image resources used by APP MESSAGE."
  (let ((keys (list (slackit-emoji-resource-key app))))
    (dolist (name (slackit-emoji--message-names message))
      (when-let* ((url (slackit-emoji--custom-url app name)))
        (push (slackit-emoji--image-resource-key app name url) keys)))
    (delete-dups keys)))

(defun slackit-emoji-ensure-message (app message)
  "Start deduplicated custom emoji image acquisition for APP MESSAGE."
  (when (appkit-app-live-p app)
    (dolist (name (slackit-emoji--message-names message))
      (when-let* ((url (slackit-emoji--custom-url app name)))
        (slackit-media-ensure-public-image
         app (slackit-emoji--image-resource-key app name url) url))))
  nil)

(defun slackit-emoji--catalog-success (app operation body)
  "Settle APP custom emoji OPERATION from emoji.list BODY."
  (when (slackit-runtime-operation-current-p app operation)
    (slackit-state-set-emojis
     (slackit-runtime-state app)
     (or (alist-get 'emoji body) nil))
    (slackit-runtime-operation-end app operation)
    (slackit-runtime-publish-resource
     app (slackit-emoji-resource-key app))))

(defun slackit-emoji--catalog-failure (app operation _error)
  "Settle failed APP custom emoji OPERATION without blocking startup."
  (slackit-runtime-operation-end app operation))

(defun slackit-emoji-load-catalog (app)
  "Load APP's custom Slack emoji catalog once per pending operation."
  (let* ((key '(emoji-catalog))
         (pending (gethash key (appkit-app-request-table app))))
    (if (slackit-runtime-operation-current-p app pending)
        pending
      (let ((operation (slackit-runtime-operation-begin app key)))
        (slackit-api-emoji-list
         app
         :on-success
         (apply-partially #'slackit-emoji--catalog-success app operation)
         :on-error
         (apply-partially #'slackit-emoji--catalog-failure app operation))
        operation))))

(provide 'slackit-emoji)

;;; slackit-emoji.el ends here
