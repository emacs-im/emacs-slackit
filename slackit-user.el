;;; slackit-user.el --- Account-owned Slack user profiles -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; One exact `(user USER-ID)' Appkit view per account.  Profile facts remain in
;; canonical account state; the view owns only identity, transient presentation
;; errors, and actions whose callbacks are fenced by account/view generations.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-chat-avatar)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'appkit-position)
(require 'appkit-ui)
(require 'appkit-view)
(require 'slackit-api)
(require 'slackit-avatar)
(require 'slackit-emoji)
(require 'slackit-decode)
(require 'slackit-render)
(require 'slackit-room)
(require 'slackit-runtime)
(require 'slackit-state)

(declare-function slackit-user-transient
                  "slackit-transient" (&optional scope))

(defface slackit-user-card-title
  '((t :inherit bold :height 1.15))
  "Face for the primary name on a Slackit user page."
  :group 'slackit)

(defvar-local slackit-user--user-id nil
  "Opaque Slack user ID owned by the current profile view.")

(defvar-local slackit-user--profile-error-code nil
  "Last redacted profile fetch error code for this view.")

(defvar-local slackit-user--dm-error-code nil
  "Last redacted direct-message error code for this view.")

(defun slackit-user--present-string (value)
  "Return non-empty string VALUE, or nil."
  (and (stringp value) (not (string-blank-p value)) value))

(defun slackit-user--view-id (user-id)
  "Return the stable Appkit identity for USER-ID."
  (list 'user user-id))

(defun slackit-user--view-current-p (view &optional user-id)
  "Return non-nil when VIEW owns exact USER-ID profile context."
  (let* ((id (and (appkit-view-p view) (appkit-view-id view)))
         (expected-user-id (or user-id slackit-user--user-id))
         (app (and (appkit-view-p view) (appkit-view-app view))))
    (and expected-user-id
         (appkit-view-live-p view)
         (eq (appkit-app-kind app) 'slackit-account)
         (equal id (slackit-user--view-id expected-user-id))
         (eq view (appkit-view-for-id app id))
         (with-current-buffer (appkit-view-buffer view)
           (and (derived-mode-p 'slackit-user-mode)
                (eq view (appkit-current-view))
                (equal slackit-user--user-id expected-user-id))))))

(defun slackit-user--current-view ()
  "Return the exact live user view attached to the current buffer."
  (let ((view (appkit-current-view)))
    (or (and (slackit-user--view-current-p view) view)
        (user-error "slackit: this command requires a live user view"))))

(defun slackit-user--state-user (view)
  "Return VIEW's current canonical Slack user, or nil."
  (slackit-state-user
   (slackit-runtime-state (appkit-view-app view))
   slackit-user--user-id))

(defun slackit-user--display-name (user)
  "Return USER's best stable display name."
  (let ((profile (alist-get 'profile user)))
    (or (slackit-user--present-string
         (alist-get 'display_name profile))
        (slackit-user--present-string
         (alist-get 'real_name profile))
        (slackit-user--present-string
         (alist-get 'real_name user))
        (slackit-user--present-string
         (alist-get 'name user))
        slackit-user--user-id
        "Slack user")))

(defun slackit-user--username (user)
  "Return USER's @username label, or nil."
  (when-let* ((name (slackit-user--present-string
                     (alist-get 'name user))))
    (concat "@" name)))

(defun slackit-user--avatar-placeholder (user)
  "Return a compact text avatar for USER."
  (let* ((parts (split-string (slackit-user--display-name user)
                              "[^[:alnum:]]+" t))
         (first (and parts (substring (car parts) 0 1)))
         (second (and (> (length parts) 1)
                      (substring (cadr parts) 0 1))))
    (format "[%s]" (upcase (concat (or first "?") (or second ""))))))

(defun slackit-user--status-string (app profile)
  "Return PROFILE's display-only Slack status string for APP."
  (let* ((emoji (slackit-user--present-string
                 (alist-get 'status_emoji profile)))
         (text (slackit-user--present-string
                (alist-get 'status_text profile)))
         (rendered-emoji
          (and emoji (slackit-emoji-substitute app emoji))))
    (string-join (delq nil (list rendered-emoji
                                 (and text
                                      (slackit-render-decode-entities text))))
                 " ")))

(defun slackit-user--timezone-label (user)
  "Return USER's timezone label with a stable UTC offset."
  (let ((label (slackit-user--present-string
                (alist-get 'tz_label user)))
        (offset (alist-get 'tz_offset user)))
    (string-join
     (delq nil
           (list label
                 (when (numberp offset)
                   (let* ((sign (if (< offset 0) "-" "+"))
                          (absolute (abs offset))
                          (hours (/ absolute 3600))
                          (minutes (/ (% absolute 3600) 60)))
                     (format "UTC%s%02d:%02d" sign hours minutes)))))
     " · ")))

(defun slackit-user--role-label (user)
  "Return USER's workspace role labels, or nil."
  (let (roles)
    (dolist (spec '((is_primary_owner . "Primary owner")
                    (is_owner . "Owner")
                    (is_admin . "Admin")
                    (is_ultra_restricted . "Single-channel guest")
                    (is_restricted . "Guest")
                    (is_bot . "Bot")
                    (is_app_user . "App user")
                    (deleted . "Deactivated")))
      (when (eq t (alist-get (car spec) user))
        (push (cdr spec) roles)))
    (string-join (nreverse roles) " · ")))

(defun slackit-user--insert-field (label value &optional face)
  "Insert profile LABEL and VALUE when VALUE is present."
  (when (or (and (stringp value) (not (string-blank-p value)))
            (numberp value))
    (insert (propertize (format "%-16s" (concat label ":")) 'face 'bold))
    (let ((start (point)))
      (insert (format "%s" value))
      (when face
        (add-face-text-property start (point) face 'append)))
    (insert "\n")))

(defun slackit-user--insert-custom-fields (app profile)
  "Insert non-empty custom PROFILE fields for APP."
  (let (fields)
    (dolist (entry (alist-get 'fields profile))
      (let* ((field (cdr-safe entry))
             (label (slackit-user--present-string
                     (alist-get 'label field)))
             (value (slackit-user--present-string
                     (alist-get 'value field))))
        (when (and label value)
          (push (cons label (slackit-emoji-substitute app value)) fields))))
    (dolist (field (sort fields (lambda (left right)
                                  (string-lessp (car left) (car right)))))
      (slackit-user--insert-field (car field) (cdr field)))))

(defun slackit-user--insert-action-buttons ()
  "Insert actions for the exact current user view."
  (insert "  ")
  (appkit-ui-insert-action-button
   (if (slackit-user--dm-pending-p
        (appkit-view-app (slackit-user--current-view))
        slackit-user--user-id)
       " Opening DM… "
     " Message ")
   #'slackit-user-open-chat
   :help-echo "Open a direct message (m)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " Open avatar " #'slackit-user-open-avatar
   :help-echo "Open the cached avatar (a)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " Copy mention " #'slackit-user-copy-mention
   :help-echo "Copy an exact Slack mention (w)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " Copy ID " #'slackit-user-copy-id
   :help-echo "Copy the Slack member ID (Y)")
  (insert "\n"))

(defun slackit-user-render ()
  "Render the current canonical Slack user profile."
  (interactive)
  (let* ((view (slackit-user--current-view))
         (app (appkit-view-app view))
         (user (slackit-user--state-user view))
         (profile (and user (alist-get 'profile user))))
    (appkit-position-render-preserving
     (lambda ()
       (let ((inhibit-read-only t))
         (erase-buffer)
         (setq-local header-line-format
                     '(:eval (slackit-user--header-line)))
         (if (null user)
             (if (slackit-runtime-user-pending-p app slackit-user--user-id)
                 (appkit-view-insert-note-line "Loading user profile…")
               (appkit-view-insert-note-line
                "User profile is unavailable; press g to retry."
                :face (and slackit-user--profile-error-code 'error)))
           (let* ((pixel-size (appkit-chat-avatar-two-line-pixel-size))
                  (image (slackit-avatar-cached-image app user pixel-size))
                  (prefixes
                   (appkit-chat-avatar-prefixes
                    image (slackit-user--avatar-placeholder user)
                    :pixel-size pixel-size :resize nil))
                  (header-prefix (plist-get prefixes :header))
                  (status-prefix (plist-get prefixes :first-body))
                  (avatar-start (point)))
             (insert header-prefix)
             (appkit-ui-add-action
              avatar-start (point) #'slackit-user-open-avatar
              :help-echo "Open avatar")
             (insert (propertize (slackit-user--display-name user)
                                 'face 'slackit-user-card-title))
             (when-let* ((username (slackit-user--username user)))
               (insert (propertize (concat " · " username) 'face 'shadow)))
             (insert "\n" status-prefix)
             (when-let* ((status (slackit-user--status-string app profile))
                         ((not (string-empty-p status))))
               (insert (propertize status 'face 'shadow)))
             (insert "\n\n"))
           (slackit-user--insert-action-buttons)
           (when (slackit-runtime-user-pending-p app slackit-user--user-id)
             (appkit-view-insert-note-line
              "Refreshing user profile…" :face 'shadow))
           (when slackit-user--profile-error-code
             (appkit-view-insert-note-line
              (format "Profile refresh failed: %s"
                      slackit-user--profile-error-code)
              :face 'error))
           (when slackit-user--dm-error-code
             (appkit-view-insert-note-line
              (format "Unable to open direct message: %s"
                      slackit-user--dm-error-code)
              :face 'error))
           (appkit-view-insert-note-line
            "g refresh · m message · a avatar · w copy mention · Y copy ID · q quit"
            :face 'shadow)
           (insert "\n")
           (appkit-view-insert-heading-line "Profile" :face 'bold)
           (slackit-user--insert-field
            "Display name" (alist-get 'display_name profile))
           (slackit-user--insert-field
            "Full name"
            (or (alist-get 'real_name profile)
                (alist-get 'real_name user)))
           (slackit-user--insert-field "Username" (slackit-user--username user))
           (slackit-user--insert-field
            "Title" (alist-get 'title profile))
           (slackit-user--insert-field
            "Pronouns" (alist-get 'pronouns profile))
           (slackit-user--insert-field
            "Time zone" (slackit-user--timezone-label user))
           (slackit-user--insert-custom-fields app profile)
           (when (or (slackit-user--present-string
                      (alist-get 'email profile))
                     (slackit-user--present-string
                      (alist-get 'phone profile)))
             (insert "\n")
             (appkit-view-insert-heading-line "Contact" :face 'bold)
             (slackit-user--insert-field
              "Email" (alist-get 'email profile))
             (slackit-user--insert-field
              "Phone" (alist-get 'phone profile)))
           (let ((roles (slackit-user--role-label user)))
             (when (or (not (string-empty-p roles))
                       slackit-user--user-id)
               (insert "\n")
               (appkit-view-insert-heading-line "Workspace" :face 'bold)
               (slackit-user--insert-field "Member ID" slackit-user--user-id)
               (slackit-user--insert-field "Role" roles)))
           (insert "\n")))
       (add-text-properties
        (point-min) (point-max)
        (list 'slackit-user-profile-key
              (slackit-user--view-id slackit-user--user-id)
              'rear-nonsticky '(slackit-user-profile-key)))
       (goto-char (point-min)))
     :anchor-property 'slackit-user-profile-key
     :preserve-window-start t)))

(defun slackit-user--header-line ()
  "Return the current user view's dynamic header line."
  (let* ((view (appkit-current-view))
         (user (and (slackit-user--view-current-p view)
                    (slackit-user--state-user view))))
    (format " Slackit user · %s (%s)%s"
            (if user (slackit-user--display-name user) "loading")
            (or slackit-user--user-id "unknown")
            (if (and view
                     (slackit-runtime-user-pending-p
                      (appkit-view-app view) slackit-user--user-id))
                " · loading"
              ""))))

(defun slackit-user--accept-events (events)
  "Apply presentation EVENTS owned by the current user profile."
  (dolist (event events)
    (when (equal (plist-get event :user-id) slackit-user--user-id)
      (pcase (plist-get event :kind)
        ('user (setq slackit-user--profile-error-code nil))
        ('user-profile-error
         (setq slackit-user--profile-error-code
               (or (plist-get event :code) "request_failed")))
        ('user-dm-error
         (setq slackit-user--dm-error-code
               (or (plist-get event :code) "request_failed")))))))

(defun slackit-user--sync (view invalidations events)
  "Synchronize exact user VIEW from INVALIDATIONS and EVENTS."
  (when (slackit-user--view-current-p view (cadr (appkit-view-id view)))
    (with-current-buffer (appkit-view-buffer view)
      (slackit-user--accept-events events)
      (when (appkit-invalidations-affect-p invalidations '(profile))
        (when-let* ((user (slackit-user--state-user view)))
          (slackit-avatar-ensure (appkit-view-app view) user)
          (when-let* ((status-emoji
                       (slackit-user--present-string
                        (alist-get 'status_emoji (alist-get 'profile user)))))
            (slackit-emoji-ensure-message
             (appkit-view-app view) `((text . ,status-emoji)))))
        (appkit-with-content-update view
          (slackit-user-render))))))

(defun slackit-user-refresh ()
  "Refresh the exact current Slack user profile."
  (interactive)
  (let* ((view (slackit-user--current-view))
         (app (appkit-view-app view)))
    (setq slackit-user--profile-error-code nil)
    (slackit-runtime-ensure-user
     app slackit-user--user-id :force t :view view)
    (appkit-request-sync view :part 'profile)
    (appkit-sync-invalidations view)))

(defun slackit-user--dm-pending-p (app user-id)
  "Return APP's current direct-message operation for USER-ID, or nil."
  (let ((operation
         (and (appkit-app-live-p app)
              (gethash (list 'open-dm user-id)
                       (appkit-app-request-table app)))))
    (and (slackit-runtime-operation-current-p app operation) operation)))

(defun slackit-user--publish-dm-error (_app operation error-data)
  "Publish redacted ERROR-DATA for OPERATION to its exact user view."
  (let ((view (slackit-operation-view operation))
        (user-id (slackit-operation-payload operation)))
    (when (slackit-user--view-current-p view user-id)
      (appkit-view-enqueue-event
       view (list :kind 'user-dm-error
                  :user-id user-id
                  :code (format "%s" (or (plist-get error-data :code)
                                         "request_failed"))))
      (appkit-request-sync view :part 'profile))))

(defun slackit-user--dm-failure (app operation error-data)
  "Settle failed direct-message OPERATION for APP."
  (when (slackit-runtime-operation-current-p app operation)
    (slackit-user--publish-dm-error app operation error-data)
    (slackit-runtime-operation-end app operation)))

(defun slackit-user--dm-success (app operation body)
  "Reduce successful direct-message BODY for current APP OPERATION."
  (when (slackit-runtime-operation-current-p app operation)
    (let* ((user-id (slackit-operation-payload operation))
           (view (slackit-operation-view operation)))
      (if (not (slackit-user--view-current-p view user-id))
          (slackit-runtime-operation-end app operation)
        (let* ((channel (alist-get 'channel body))
               (channel-id (alist-get 'id channel))
               (returned-user (alist-get 'user channel)))
          (if (and (stringp channel-id)
                   (not (string-empty-p channel-id))
                   (or (null returned-user)
                       (equal returned-user user-id)))
              (let* ((state (slackit-runtime-state app))
                     (rest (copy-tree channel))
                     (_
                      (dolist (key '(id user is_im is_member))
                        (setq rest (assq-delete-all key rest))))
                     (normalized
                      (slackit-decode-conversation
                       (append `((id . ,channel-id)
                                 (user . ,user-id)
                                 (is_im . t)
                                 (is_member . t))
                               rest))))
                (slackit-runtime-operation-end app operation)
                (slackit-state-put-conversation state normalized)
                (slackit-runtime-publish-changes
                 app (list (list :kind 'conversation
                                 :conversation-id channel-id)))
                (slackit-room-open app channel-id t))
            (slackit-user--dm-failure
             app operation '(:code "invalid_response"))))))))

(defun slackit-user-open-chat ()
  "Open or create a direct-message room with the current profile user."
  (interactive)
  (let* ((view (slackit-user--current-view))
         (app (appkit-view-app view))
         (user-id slackit-user--user-id)
         (state (slackit-runtime-state app))
         (existing (slackit-state-im-conversation-id state user-id)))
    (cond
     (existing
      (slackit-room-open app existing t))
     ((slackit-user--dm-pending-p app user-id)
      (user-error "slackit: direct message is already opening"))
     (t
      (setq slackit-user--dm-error-code nil)
      (let ((operation
             (slackit-runtime-operation-begin
              app (list 'open-dm user-id) view nil user-id)))
        (appkit-request-sync view :part 'profile)
        (slackit-api-conversations-open
         app user-id
         :owner view
         :on-success
         (apply-partially #'slackit-user--dm-success app operation)
         :on-error
         (apply-partially #'slackit-user--dm-failure app operation))
        (appkit-sync-invalidations view))))))

(defun slackit-user-open-avatar ()
  "Open the current user's avatar from its local account cache."
  (interactive)
  (let* ((view (slackit-user--current-view))
         (app (appkit-view-app view))
         (user (or (slackit-user--state-user view)
                   (user-error "slackit: user profile is unavailable"))))
    (slackit-avatar-open app user)))

(defun slackit-user-copy-mention ()
  "Copy the current user's exact Slack mention syntax."
  (interactive)
  (slackit-user--current-view)
  (kill-new (format "<@%s>" slackit-user--user-id))
  (message "slackit: copied exact user mention"))

(defun slackit-user-copy-id ()
  "Copy the current profile's opaque Slack member ID."
  (interactive)
  (slackit-user--current-view)
  (kill-new slackit-user--user-id)
  (message "slackit: copied member ID %s" slackit-user--user-id))

(defun slackit-user-button-backward ()
  "Move point to the previous user-page button."
  (interactive)
  (forward-button -1))

(defvar slackit-user-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'slackit-user-refresh)
    (define-key map (kbd "m") #'slackit-user-open-chat)
    (define-key map (kbd "a") #'slackit-user-open-avatar)
    (define-key map (kbd "w") #'slackit-user-copy-mention)
    (define-key map (kbd "Y") #'slackit-user-copy-id)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'slackit-user-button-backward)
    (define-key map (kbd "?") #'slackit-user-transient)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `slackit-user-mode'.")

(define-derived-mode slackit-user-mode special-mode "Slackit-User"
  "Major mode for one exact account-owned Slack user profile."
  (setq-local truncate-lines nil)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (setq-local header-line-format '(:eval (slackit-user--header-line)))
  (buffer-disable-undo)
  (setq-local buffer-undo-list t))
(defun slackit-user--release-view-operations (app view)
  "Retire account operations whose exact lifecycle owner is VIEW."
  (let ((table (appkit-app-request-table app))
        keys)
    (maphash
     (lambda (key operation)
       (when (and (slackit-operation-p operation)
                  (eq (slackit-operation-view operation) view))
         (push key keys)))
     table)
    (dolist (key keys)
      (remhash key table))))


(defun slackit-user--setup-view (view)
  "Bind exact identity and lifecycle state for user profile VIEW."
  (let ((user-id (cadr (appkit-view-id view))))
    (with-current-buffer (appkit-view-buffer view)
      (setq slackit-user--user-id user-id
            slackit-user--profile-error-code nil
            slackit-user--dm-error-code nil))
    (appkit-register-handle
     view 'function
     (apply-partially
      #'slackit-user--release-view-operations
      (appkit-view-app view) view))))

;;;###autoload
(defun slackit-user-open (app user-id &optional select)
  "Open APP's exact USER-ID profile and optionally SELECT it."
  (unless (and (appkit-app-live-p app)
               (eq (appkit-app-kind app) 'slackit-account))
    (user-error "slackit: user profile requires a live account"))
  (unless (and (stringp user-id)
               (not (string-empty-p user-id))
               (not (string-prefix-p "B" user-id)))
    (user-error "slackit: user profile requires an exact Slack user ID"))
  (let* ((view-id (slackit-user--view-id user-id))
         (existing (appkit-view-for-id app view-id))
         (view
          (appkit-open-view
           :app app
           :id view-id
           :mode 'slackit-user-mode
           :buffer-name (format "*Slackit:%s:user %s*"
                                (appkit-app-id app) user-id)
           :state user-id
           :sync-function #'slackit-user--sync
           :parts '(profile)
           :setup #'slackit-user--setup-view
           :select select)))
    (with-current-buffer (appkit-view-buffer view)
      (unless existing
        (slackit-runtime-ensure-user app user-id :force t :view view))
      (appkit-invalidate view :structure t)
      (appkit-sync-invalidations view))
    view))

(provide 'slackit-user)

;;; slackit-user.el ends here
