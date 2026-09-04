;;; slackit-root.el --- Slackit account directory -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Appkit directory projection of one exact account's joined conversations.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-directory)
(require 'appkit-surface)
(require 'appkit-projection)
(require 'slackit-customize)
(require 'slackit-runtime)
(require 'slackit-state)

(declare-function slackit-bootstrap-account "slackit" (app))
(declare-function slackit-room-open "slackit-room" (app conversation-id &optional select))
(declare-function slackit-runtime-ensure-user
                  "slackit-runtime" (app user-id))
(declare-function slackit-root-transient "slackit-transient" (&optional scope))

(defvar slackit-root-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map appkit-directory-mode-map)
    (define-key map (kbd "g") #'slackit-root-refresh)
    (define-key map (kbd "?") #'slackit-root-transient)
    map)
  "Keymap for `slackit-root-mode'.")

(define-derived-mode slackit-root-mode appkit-directory-mode "Slackit"
  "Directory of joined Slack conversations for one account.")

(defun slackit-root--status-label (state)
  "Return honest account status text for STATE."
  (let ((connection (slackit-account-state-connection-status state))
        (error-code (slackit-account-state-bootstrap-error state)))
    (cond
     (error-code (format "Bootstrap error: %s — press g to retry" error-code))
     ((not (slackit-state-bootstrap-ready-p state))
      (format
       "Loading directory (identity: %s, users: on-demand, conversations: %s); Realtime: %s"
       (if (slackit-account-state-bootstrap-identity-complete-p state)
           "complete" "loading")
       (if (slackit-account-state-bootstrap-conversations-complete-p state)
           "complete" "loading")
       connection))
     (t (format "Directory complete; Realtime: %s" connection)))))

(defun slackit-root--section-kind (conversation)
  "Return directory section kind for CONVERSATION."
  (cond
   ((alist-get 'is_im conversation) 'direct)
   ((alist-get 'is_mpim conversation) 'group-direct)
   (t 'channel)))

(defun slackit-root--conversation-unread-p (state conversation-id)
  "Return derived unread state for CONVERSATION-ID when facts are known."
  (let* ((read-ts (slackit-state-read-ts state conversation-id))
         (keys (slackit-state-top-level-keys state conversation-id))
         (latest (car (last keys))))
    (and read-ts latest (string< read-ts latest))))

(defun slackit-root--conversation-entry (state section conversation-id)
  "Build one directory item for CONVERSATION-ID under SECTION."
  (let* ((conversation (slackit-state-conversation state conversation-id))
         (name (slackit-state-conversation-name state conversation-id))
         (label (slackit-state-conversation-label state conversation-id))
         (unread-p (slackit-root--conversation-unread-p state conversation-id)))
    (appkit-directory-entry-create
     :key conversation-id
     :role 'item
     :section-key section
     :label label
     :trailing (and unread-p "  •")
     :face 'slackit-room-name
     :indent 2
     :item-p t
     :unread-p unread-p
     :payload conversation-id
     :stamp (list name unread-p conversation))))

(defun slackit-root--entries (state)
  "Project STATE into a flat Appkit directory entry sequence."
  (let ((buckets (list (cons 'direct nil)
                       (cons 'group-direct nil)
                       (cons 'channel nil)))
        entries)
    (dolist (conversation-id (slackit-state-joined-conversation-ids state))
      (let* ((conversation (slackit-state-conversation state conversation-id))
             (kind (slackit-root--section-kind conversation)))
        (push conversation-id (alist-get kind buckets))))
    (push (appkit-directory-entry-create
           :key '(status)
           :role 'note
           :label (slackit-root--status-label state)
           :face 'slackit-status
           :stamp (list (slackit-account-state-connection-status state)
                        (slackit-account-state-bootstrap-identity-complete-p state)
                        (slackit-account-state-bootstrap-users-complete-p state)
                        (slackit-account-state-bootstrap-conversations-complete-p state)
                        (slackit-account-state-bootstrap-error state)))
          entries)
    (dolist (spec '((direct "Direct messages")
                    (group-direct "Group direct messages")
                    (channel "Channels")))
      (pcase-let ((`(,kind ,label) spec))
        (push (appkit-directory-entry-create
               :key (list 'section kind)
               :role 'section
               :label label
               :face 'slackit-account-name
               :stamp label)
              entries)
        (dolist (conversation-id
                 (sort (copy-sequence (alist-get kind buckets))
                       (lambda (left right)
                         (string-lessp
                          (slackit-state-conversation-name state left)
                          (slackit-state-conversation-name state right)))))
          (push (slackit-root--conversation-entry
                 state kind conversation-id)
                entries))))
    (nreverse entries)))

(defun slackit-root--activate (_surface entry)
  "Open the exact conversation carried by directory ENTRY."
  (when-let* ((view (appkit-current-surface))
              (conversation-id (appkit-directory-entry-payload entry)))
    (slackit-room-open (appkit-surface-app view) conversation-id t)))

(defun slackit-root--setup (_view)
  "Configure the current root directory adapter."
  (appkit-directory-configure
   (appkit-directory-surface)
   :activate-function #'slackit-root--activate))

(defun slackit-root-open (app &optional select)
  "Open APP's account directory Surface."
  (let* ((id '(root))
         (surface (or (appkit-app-surface app id)
                      (appkit-open-generated-surface
                       (slackit-runtime--surface-type 'root #'slackit-root-mode)
                       :app app :identity id :input id
                       :buffer (slackit-runtime--host-buffer app id 'slackit-root-mode)
                       :buffer-name (format "*Slackit:%s*" (appkit-app-identity app))))))
    (slackit-runtime--remember-host surface)
    (with-current-buffer (appkit-surface-buffer surface)
      (slackit-runtime-render surface))
    (when select (pop-to-buffer (appkit-surface-buffer surface)))
    surface))

(defun slackit-root-refresh ()
  "Restart cursor-complete bootstrap for the current root account."
  (interactive)
  (let* ((view (appkit-current-surface))
         (app (and (appkit-surface-p view) (appkit-surface-app view))))
    (unless (and (appkit-surface-live-p view)
                 (slackit-runtime-account-p app)
                 (equal (appkit-surface-identity view) '(root)))
      (user-error "slackit: this command requires a live Slackit root view"))
    (slackit-bootstrap-account app)))

(provide 'slackit-root)

;;; slackit-root.el ends here
