;;; slackit-evil.el --- Native Evil bindings for Slackit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Slackit's ordinary maps remain its Emacs-state interface.  This optional
;; adapter installs deliberate application actions in Evil state maps.  Message
;; actions live on the timeline minor-mode map, which Appkit disables in the
;; composer; ordinary Evil editing remains available in the writable draft.

;;; Code:

(require 'appkit-evil)
(require 'slackit-customize)

(defgroup slackit-evil nil
  "Optional native Evil integration for Slackit."
  :group 'slackit
  :prefix "slackit-evil-")

(defcustom slackit-evil-enable-integration t
  "If non-nil, install Slackit's Evil bindings automatically."
  :type 'boolean
  :group 'slackit-evil)

(defcustom slackit-evil-initial-state 'normal
  "Initial Evil state used for Slackit application buffers.
When nil, leave Evil's initial-state selection untouched."
  :type '(choice (const :tag "Don't override" nil)
          (const :tag "Normal" normal)
          (const :tag "Motion" motion)
          (const :tag "Emacs" emacs)
          (symbol :tag "Custom state"))
  :group 'slackit-evil)

(defconst slackit-evil--application-modes
  '(slackit-root-mode slackit-room-mode slackit-thread-mode slackit-user-mode)
  "Major modes participating in Slackit's Evil integration.")

(defun slackit-evil--set-initial-states ()
  "Register `slackit-evil-initial-state' for Slackit modes."
  (appkit-evil-set-initial-states
   slackit-evil--application-modes slackit-evil-initial-state))

(defun slackit-evil--define-root-keys ()
  "Install root-directory modal bindings."
  (appkit-evil-define-readonly-keys 'slackit-root-mode-map)
  (appkit-evil-map
    (:map slackit-root-mode-map
     :nm
     "RET" #'appkit-directory-activate
     "<return>" #'appkit-directory-activate
     "g r" #'slackit-root-refresh
     "g j" #'appkit-directory-next-item
     "g k" #'appkit-directory-previous-item
     "TAB" #'appkit-directory-tab-dwim
     "<backtab>" #'appkit-directory-previous-item
     "?" #'slackit-root-transient)))

(defun slackit-evil--define-room-keys ()
  "Install room-wide and timeline-only modal bindings."
  ;; Appkit disables the message action map in the writable composer.
  (appkit-evil-map
    (:map slackit-room-mode-map
     :nm
     "g +" #'slackit-room-load-older
     "?" #'slackit-room-transient
     "Z a" #'slackit-compose-attach
     "Z f" #'slackit-compose-attach-file
     "Z v" #'slackit-compose-attach-clipboard-image
     :i
     "RET" #'newline
     "<return>" #'newline)
    (:map slackit-room-timeline-mode-map
     :nm
     "q" #'quit-window
     "RET" #'slackit-actions-activate
     "<return>" #'slackit-actions-activate
     "g r" #'slackit-actions-open-thread
     "r" #'slackit-actions-open-thread
     "R" #'undefined
     "i" #'slackit-actions-edit
     "!" #'slackit-actions-react
     "Z y" #'slackit-actions-copy-text
     "?" #'slackit-actions-transient
     "D" #'slackit-actions-delete
     "d d" #'slackit-actions-delete)))

(defun slackit-evil--define-user-keys ()
  "Install user-profile modal bindings."
  (appkit-evil-define-readonly-keys 'slackit-user-mode-map)
  (appkit-evil-map
    (:map slackit-user-mode-map
     :nm
     "m" #'slackit-user-open-chat
     "a" #'slackit-user-open-avatar
     "Z y" #'slackit-user-copy-mention
     "g I" #'slackit-user-copy-id
     "Y" #'undefined
     "TAB" #'forward-button
     "<backtab>" #'slackit-user-button-backward
     "?" #'slackit-user-transient
     "q" #'quit-window)))

(defun slackit-evil--refresh-live-buffers ()
  "Refresh Evil projections in existing Slackit application buffers."
  (appkit-evil-normalize-buffers slackit-evil--application-modes))

;;;###autoload
(defun slackit-evil-setup ()
  "Install Slackit's native Evil integration.
Safe to call multiple times."
  (interactive)
  (when (and (featurep 'evil) slackit-evil-enable-integration)
    (slackit-evil--set-initial-states)
    (slackit-evil--define-root-keys)
    (slackit-evil--define-room-keys)
    (slackit-evil--define-user-keys)
    (slackit-evil--refresh-live-buffers)))

(with-eval-after-load 'evil
  (slackit-evil-setup))

(with-eval-after-load 'evil-snipe
  (dolist (mode slackit-evil--application-modes)
    (add-hook (intern (format "%s-hook" mode)) #'turn-off-evil-snipe-mode)
    (add-hook (intern (format "%s-hook" mode)) #'turn-off-evil-snipe-override-mode)))

(provide 'slackit-evil)

;;; slackit-evil.el ends here
