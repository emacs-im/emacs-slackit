;;; slackit-evil.el --- Native Evil bindings for Slackit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Slackit's ordinary maps remain its Emacs-state interface.  This optional
;; adapter installs only deliberate application actions in Evil state maps, so
;; native prefixes, motions, and operators remain available.  Message actions
;; live on the timeline minor-mode map, which Appkit disables in the composer.

;;; Code:

(require 'appkit-evil)
(require 'slackit-customize)

(declare-function appkit-evil-normalize-keymaps "appkit-evil" ())
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function appkit-chatbuf-focus-input "appkit-chatbuf" ())

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
  '(slackit-root-mode slackit-room-mode slackit-thread-mode)
  "Major modes participating in Slackit's Evil integration.")

(defconst slackit-evil--application-states '(normal motion)
  "Evil states used by Slackit application bindings.")

(defun slackit-evil--set-initial-states ()
  "Register `slackit-evil-initial-state' for Slackit modes."
  (when slackit-evil-initial-state
    (dolist (mode slackit-evil--application-modes)
      (evil-set-initial-state mode slackit-evil-initial-state))))

(defun slackit-evil--define-root-keys ()
  "Install root-directory modal bindings."
  (appkit-evil-define-readonly-keys 'slackit-root-mode-map)
  (appkit-evil-define-keys slackit-evil--application-states
      'slackit-root-mode-map
    (kbd "RET") #'appkit-directory-activate
    (kbd "<return>") #'appkit-directory-activate
    (kbd "g r") #'slackit-root-refresh
    (kbd "TAB") #'appkit-directory-tab-dwim
    (kbd "<backtab>") #'appkit-directory-previous-item
    (kbd "?") #'slackit-root-transient))

(defun slackit-evil--define-room-keys ()
  "Install room-wide and timeline-only modal bindings."
  (appkit-evil-define-keys slackit-evil--application-states
      'slackit-room-mode-map
    (kbd "g r") #'slackit-room-refresh
    (kbd "g +") #'slackit-room-load-older
    (kbd "?") #'slackit-room-transient)

  ;; Appkit disables this mode in the writable composer.  Keep operator and
  ;; word-motion prefixes untouched; the uppercase aliases are deliberate
  ;; message actions only while point is on generated timeline content.
  (appkit-evil-define-keys slackit-evil--application-states
      'slackit-room-timeline-mode-map
    (kbd "q") #'quit-window
    (kbd "RET") #'slackit-actions-open-thread
    (kbd "<return>") #'slackit-actions-open-thread
    (kbd "i") #'appkit-chatbuf-focus-input
    (kbd "E") #'slackit-actions-edit
    (kbd "R") #'slackit-actions-react
    (kbd "Y") #'slackit-actions-copy-text
    (kbd "?") #'slackit-actions-transient)
  (appkit-evil-define-keys 'normal 'slackit-room-timeline-mode-map
    (kbd "D") #'slackit-actions-delete))

(defun slackit-evil--refresh-live-buffers ()
  "Refresh Evil projections in existing Slackit application buffers."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (memq major-mode slackit-evil--application-modes)
          (appkit-evil-normalize-keymaps))))))

;;;###autoload
(defun slackit-evil-setup ()
  "Install Slackit's native Evil integration.
Safe to call multiple times."
  (interactive)
  (when (and (featurep 'evil) slackit-evil-enable-integration)
    (slackit-evil--set-initial-states)
    (slackit-evil--define-root-keys)
    (slackit-evil--define-room-keys)
    (slackit-evil--refresh-live-buffers)))

(with-eval-after-load 'evil
  (slackit-evil-setup))

(provide 'slackit-evil)

;;; slackit-evil.el ends here
