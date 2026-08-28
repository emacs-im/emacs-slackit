;;; slackit-room.el --- Slackit conversation room view -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Appkit chatbuf/timeline assembly and descriptor-driven synchronization for
;; one exact account conversation.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'appkit-chatbuf)
(require 'appkit-chat-completion)
(require 'appkit-chat-history)
(require 'appkit-chat-timeline)
(require 'slackit-customize)
(require 'slackit-history)
(require 'slackit-runtime)
(require 'slackit-state)

(declare-function slackit-compose-submit "slackit-compose" ())
(declare-function slackit-compose-apply-settlement "slackit-compose" (event))
(declare-function slackit-compose-cancel-context "slackit-compose" ())
(declare-function slackit-completion-user "slackit-completion" ())
(declare-function slackit-completion-channel "slackit-completion" ())
(declare-function slackit-completion-setup "slackit-completion" ())
(declare-function slackit-render-message-row "slackit-render" (state message context))
(declare-function slackit-render-reference-dependencies "slackit-render" (text))
(declare-function slackit-actions-open-thread "slackit-actions" ())
(declare-function slackit-actions-edit "slackit-actions" ())
(declare-function slackit-actions-delete "slackit-actions" ())
(declare-function slackit-actions-react "slackit-actions" ())
(declare-function slackit-actions-copy-text "slackit-actions" ())
(declare-function slackit-read-mark-at-point "slackit-read" ())

(defconst slackit-message-key-property 'slackit-message-ts
  "Text property identifying one rendered Slack message row.")

(defvar-local slackit-room--conversation-id nil
  "Exact Slack conversation ID owned by this room buffer.")

(defvar-local slackit-room--app nil
  "Exact Slackit application owned by this room buffer.")

(defun slackit-room-current-app ()
  "Return the current room's live Slackit application."
  (or (and (appkit-app-live-p slackit-room--app) slackit-room--app)
      (when-let* ((view (appkit-current-view))) (appkit-view-app view))
      (user-error "slackit: room has no live account")))

(defun slackit-room-current-conversation-id ()
  "Return the current room's exact conversation ID."
  (or slackit-room--conversation-id
      (user-error "slackit: room has no conversation")))

(defun slackit-room-message-ts-at-point ()
  "Return Slack message timestamp at point, or nil."
  (or (get-text-property (point) slackit-message-key-property)
      (get-text-property (line-beginning-position)
                         slackit-message-key-property)
      (when (appkit-chat-timeline-live-p)
        (appkit-chat-timeline-key-at-point))))

(defvar slackit-room-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'slackit-actions-open-thread)
    (define-key map (kbd "e") #'slackit-actions-edit)
    (define-key map (kbd "d") #'slackit-actions-delete)
    (define-key map (kbd "r") #'slackit-actions-react)
    (define-key map (kbd "m") #'slackit-read-mark-at-point)
    (define-key map (kbd "w") #'slackit-actions-copy-text)
    (define-key map (kbd "g") #'slackit-room-refresh)
    (define-key map (kbd "M-p") #'slackit-room-load-older)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap active over Slackit room generated content.")

(define-minor-mode slackit-room-timeline-mode
  "Minor mode active when point is in a Slackit timeline."
  :lighter nil
  :keymap slackit-room-timeline-mode-map)

(defvar slackit-room-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map appkit-chatbuf-mode-map)
    (define-key map (kbd "C-c C-c") #'slackit-compose-submit)
    (define-key map (kbd "C-c C-o") #'slackit-room-load-older)
    (define-key map (kbd "C-c C-u") #'slackit-completion-user)
    (define-key map (kbd "C-c #") #'slackit-completion-channel)
    (define-key map (kbd "TAB") #'appkit-chat-completion-complete)
    map)
  "Keymap for `slackit-room-mode'.")

(define-derived-mode slackit-room-mode appkit-chatbuf-mode "Slackit-Room"
  "Writable Slackit room with an Appkit timeline and composer."
  (slackit-history-init)
  (slackit-completion-setup)
  (setq-local appkit-chatbuf-input-sync-function
              #'appkit-chatbuf-input-state-sync)
  (appkit-chatbuf-use-timeline-mode #'slackit-room-timeline-mode))

(defun slackit-room--header (state conversation-id)
  "Return room header for STATE and CONVERSATION-ID."
  (let* ((name (slackit-state-conversation-name state conversation-id))
         (status (slackit-account-state-connection-status state)))
    (propertize (format "#%s   [%s]\n\n" name status)
                'read-only t)))

(defun slackit-room--footer ()
  "Return exact history status footer for the current room."
  (concat
   (when (appkit-chatbuf-aux-active-p)
     (appkit-chatbuf-aux-render
      :title (or (plist-get (appkit-chatbuf-aux-state) :title)
                 "Message action")
      :cancel-action #'slackit-compose-cancel-context))
   (appkit-chat-history-delimiter-string
    (max 20 (or fill-column 80))
    :loading-text "loading Slack history…")
   (when slackit-history--error
     (format "\nHistory error: %s" slackit-history--error))
   "\n"))

(defun slackit-room--bind-composer ()
  "Bind the stable writable composer after the timeline footer."
  (appkit-chatbuf-bind-input-region
   :visible-p t
   :prompt "Message: "
   :input-text (appkit-chatbuf-input-state)))

(defun slackit-room--ensure-timeline ()
  "Ensure the current room owns one Appkit timeline."
  (let* ((app (slackit-room-current-app))
         (state (slackit-runtime-state app))
         (conversation-id (slackit-room-current-conversation-id)))
    (appkit-chat-timeline-ensure
     :printer #'slackit-room--print-row
     :anchor-property slackit-message-key-property
     :header (slackit-room--header state conversation-id)
     :footer (slackit-room--footer)
     :after-mutation-function #'appkit-chatbuf-update-context-mode)))

(defun slackit-room--print-row (row)
  "Render Appkit timeline ROW through Slackit's protocol renderer."
  (let* ((app (slackit-room-current-app))
         (state (slackit-runtime-state app))
         (message (appkit-chat-timeline-row-payload row))
         (context (appkit-chat-timeline-row-context row))
         (ts (appkit-chat-timeline-row-key row))
         (start (point)))
    (slackit-render-message-row state message context)
    (unless (bolp) (insert "\n"))
    (add-text-properties start (point)
                         (list slackit-message-key-property ts))))

(defun slackit-room--all-messages (state conversation-id)
  "Return ordered top-level canonical messages for CONVERSATION-ID."
  (delq nil
        (mapcar (lambda (ts)
                  (slackit-state-message state conversation-id ts))
                (slackit-state-top-level-keys state conversation-id))))

(defun slackit-room--project (state messages)
  "Project exact visible MESSAGES from STATE into timeline rows."
  (let ((read-ts (slackit-state-read-ts
                  state (slackit-room-current-conversation-id)))
        first-unread-seen)
    (appkit-chat-timeline-project
     messages
     (lambda (message) (slackit-normalize-get message 'ts))
     :context-function
     (lambda (_previous message)
       (let* ((ts (slackit-normalize-get message 'ts))
              (first-unread
               (and (not first-unread-seen)
                    read-ts
                    (string< read-ts ts))))
         (when first-unread (setq first-unread-seen t))
         (list :unread-divider first-unread)))
     :dependencies-function
     (lambda (message)
       (delete-dups
        (delq nil
              (append
               (list (when-let* ((user (slackit-normalize-get message 'user)))
                       (list :user user)))
               (slackit-render-reference-dependencies
                (slackit-normalize-get message 'text)))))))))

(defun slackit-room--render (&optional force-keys resources)
  "Synchronize the current room, forcing FORCE-KEYS and RESOURCES."
  (let* ((app (slackit-room-current-app))
         (state (slackit-runtime-state app))
         (conversation-id (slackit-room-current-conversation-id))
         (all (slackit-room--all-messages state conversation-id))
         (visible (and (appkit-chat-history-window-known-p)
                       (slackit-history-slice-messages all))))
    (slackit-room--ensure-timeline)
    (appkit-chat-timeline-sync
     (slackit-room--project state visible)
     :force-keys force-keys
     :changed-resources resources)
    (appkit-chat-timeline-set-frame
     (slackit-room--header state conversation-id)
     (slackit-room--footer)
     :bind-input-function #'slackit-room--bind-composer
     :composer-visible-p t)))

(defun slackit-room--repair-deleted-edge (state conversation-id ts)
  "Repair exact history edge after deleting TS from CONVERSATION-ID."
  (when (appkit-chat-history-window-known-p)
    (let ((first (appkit-chat-history-window-first-key))
          (last (appkit-chat-history-window-last-key))
          (keys (slackit-state-top-level-keys state conversation-id)))
      (cond
       ((and first (equal first ts))
        (if-let* ((next (seq-find (lambda (key) (string< ts key)) keys)))
            (appkit-chat-history-window-set next last)
          (if (null last)
              (appkit-chat-history-window-establish-empty)
            (appkit-chat-history-window-clear))))
       ((and last (equal last ts))
        (if-let* ((previous (car (last (seq-filter
                                        (lambda (key) (string< key ts))
                                        keys)))))
            (appkit-chat-history-window-set first previous)
          (appkit-chat-history-window-clear)))))))

(defun slackit-room--apply-event (view change)
  "Apply controller consequences of canonical CHANGE for VIEW."
  (let* ((state (slackit-runtime-state (appkit-view-app view)))
         (conversation-id (slackit-room-current-conversation-id))
         (kind (plist-get change :kind))
         (ts (plist-get change :ts)))
    (pcase kind
      ('message-create
       (when (and (plist-get change :top-level-p)
                  (appkit-chat-history-window-empty-p))
         (appkit-chat-history-window-seed-live ts)))
      ('message-delete
       (slackit-room--repair-deleted-edge state conversation-id ts))
      ((or 'compose-success 'compose-failure)
       (slackit-compose-apply-settlement change)))))

(defun slackit-room--sync (view invalidations)
  "Synchronize room VIEW from coalesced INVALIDATIONS."
  (let ((events (appkit-view-pending-events-snapshot view)))
    (dolist (event events) (slackit-room--apply-event view event))
    (appkit-view-acknowledge-events view (length events))
    (slackit-room--render
     (appkit-invalidations-entry-keys invalidations)
     (appkit-invalidations-resource-keys invalidations))))

(defun slackit-room--setup (conversation-id app _view)
  "Initialize a newly attached room for CONVERSATION-ID and APP."
  (setq-local slackit-room--conversation-id conversation-id
              slackit-room--app app)
  (slackit-history-init))

(defun slackit-room-open (app conversation-id &optional select)
  "Open APP's CONVERSATION-ID room and optionally SELECT it."
  (let* ((state (slackit-runtime-state app))
         (name (slackit-state-conversation-name state conversation-id))
         (view-id (list 'room conversation-id))
         (existing (appkit-view-for-id app view-id))
         (view (appkit-open-view
                :app app
                :id view-id
                :mode 'slackit-room-mode
                :buffer-name (format "*Slackit:%s:#%s*"
                                     (appkit-app-id app) name)
                :state conversation-id
                :sync-function #'slackit-room--sync
                :parts '(frame timeline composer)
                :setup (apply-partially
                        #'slackit-room--setup conversation-id app)
                :select select)))
    (with-current-buffer (appkit-view-buffer view)
      (setq-local slackit-room--conversation-id conversation-id
                  slackit-room--app app)
      (unless existing
        (slackit-history-load-latest view conversation-id)
        (appkit-invalidate view :structure t)
        (appkit-sync-invalidations view)))
    view))

(defun slackit-room-load-older ()
  "Load the current room's next older history page."
  (interactive)
  (let* ((view (or (appkit-current-view)
                   (user-error "slackit: no live room view")))
         (id (appkit-view-id view))
         (root-ts (and (eq (car-safe id) 'thread) (nth 2 id))))
    (slackit-history-load-older
     view (slackit-room-current-conversation-id) root-ts)
    (appkit-request-sync view :part 'frame)))

(defun slackit-room-refresh ()
  "Replace current room exact history with a fresh latest page."
  (interactive)
  (let* ((view (or (appkit-current-view)
                   (user-error "slackit: no live room view")))
         (id (appkit-view-id view))
         (root-ts (and (eq (car-safe id) 'thread) (nth 2 id))))
    (appkit-chat-history-window-clear)
    (setq slackit-history--cursor nil
          slackit-history--error nil)
    (slackit-history-load-latest
     view (slackit-room-current-conversation-id) root-ts)
    (appkit-request-sync view :structure t)))

(provide 'slackit-room)

;;; slackit-room.el ends here
