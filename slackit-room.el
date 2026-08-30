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
(require 'appkit-view)
(require 'slackit-customize)
(require 'slackit-history)
(require 'slackit-runtime)
(require 'slackit-state)
(require 'slackit-avatar)
(require 'slackit-media)
(require 'slackit-emoji)

(declare-function slackit-compose-submit "slackit-compose" ())
(declare-function slackit-compose-apply-settlement "slackit-compose" (event))
(declare-function slackit-compose-cancel-context "slackit-compose" ())
(declare-function slackit-compose-setup "slackit-compose" ())
(declare-function slackit-compose-attach "slackit-compose" ())
(declare-function slackit-compose-attach-file "slackit-compose" (file))
(declare-function slackit-compose-remove-attachment "slackit-compose"
                  (&optional attachment))
(declare-function slackit-compose-cancel-dwim "slackit-compose" ())
(declare-function slackit-compose-upload-card "slackit-compose" ())
(declare-function slackit-completion-user "slackit-completion" ())
(declare-function slackit-completion-channel "slackit-completion" ())
(declare-function slackit-completion-setup "slackit-completion" ())
(declare-function slackit-render-message-row
                  "slackit-render" (app state message context))
(declare-function slackit-render-reference-dependencies "slackit-render" (text))
(declare-function slackit-render-avatar-subject
                  "slackit-render" (state message))
(declare-function slackit-actions-activate "slackit-actions" ())
(declare-function slackit-actions-open-thread "slackit-actions" ())
(declare-function slackit-actions-edit "slackit-actions" ())
(declare-function slackit-actions-delete "slackit-actions" ())
(declare-function slackit-actions-react "slackit-actions" ())
(declare-function slackit-actions-copy-text "slackit-actions" ())
(declare-function slackit-read-mark-at-point "slackit-read" ())
(declare-function slackit-runtime-ensure-user
                  "slackit-runtime" (app user-id))
(declare-function slackit-avatar-resource-key
                  "slackit-avatar" (app user))
(declare-function slackit-avatar-ensure "slackit-avatar" (app user))
(declare-function slackit-room-transient "slackit-transient" (&optional scope))
(declare-function slackit-actions-transient "slackit-transient" (&optional scope))

(defconst slackit-message-key-property 'slackit-message-ts
  "Text property identifying one rendered Slack message row.")

(defvar-local slackit-room--conversation-id nil
  "Exact Slack conversation ID owned by this room buffer.")


(defun slackit-room-current-view ()
  "Return the exact live Slackit room or thread view."
  (let* ((view (appkit-current-view))
         (id (and (appkit-view-p view) (appkit-view-id view)))
         (app (and (appkit-view-p view) (appkit-view-app view))))
    (unless (and (appkit-view-live-p view)
                 (eq (appkit-app-kind app) 'slackit-account)
                 (memq (car-safe id) '(room thread))
                 (eq (appkit-view-buffer view) (current-buffer)))
      (user-error "slackit: this command requires a live Slackit chat view"))
    view))

(defun slackit-room-current-app ()
  "Return the current room's live Slackit application."
  (appkit-view-app (slackit-room-current-view)))

(defun slackit-room-current-conversation-id ()
  "Return the current room's exact conversation ID."
  (nth 1 (appkit-view-id (slackit-room-current-view))))

(defun slackit-room-message-ts-at-point ()
  "Return Slack message timestamp at point, or nil."
  (or (get-text-property (point) slackit-message-key-property)
      (get-text-property (line-beginning-position)
                         slackit-message-key-property)
      (when (appkit-chat-timeline-live-p)
        (appkit-chat-timeline-key-at-point))))

(defvar slackit-room-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'slackit-actions-activate)
    (define-key map (kbd "<return>") #'slackit-actions-activate)
    (define-key map (kbd "T") #'slackit-actions-open-thread)
    (define-key map (kbd "e") #'slackit-actions-edit)
    (define-key map (kbd "d") #'slackit-actions-delete)
    (define-key map (kbd "r") #'slackit-actions-react)
    (define-key map (kbd "m") #'slackit-read-mark-at-point)
    (define-key map (kbd "w") #'slackit-actions-copy-text)
    (define-key map (kbd "g") #'slackit-room-refresh)
    (define-key map (kbd "M-p") #'slackit-room-load-older)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "?") #'slackit-actions-transient)
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
    (define-key map (kbd "C-c C-a") #'slackit-compose-attach)
    (define-key map (kbd "C-c C-f") #'slackit-compose-attach-file)
    (define-key map (kbd "C-c C-d") #'slackit-compose-remove-attachment)
    (define-key map (kbd "C-c C-k") #'slackit-compose-cancel-dwim)
    (define-key map (kbd "C-c C-o") #'slackit-room-load-older)
    (define-key map (kbd "C-c C-u") #'slackit-completion-user)
    (define-key map (kbd "C-c #") #'slackit-completion-channel)
    (define-key map (kbd "C-c ?") #'slackit-room-transient)
    (define-key map (kbd "C-c m") #'slackit-actions-transient)
    (define-key map (kbd "TAB") #'appkit-chat-completion-complete)
    map)
  "Keymap for `slackit-room-mode'.")


(define-derived-mode slackit-room-mode appkit-chatbuf-mode "Slackit-Room"
  "Writable Slackit room with an Appkit timeline and composer."
  (slackit-history-init)
  (slackit-completion-setup)
  (slackit-compose-setup)
  (setq-local appkit-chatbuf-input-sync-function
              #'appkit-chatbuf-input-state-sync)
  (appkit-chatbuf-use-timeline-mode #'slackit-room-timeline-mode))

(defun slackit-room--header (state conversation-id)
  "Return room header for STATE and CONVERSATION-ID."
  (let ((label (slackit-state-conversation-label state conversation-id))
        (status (slackit-account-state-connection-status state)))
    (propertize (format "%s   [%s]\n\n" label status)
                'read-only t)))

(defun slackit-room--footer ()
  "Return exact history status footer for the current room."
  (concat
   (slackit-compose-upload-card)
   (when (appkit-chatbuf-aux-active-p)
     (appkit-chatbuf-aux-render
      :title (or (plist-get (appkit-chatbuf-aux-state) :title)
                 "Message action")
      :cancel-action #'slackit-compose-cancel-context))
   (appkit-chat-history-delimiter-string
    (max 20
         (or (appkit-view-responsive-width
              slackit-room-auto-fill-margin-columns)
             fill-column
             80))
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
    (slackit-render-message-row app state message context)
    (unless (bolp) (insert "\n"))
    (add-text-properties start (point)
                         (list slackit-message-key-property ts))))

(defun slackit-room--all-messages (state conversation-id)
  "Return ordered top-level canonical messages for CONVERSATION-ID."
  (delq nil
        (mapcar (lambda (ts)
                  (slackit-state-message state conversation-id ts))
                (slackit-state-top-level-keys state conversation-id))))

(defun slackit-room--message-resources (app state message)
  "Return and ensure opaque Appkit resources for APP MESSAGE from STATE."
  (let* ((user-id (alist-get 'user message))
         (subject (slackit-render-avatar-subject state message))
         (resources
          (delete-dups
           (delq nil
                 (append
                  (list
                   (and user-id (list :user user-id))
                   (and subject
                        (slackit-avatar-resource-key app subject)))
                  (slackit-render-reference-dependencies
                   (alist-get 'text message))
                  (slackit-media-message-resource-keys app message)
                  (slackit-emoji-message-resource-keys app message))))))
    (when user-id
      (slackit-runtime-ensure-user app user-id))
    (when (and slackit-show-avatars
               (display-graphic-p)
               subject)
      (slackit-avatar-ensure app subject))
    (slackit-media-ensure-message app message)
    (slackit-emoji-ensure-message app message)
    resources))

(defun slackit-room--message-epoch (message)
  "Return numeric presentation time for MESSAGE's opaque Slack timestamp."
  (let ((timestamp (alist-get 'ts message)))
    (when (and (stringp timestamp)
               (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" timestamp))
      (string-to-number timestamp))))

(defun slackit-room--message-day-key (message)
  "Return local calendar day key for MESSAGE, or nil."
  (when-let* ((epoch (slackit-room--message-epoch message)))
    (format-time-string "%Y-%m-%d" (seconds-to-time epoch))))

(defun slackit-room--message-day-label (message)
  "Return readable local date separator label for MESSAGE."
  (when-let* ((epoch (slackit-room--message-epoch message)))
    (format-time-string "%A, %B %e, %Y" (seconds-to-time epoch))))

(defun slackit-room--message-sender-key (message)
  "Return stable sender key for MESSAGE."
  (or (alist-get 'user message)
      (alist-get 'bot_id message)
      (alist-get 'username message)))

(defun slackit-room--messages-compact-group-p (previous message)
  "Return non-nil when MESSAGE may visually continue PREVIOUS."
  (and slackit-group-messages
       previous
       (equal (slackit-room--message-sender-key previous)
              (slackit-room--message-sender-key message))
       (let ((previous-time (slackit-room--message-epoch previous))
             (message-time (slackit-room--message-epoch message)))
         (and previous-time
              message-time
              (<= (abs (- message-time previous-time))
                  (max 0 slackit-group-messages-timespan))))))

(defun slackit-room--message-context
    (previous message &optional unread-divider)
  "Return visual context for MESSAGE after PREVIOUS.

UNREAD-DIVIDER marks MESSAGE as the first unread row."
  (let* ((day (slackit-room--message-day-key message))
         (previous-day (and previous
                            (slackit-room--message-day-key previous)))
         (new-day (and day (not (equal day previous-day)))))
    (list :date-separator
          (and new-day (slackit-room--message-day-label message))
          :unread-divider (and unread-divider t)
          :compact
          (and (not new-day)
               (not unread-divider)
               (slackit-room--messages-compact-group-p previous message)))))

(defun slackit-room--project (app state messages)
  "Project exact visible MESSAGES from STATE into timeline rows owned by APP."
  (let ((read-ts (slackit-state-read-ts
                  state (slackit-room-current-conversation-id)))
        first-unread-seen)
    (appkit-chat-timeline-project
     messages
     (lambda (message) (alist-get 'ts message))
     :context-function
     (lambda (previous message)
       (let* ((timestamp (alist-get 'ts message))
              (first-unread
               (and (not first-unread-seen)
                    read-ts
                    (string< read-ts timestamp))))
         (when first-unread (setq first-unread-seen t))
         (slackit-room--message-context previous message first-unread)))
     :dependencies-function
     (lambda (message)
       (slackit-room--message-resources app state message)))))

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
     (slackit-room--project app state visible)
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

(defun slackit-room--configure-responsive-view (view sync-function)
  "Configure live chat VIEW for responsive rendering through SYNC-FUNCTION."
  (setf (appkit-view-sync-function view) sync-function
        (appkit-view-parts view) '(frame timeline composer geometry))
  (appkit-view-enable-responsive-geometry view)
  view)

(defun slackit-room--invalidation-force-keys (invalidations)
  "Return row keys requiring redraw for coalesced INVALIDATIONS."
  (let ((entries (appkit-invalidations-entry-keys invalidations)))
    (if (memq 'geometry (appkit-invalidations-parts invalidations))
        (progn
          (when-let* ((width
                       (appkit-view-responsive-width
                        slackit-room-auto-fill-margin-columns)))
            (setq-local fill-column width))
          (delete-dups
           (append entries
                   (and (appkit-chat-timeline-live-p)
                        (appkit-chat-timeline-keys)))))
      entries)))

(defun slackit-room--sync (view invalidations)
  "Synchronize room VIEW from coalesced INVALIDATIONS."
  (let ((events (appkit-view-pending-events-snapshot view)))
    (dolist (event events) (slackit-room--apply-event view event))
    (appkit-view-acknowledge-events view (length events))
    (slackit-room--render
     (slackit-room--invalidation-force-keys invalidations)
     (appkit-invalidations-resource-keys invalidations))))

(defun slackit-room--setup (conversation-id _app _view)
  "Initialize a newly attached room for CONVERSATION-ID."
  (setq-local slackit-room--conversation-id conversation-id)
  (slackit-history-init))

(defun slackit-room-open (app conversation-id &optional select)
  "Open APP's CONVERSATION-ID room and optionally SELECT it."
  (let* ((state (slackit-runtime-state app))
         (label (slackit-state-conversation-label state conversation-id))
         (view-id (list 'room conversation-id))
         (existing (appkit-view-for-id app view-id))
         (view (appkit-open-view
                :app app
                :id view-id
                :mode 'slackit-room-mode
                :buffer-name (format "*Slackit:%s:%s*"
                                     (appkit-app-id app) label)
                :state conversation-id
                :sync-function #'slackit-room--sync
                :parts '(frame timeline composer geometry)
                :setup (apply-partially
                        #'slackit-room--setup conversation-id app)
                :select select)))
    (slackit-room--configure-responsive-view
     view #'slackit-room--sync)
    (with-current-buffer (appkit-view-buffer view)
      (setq-local slackit-room--conversation-id conversation-id)
      (unless existing
        (slackit-history-load-latest view conversation-id)
        (appkit-invalidate view :structure t)
        (appkit-sync-invalidations view))
      (appkit-view-refresh-responsive-geometry))
    view))

(defun slackit-room-load-older ()
  "Load the current room-history or thread-replies next page."
  (interactive)
  (let* ((view (slackit-room-current-view))
         (id (appkit-view-id view))
         (root-ts (and (eq (car-safe id) 'thread) (nth 2 id))))
    (slackit-history-load-older
     view (slackit-room-current-conversation-id) root-ts)
    (appkit-request-sync view :part 'frame)))

(defun slackit-room-refresh ()
  "Replace current room or thread history with a fresh first page."
  (interactive)
  (let* ((view (slackit-room-current-view))
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
