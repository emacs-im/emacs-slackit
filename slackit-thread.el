;;; slackit-thread.el --- Slackit thread view -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Stable Appkit chatbuf/timeline view for one Slack root timestamp and its
;; replies.  Canonical messages remain owned once by account state.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'appkit-chat-history)
(require 'appkit-chat-timeline)
(require 'appkit-view)
(require 'slackit-compose)
(require 'slackit-history)
(require 'slackit-render)
(require 'slackit-room)
(require 'slackit-runtime)
(require 'slackit-state)
(declare-function slackit-room--ensure-user-resources
                  "slackit-room" (app resources))
(declare-function slackit-room--message-context
                  "slackit-room" (previous message &optional unread-divider))
(declare-function slackit-avatar-resource-key
                  "slackit-avatar" (app user))

(defvar-local slackit-thread--root-ts nil
  "Root Slack timestamp owned by the current thread view.")

(define-derived-mode slackit-thread-mode slackit-room-mode "Slackit-Thread"
  "Writable Slackit thread view backed by Appkit chat primitives.")

(defun slackit-thread-current-root-ts ()
  "Return exact root timestamp for the current thread."
  (or slackit-thread--root-ts
      (when-let* ((view (appkit-current-view))) (nth 2 (appkit-view-id view)))
      (user-error "slackit: thread has no root message")))

(defun slackit-thread--header (state conversation-id root-ts)
  "Return header for thread ROOT-TS in CONVERSATION-ID from STATE."
  (let* ((room (slackit-state-conversation-name state conversation-id))
         (root (slackit-state-message state conversation-id root-ts))
         (sender (and root
                      (slackit-state-user-name
                       state (slackit-normalize-get root 'user)))))
    (propertize
     (format "#%s thread %s%s   [%s]\n\n"
             room root-ts (if sender (format " — %s" sender) "")
             (slackit-account-state-connection-status state))
     'read-only t)))

(defun slackit-thread--messages (state conversation-id root-ts)
  "Return ordered root/reply messages for one thread."
  (delq nil
        (mapcar (lambda (ts)
                  (slackit-state-message state conversation-id ts))
                (slackit-state-reply-keys state conversation-id root-ts))))

(defun slackit-thread--project (app state messages)
  "Project thread MESSAGES from STATE into stable Appkit rows owned by APP."
  (appkit-chat-timeline-project
   messages
   (lambda (message) (slackit-normalize-get message 'ts))
   :context-function
   (lambda (previous message)
     (slackit-room--message-context previous message))
   :dependencies-function
   (lambda (message)
     (let* ((user-id (slackit-normalize-get message 'user))
            (user (and user-id (slackit-state-user state user-id)))
            (dependencies
             (delete-dups
              (delq nil
                    (append
                     (list
                      (and user-id (list :user user-id))
                      (and user
                           (slackit-avatar-resource-key app user)))
                     (slackit-render-reference-dependencies
                      (slackit-normalize-get message 'text)))))))
       (slackit-room--ensure-user-resources app dependencies)))))

(defun slackit-thread--ensure-timeline ()
  "Ensure the current thread owns one Appkit timeline."
  (let* ((app (slackit-room-current-app))
         (state (slackit-runtime-state app))
         (conversation-id (slackit-room-current-conversation-id))
         (root-ts (slackit-thread-current-root-ts)))
    (appkit-chat-timeline-ensure
     :printer #'slackit-room--print-row
     :anchor-property slackit-message-key-property
     :header (slackit-thread--header state conversation-id root-ts)
     :footer (slackit-room--footer)
     :after-mutation-function #'appkit-chatbuf-update-context-mode)))

(defun slackit-thread--render (&optional force-keys resources)
  "Synchronize current thread, forcing FORCE-KEYS and RESOURCES."
  (let* ((app (slackit-room-current-app))
         (state (slackit-runtime-state app))
         (conversation-id (slackit-room-current-conversation-id))
         (root-ts (slackit-thread-current-root-ts))
         (all (slackit-thread--messages state conversation-id root-ts))
         (visible (and (appkit-chat-history-window-known-p)
                       (slackit-history-slice-messages all))))
    (slackit-thread--ensure-timeline)
    (appkit-chat-timeline-sync
     (slackit-thread--project app state visible)
     :force-keys force-keys
     :changed-resources resources)
    (appkit-chat-timeline-set-frame
     (slackit-thread--header state conversation-id root-ts)
     (slackit-room--footer)
     :bind-input-function #'slackit-room--bind-composer
     :composer-visible-p t)))

(defun slackit-thread--repair-deleted-edge (state conversation-id root-ts ts)
  "Repair thread exact history edge after deleting TS."
  (when (appkit-chat-history-window-known-p)
    (let ((first (appkit-chat-history-window-first-key))
          (last (appkit-chat-history-window-last-key))
          (keys (slackit-state-reply-keys state conversation-id root-ts)))
      (cond
       ((equal first ts)
        (if-let* ((next (seq-find (lambda (key) (string< ts key)) keys)))
            (appkit-chat-history-window-set next last)
          (if (null last)
              (appkit-chat-history-window-establish-empty)
            (appkit-chat-history-window-clear))))
       ((equal last ts)
        (if-let* ((previous (car (last (seq-filter
                                       (lambda (key) (string< key ts)) keys)))))
            (appkit-chat-history-window-set first previous)
          (appkit-chat-history-window-clear)))))))

(defun slackit-thread--relevant-message-p (change root-ts)
  "Return non-nil when canonical CHANGE belongs to ROOT-TS thread."
  (or (equal (plist-get change :ts) root-ts)
      (equal (plist-get change :root-ts) root-ts)))

(defun slackit-thread--apply-event (view change)
  "Apply controller CHANGE for thread VIEW."
  (let* ((app (appkit-view-app view))
         (state (slackit-runtime-state app))
         (conversation-id (slackit-room-current-conversation-id))
         (root-ts (slackit-thread-current-root-ts))
         (kind (plist-get change :kind))
         (ts (plist-get change :ts)))
    (pcase kind
      ('message-create
       (when (and (slackit-thread--relevant-message-p change root-ts)
                  (appkit-chat-history-window-empty-p))
         (appkit-chat-history-window-seed-live ts)))
      ('message-delete
       (when (slackit-thread--relevant-message-p change root-ts)
         (slackit-thread--repair-deleted-edge
          state conversation-id root-ts ts)))
      ((or 'compose-success 'compose-failure)
       (slackit-compose-apply-settlement change)))))

(defun slackit-thread--sync (view invalidations)
  "Synchronize thread VIEW from coalesced INVALIDATIONS."
  (let* ((events (appkit-view-pending-events-snapshot view))
         (parts (appkit-invalidations-parts invalidations))
         (geometry-p (memq 'geometry parts))
         (entries (appkit-invalidations-entry-keys invalidations))
         (resources (appkit-invalidations-resource-keys invalidations)))
    (when geometry-p
      (when-let* ((width
                   (appkit-view-responsive-width
                    slackit-room-auto-fill-margin-columns)))
        (setq-local fill-column width)))
    (dolist (event events) (slackit-thread--apply-event view event))
    (appkit-view-acknowledge-events view (length events))
    (slackit-thread--render
     (if geometry-p
         (delete-dups
          (append entries
                  (and (appkit-chat-timeline-live-p)
                       (appkit-chat-timeline-keys))))
       entries)
     resources)))

(defun slackit-thread--setup (app conversation-id root-ts _view)
  "Initialize one newly attached APP thread view."
  (setq-local slackit-room--app app
              slackit-room--conversation-id conversation-id
              slackit-thread--root-ts root-ts)
  (slackit-history-init))

(defun slackit-thread-open (app conversation-id root-ts &optional select)
  "Open stable thread ROOT-TS in APP CONVERSATION-ID."
  (unless (slackit-state-message
           (slackit-runtime-state app) conversation-id root-ts)
    (user-error "slackit: thread root no longer exists"))
  (let* ((name (slackit-state-conversation-name
                (slackit-runtime-state app) conversation-id))
         (view-id (list 'thread conversation-id root-ts))
         (existing (appkit-view-for-id app view-id))
         (view (appkit-open-view
                :app app
                :id view-id
                :mode 'slackit-thread-mode
                :buffer-name (format "*Slackit:%s:#%s:thread %s*"
                                     (appkit-app-id app) name root-ts)
                :state (cons conversation-id root-ts)
                :sync-function #'slackit-thread--sync
                :parts '(frame timeline composer geometry)
                :setup (apply-partially
                        #'slackit-thread--setup
                        app conversation-id root-ts)
                :select select)))
    (setf (appkit-view-sync-function view) #'slackit-thread--sync
          (appkit-view-parts view) '(frame timeline composer geometry))
    (appkit-view-enable-responsive-geometry view)
    (with-current-buffer (appkit-view-buffer view)
      (setq-local slackit-room--app app
                  slackit-room--conversation-id conversation-id
                  slackit-thread--root-ts root-ts)
      (unless existing
        (slackit-history-load-latest view conversation-id root-ts)
        (appkit-invalidate view :structure t)
        (appkit-sync-invalidations view))
      (appkit-view-refresh-responsive-geometry))
    view))

(defun slackit-thread-load-older ()
  "Load next older page in the current thread."
  (interactive)
  (let ((view (or (appkit-current-view)
                  (user-error "slackit: no live thread view"))))
    (slackit-history-load-older
     view (slackit-room-current-conversation-id)
     (slackit-thread-current-root-ts))
    (appkit-request-sync view :part 'frame)))

(provide 'slackit-thread)

;;; slackit-thread.el ends here
