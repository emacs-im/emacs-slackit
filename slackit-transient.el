;;; slackit-transient.el --- Context-safe Slackit command menus -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Discoverable command menus for one exact Slackit Appkit view.  Each prefix
;; captures the originating view and stable Slack keys once.  Suffixes resolve
;; those keys against that same live app generation instead of consulting the
;; selected buffer or point again.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'transient)
(require 'appkit-chatbuf)
(require 'appkit-core)
(require 'appkit-directory)
(require 'appkit-media-card)
(require 'slackit-actions)
(require 'slackit-compose)
(require 'slackit-read)
(require 'slackit-root)
(require 'slackit-user)

(cl-defstruct (slackit-transient-scope
               (:constructor slackit-transient--scope-create))
  "Immutable target captured when a Slackit command menu opens."
  view
  view-id
  user-id
  conversation-id
  message-ts
  entry-payload
  media-context)

(defun slackit-transient--view-valid-p (scope)
  "Return non-nil when SCOPE still owns its exact registered Appkit view."
  (when (slackit-transient-scope-p scope)
    (let* ((view (slackit-transient-scope-view scope))
           (expected-id (slackit-transient-scope-view-id scope))
           (app (and (appkit-surface-p view) (appkit-surface-app view))))
      (and (appkit-surface-live-p view)
           (slackit-runtime-account-p app)
           (equal (appkit-surface-identity view) expected-id)
           (eq view (appkit-app-surface app expected-id))
           (with-current-buffer (appkit-surface-buffer view)
             (eq (appkit-current-surface) view))))))

(defun slackit-transient--room-view-valid-p (scope)
  "Return non-nil when SCOPE still owns its exact room or thread context."
  (and (slackit-transient--view-valid-p scope)
       (let* ((view (slackit-transient-scope-view scope))
              (view-id (slackit-transient-scope-view-id scope))
              (conversation-id
               (slackit-transient-scope-conversation-id scope))
              (app (appkit-surface-app view)))
         (and (memq (car-safe view-id) '(room thread))
              (equal conversation-id (nth 1 view-id))
              (with-current-buffer (appkit-surface-buffer view)
                (equal slackit-room--conversation-id conversation-id))
              (slackit-state-conversation
               (slackit-runtime-state app) conversation-id)))))

(defun slackit-transient--user-view-valid-p (scope)
  "Return non-nil when SCOPE still owns its exact user profile."
  (and (slackit-transient--view-valid-p scope)
       (let* ((view (slackit-transient-scope-view scope))
              (view-id (slackit-transient-scope-view-id scope))
              (user-id (slackit-transient-scope-user-id scope)))
         (and (eq (car-safe view-id) 'user)
              (equal user-id (cadr view-id))
              (with-current-buffer (appkit-surface-buffer view)
                (equal slackit-user--user-id user-id))))))

(defun slackit-transient--conversation-valid-p (scope)
  "Return non-nil when SCOPE's exact conversation is still canonical."
  (and (slackit-transient--view-valid-p scope)
       (let* ((view (slackit-transient-scope-view scope))
              (conversation-id
               (slackit-transient-scope-conversation-id scope)))
         (and conversation-id
              (slackit-state-conversation
               (slackit-runtime-state (appkit-surface-app view))
               conversation-id)))))

(defun slackit-transient--message (scope)
  "Return SCOPE's current canonical message, or nil."
  (when (slackit-transient--room-view-valid-p scope)
    (let* ((view (slackit-transient-scope-view scope))
           (app (appkit-surface-app view))
           (conversation-id
            (slackit-transient-scope-conversation-id scope))
           (ts (slackit-transient-scope-message-ts scope)))
      (and ts
           (slackit-state-message
            (slackit-runtime-state app) conversation-id ts)))))

(defun slackit-transient--message-owned-p (scope)
  "Return non-nil when SCOPE's message belongs to the exact account self."
  (when-let* ((message (slackit-transient--message scope)))
    (let* ((app (appkit-surface-app (slackit-transient-scope-view scope)))
           (state (slackit-runtime-state app))
           (self-id (slackit-state-self-id state))
           (author-id (alist-get 'user message)))
      (and (stringp self-id)
           (not (string-empty-p self-id))
           (equal self-id author-id)))))

(defun slackit-transient--capture-view (kinds)
  "Capture the exact current Slackit view whose ID kind belongs to KINDS."
  (let* ((view (appkit-current-surface))
         (view-id (and (appkit-surface-p view) (appkit-surface-identity view)))
         (app (and (appkit-surface-p view) (appkit-surface-app view))))
    (unless (and (appkit-surface-live-p view)
                 (slackit-runtime-account-p app)
                 (memq (car-safe view-id) kinds)
                 (eq view (appkit-app-surface app view-id)))
      (user-error "slackit: this command requires a live Slackit %s view"
                  (mapconcat #'symbol-name kinds " or ")))
    view))

(defun slackit-transient--capture-root-scope ()
  "Capture the current root view and its exact optional directory item."
  (let* ((view (slackit-transient--capture-view '(root)))
         (entry (appkit-directory-entry-at-point))
         (payload (and (appkit-directory-entry-p entry)
                       (appkit-directory-entry-item-p entry)
                       (appkit-directory-entry-payload entry))))
    (slackit-transient--scope-create
     :view view
     :view-id (copy-tree (appkit-surface-identity view))
     :conversation-id (and payload (copy-tree payload))
     :entry-payload (and payload (copy-tree payload)))))

(defun slackit-transient--capture-user-scope ()
  "Capture the exact current Slackit user profile."
  (let* ((view (slackit-transient--capture-view '(user)))
         (view-id (appkit-surface-identity view))
         (user-id (cadr view-id))
         (scope
          (slackit-transient--scope-create
           :view view
           :view-id (copy-tree view-id)
           :user-id (and user-id (copy-sequence user-id)))))
    (unless (slackit-transient--user-view-valid-p scope)
      (user-error "slackit: the user target is no longer canonical"))
    scope))

(defun slackit-transient--capture-room-scope (&optional require-message)
  "Capture the current room view and exact row.

When REQUIRE-MESSAGE is non-nil, reject a missing or stale message row."
  (let* ((view (slackit-transient--capture-view '(room thread)))
         (view-id (appkit-surface-identity view))
         (app (appkit-surface-app view))
         (conversation-id (nth 1 view-id))
         (ts (slackit-room-message-ts-at-point))
         (scope
          (slackit-transient--scope-create
           :view view
           :view-id (copy-tree view-id)
           :conversation-id (copy-tree conversation-id)
           :message-ts (and ts (copy-sequence ts))
           :media-context
           (when-let* ((context (appkit-media-card-context-at-point)))
             (copy-sequence context)))))
    (unless (and (equal slackit-room--conversation-id conversation-id)
                 (slackit-state-conversation
                  (slackit-runtime-state app) conversation-id))
      (user-error "slackit: the room target is no longer canonical"))
    (when (and require-message (null (slackit-transient--message scope)))
      (user-error "slackit: no live message at point"))
    scope))

(defun slackit-transient--prefix-scope (prefix)
  "Return PREFIX's Slackit scope, or nil."
  (let ((scope (transient-scope prefix)))
    (and (slackit-transient-scope-p scope) scope)))

(defun slackit-transient--require-view (scope)
  "Return SCOPE's exact view or signal when its generation is stale."
  (unless (slackit-transient--view-valid-p scope)
    (user-error "slackit: the view that opened this menu is no longer live"))
  (slackit-transient-scope-view scope))

(defun slackit-transient--require-room (scope)
  "Return SCOPE's exact room view or signal when its target is stale."
  (unless (slackit-transient--room-view-valid-p scope)
    (user-error "slackit: the room that opened this menu is no longer live"))
  (slackit-transient-scope-view scope))

(defun slackit-transient--require-conversation (scope)
  "Return SCOPE's exact canonical conversation ID or signal."
  (unless (slackit-transient--conversation-valid-p scope)
    (user-error "slackit: the captured conversation no longer exists"))
  (slackit-transient-scope-conversation-id scope))

(defun slackit-transient--require-message (scope)
  "Return SCOPE's current canonical message or signal."
  (or (slackit-transient--message scope)
      (user-error "slackit: the captured message no longer exists")))

(defun slackit-transient--root-scope ()
  "Return and revalidate the active root menu scope."
  (let ((scope (slackit-transient--prefix-scope 'slackit-root-transient)))
    (slackit-transient--require-view scope)
    scope))

(defun slackit-transient--room-scope ()
  "Return and revalidate the active room menu scope."
  (let ((scope (slackit-transient--prefix-scope 'slackit-room-transient)))
    (slackit-transient--require-room scope)
    scope))

(defun slackit-transient--actions-scope ()
  "Return and revalidate the active message-actions menu scope."
  (let ((scope (slackit-transient--prefix-scope 'slackit-actions-transient)))
    (slackit-transient--require-message scope)
    scope))

(defun slackit-transient--user-scope ()
  "Return and revalidate the active user menu scope."
  (let ((scope (slackit-transient--prefix-scope 'slackit-user-transient)))
    (unless (slackit-transient--user-view-valid-p scope)
      (user-error "slackit: the user view that opened this menu is stale"))
    scope))

(defun slackit-transient--call-in-view (scope function)
  "Call zero-argument FUNCTION in SCOPE's exact registered view."
  (let ((view (slackit-transient--require-view scope)))
    (with-current-buffer (appkit-surface-buffer view)
      ;; Revalidate after changing buffers so no repurposed buffer can run it.
      (slackit-transient--require-view scope)
      (funcall function))))

(defun slackit-transient--view-inapt-p (prefix)
  "Return non-nil when PREFIX has no exact live view."
  (not (slackit-transient--view-valid-p
        (slackit-transient--prefix-scope prefix))))

(defun slackit-transient--root-view-inapt-p ()
  "Return non-nil when the root prefix has lost its view generation."
  (slackit-transient--view-inapt-p 'slackit-root-transient))

(defun slackit-transient--root-entry-inapt-p ()
  "Return non-nil when the root prefix has no canonical captured item."
  (let ((scope (slackit-transient--prefix-scope 'slackit-root-transient)))
    (or (not (slackit-transient--conversation-valid-p scope))
        (not (equal (slackit-transient-scope-entry-payload scope)
                    (slackit-transient-scope-conversation-id scope))))))

(defun slackit-transient--room-inapt-p ()
  "Return non-nil when the room prefix has lost its canonical context."
  (not (slackit-transient--room-view-valid-p
        (slackit-transient--prefix-scope 'slackit-room-transient))))

(defun slackit-transient--room-message-inapt-p ()
  "Return non-nil when the room prefix has no canonical captured message."
  (null (slackit-transient--message
         (slackit-transient--prefix-scope 'slackit-room-transient))))

(defun slackit-transient--room-read-inapt-p ()
  "Return non-nil when captured room context cannot mark read."
  (let ((scope (slackit-transient--prefix-scope 'slackit-room-transient)))
    (or (null (slackit-transient--message scope))
        (not (eq (car-safe (slackit-transient-scope-view-id scope)) 'room)))))

(defun slackit-transient--room-cancel-inapt-p ()
  "Return non-nil when the captured composer has nothing to cancel."
  (let ((scope (slackit-transient--prefix-scope 'slackit-room-transient)))
    (or (not (slackit-transient--room-view-valid-p scope))
        (with-current-buffer
            (appkit-surface-buffer (slackit-transient-scope-view scope))
          (and (not (slackit-compose-upload-active-p))
               (not (appkit-chatbuf-aux-active-p)))))))

(defun slackit-transient--actions-message-inapt-p ()
  "Return non-nil when the actions prefix has lost its message."
  (null (slackit-transient--message
         (slackit-transient--prefix-scope 'slackit-actions-transient))))

(defun slackit-transient--actions-owned-inapt-p ()
  "Return non-nil unless the actions message belongs to account self."
  (not (slackit-transient--message-owned-p
        (slackit-transient--prefix-scope 'slackit-actions-transient))))

(defun slackit-transient--actions-edit-inapt-p ()
  "Return non-nil when the exact message cannot enter edit composition."
  (let ((scope (slackit-transient--prefix-scope 'slackit-actions-transient)))
    (or (not (slackit-transient--message-owned-p scope))
        (with-current-buffer
            (appkit-surface-buffer (slackit-transient-scope-view scope))
          (not (appkit-chatbuf-composer-idle-p))))))

(defun slackit-transient--actions-media-inapt-p (action)
  "Return non-nil when captured media ACTION is unavailable."
  (let* ((scope
          (slackit-transient--prefix-scope 'slackit-actions-transient))
         (context
          (and scope (slackit-transient-scope-media-context scope))))
    (or (not (slackit-transient--room-view-valid-p scope))
        (appkit-media-card-action-inapt-reason action context))))

(defun slackit-transient--actions-media-open-inapt-p ()
  "Return non-nil when captured media cannot be opened."
  (slackit-transient--actions-media-inapt-p 'open))

(defun slackit-transient--actions-media-download-inapt-p ()
  "Return non-nil when captured media cannot be downloaded."
  (slackit-transient--actions-media-inapt-p 'download))

(defun slackit-transient--actions-media-cancel-inapt-p ()
  "Return non-nil when captured media download cannot be canceled."
  (slackit-transient--actions-media-inapt-p 'cancel))

(defun slackit-transient--actions-media-save-inapt-p ()
  "Return non-nil when captured media cannot be saved."
  (slackit-transient--actions-media-inapt-p 'save-as))

(defun slackit-transient--actions-read-inapt-p ()
  "Return non-nil when captured message cannot advance room read state."
  (let ((scope (slackit-transient--prefix-scope 'slackit-actions-transient)))
    (or (null (slackit-transient--message scope))
        (not (eq (car-safe (slackit-transient-scope-view-id scope)) 'room)))))

(defun slackit-transient--quit-inapt-p (prefix)
  "Return non-nil when PREFIX's exact view has no displayed live window."
  (let* ((scope (slackit-transient--prefix-scope prefix))
         (view (and (slackit-transient--view-valid-p scope)
                    (slackit-transient-scope-view scope))))
    (not (and view
              (window-live-p
               (get-buffer-window (appkit-surface-buffer view) t))))))

(defun slackit-transient--root-quit-inapt-p ()
  "Return non-nil when the root source window cannot be quit."
  (slackit-transient--quit-inapt-p 'slackit-root-transient))

(defun slackit-transient--room-quit-inapt-p ()
  "Return non-nil when the room source window cannot be quit."
  (slackit-transient--quit-inapt-p 'slackit-room-transient))

(defun slackit-transient--user-view-inapt-p ()
  "Return non-nil when the user prefix has lost its exact view."
  (not (slackit-transient--user-view-valid-p
        (slackit-transient--prefix-scope 'slackit-user-transient))))

(defun slackit-transient--user-quit-inapt-p ()
  "Return non-nil when the user source window cannot be quit."
  (slackit-transient--quit-inapt-p 'slackit-user-transient))

(defun slackit-transient--root-description ()
  "Return the exact root menu title."
  (let* ((scope (slackit-transient--prefix-scope 'slackit-root-transient))
         (view (and (slackit-transient--view-valid-p scope)
                    (slackit-transient-scope-view scope))))
    (if view
        (format "Slackit · %s" (appkit-app-identity (appkit-surface-app view)))
      "Slackit root · stale context")))

(defun slackit-transient--room-description ()
  "Return the exact room or thread menu title."
  (let* ((scope (slackit-transient--prefix-scope 'slackit-room-transient))
         (view (and (slackit-transient--room-view-valid-p scope)
                    (slackit-transient-scope-view scope))))
    (if (not view)
        "Slackit room · stale context"
      (let* ((app (appkit-surface-app view))
             (view-id (slackit-transient-scope-view-id scope))
             (conversation-id
              (slackit-transient-scope-conversation-id scope))
             (label (slackit-state-conversation-label
                     (slackit-runtime-state app) conversation-id)))
        (if (eq (car-safe view-id) 'thread)
            (format "%s thread · %s" label (nth 2 view-id))
          (format "%s actions" label))))))

(defun slackit-transient--actions-description ()
  "Return the exact message-actions menu title."
  (let ((scope (slackit-transient--prefix-scope
                'slackit-actions-transient)))
    (if (slackit-transient--message scope)
        (format "Message %s in %s"
                (slackit-transient-scope-message-ts scope)
                (slackit-state-conversation-label
                 (slackit-runtime-state
                  (appkit-surface-app
                   (slackit-transient-scope-view scope)))
                 (slackit-transient-scope-conversation-id scope)))
      "Slackit message · stale context")))

(defun slackit-transient--user-description ()
  "Return the exact user-profile menu title."
  (let* ((scope
          (slackit-transient--prefix-scope 'slackit-user-transient))
         (view (and (slackit-transient--user-view-valid-p scope)
                    (slackit-transient-scope-view scope))))
    (if (not view)
        "Slackit user · stale context"
      (let* ((user-id (slackit-transient-scope-user-id scope))
             (state (slackit-runtime-state (appkit-surface-app view))))
        (format "%s · %s"
                (slackit-state-user-name state user-id)
                user-id)))))

(defun slackit-transient--quit-view (scope)
  "Quit the window displaying SCOPE's exact view."
  (let* ((view (slackit-transient--require-view scope))
         (window (get-buffer-window (appkit-surface-buffer view) t)))
    (unless (window-live-p window)
      (user-error "slackit: the menu's source view is no longer displayed"))
    (quit-window nil window)))

(transient-define-suffix slackit-transient-root-open (scope)
  "Open the exact conversation captured by the root menu."
  :inapt-if #'slackit-transient--root-entry-inapt-p
  (interactive (list (slackit-transient--root-scope)))
  (let* ((view (slackit-transient--require-view scope))
         (app (appkit-surface-app view))
         (conversation-id (slackit-transient--require-conversation scope)))
    (unless (equal conversation-id
                   (slackit-transient-scope-entry-payload scope))
      (user-error "slackit: the captured directory item is stale"))
    (slackit-room-open app conversation-id t)))

(transient-define-suffix slackit-transient-root-refresh (scope)
  "Refresh the exact account captured by the root menu."
  :inapt-if #'slackit-transient--root-view-inapt-p
  (interactive (list (slackit-transient--root-scope)))
  (slackit-transient--call-in-view scope #'slackit-root-refresh))

(transient-define-suffix slackit-transient-root-stop (scope)
  "Stop the exact app generation captured by the root menu."
  :inapt-if #'slackit-transient--root-view-inapt-p
  (interactive (list (slackit-transient--root-scope)))
  (let ((app (appkit-surface-app (slackit-transient--require-view scope))))
    (unless (slackit-runtime-stop-account app)
      (user-error "slackit: the captured account is no longer running"))))

(transient-define-suffix slackit-transient-root-quit-window (scope)
  "Quit the exact root view window."
  :inapt-if #'slackit-transient--root-quit-inapt-p
  (interactive (list (slackit-transient--root-scope)))
  (slackit-transient--quit-view scope))

(transient-define-suffix slackit-transient-root-describe-mode (scope)
  "Describe the exact root view's mode."
  :inapt-if #'slackit-transient--root-view-inapt-p
  (interactive (list (slackit-transient--root-scope)))
  (slackit-transient--call-in-view scope #'describe-mode))

(transient-define-suffix slackit-transient-room-message-actions (scope)
  "Open message actions for the exact row captured by the room menu."
  :inapt-if #'slackit-transient--room-message-inapt-p
  (interactive (list (slackit-transient--room-scope)))
  (slackit-transient--require-message scope)
  (transient-setup 'slackit-actions-transient nil nil :scope scope))

(transient-define-suffix slackit-transient-room-refresh (scope)
  "Refresh the exact room or thread captured by the menu."
  :inapt-if #'slackit-transient--room-inapt-p
  (interactive (list (slackit-transient--room-scope)))
  (slackit-transient--call-in-view scope #'slackit-room-refresh))

(transient-define-suffix slackit-transient-room-load-older (scope)
  "Load older rows in the exact room or thread."
  :inapt-if #'slackit-transient--room-inapt-p
  (interactive (list (slackit-transient--room-scope)))
  (slackit-transient--call-in-view scope #'slackit-room-load-older))

(transient-define-suffix slackit-transient-room-mark-read (scope)
  "Mark the exact room read through its captured message."
  :inapt-if #'slackit-transient--room-read-inapt-p
  (interactive (list (slackit-transient--room-scope)))
  (slackit-transient--require-message scope)
  (let* ((view (slackit-transient--require-room scope))
         (view-id (slackit-transient-scope-view-id scope))
         (app (appkit-surface-app view))
         (conversation-id
          (slackit-transient-scope-conversation-id scope))
         (ts (slackit-transient-scope-message-ts scope)))
    (unless (eq (car-safe view-id) 'room)
      (user-error "slackit: read marking is unavailable in thread views"))
    (slackit-read-mark app conversation-id ts)
    (message "slackit: marking read through %s" ts)))

(transient-define-suffix slackit-transient-room-submit (scope)
  "Submit the exact captured view's composer."
  :inapt-if #'slackit-transient--room-inapt-p
  (interactive (list (slackit-transient--room-scope)))
  (slackit-transient--call-in-view scope #'slackit-compose-submit))

(transient-define-suffix slackit-transient-room-attach (scope)
  "Choose an attachment action for the exact captured view's composer."
  :inapt-if #'slackit-transient--room-inapt-p
  (interactive (list (slackit-transient--room-scope)))
  (slackit-transient--call-in-view
   scope (lambda () (call-interactively #'slackit-compose-attach))))

(transient-define-suffix slackit-transient-room-remove-attachment (scope)
  "Remove one attachment from the exact captured view's composer."
  :inapt-if #'slackit-transient--room-inapt-p
  (interactive (list (slackit-transient--room-scope)))
  (slackit-transient--call-in-view
   scope #'slackit-compose-remove-attachment))

(transient-define-suffix slackit-transient-room-cancel-context (scope)
  "Cancel the captured view's upload or reply/edit context."
  :inapt-if #'slackit-transient--room-cancel-inapt-p
  (interactive (list (slackit-transient--room-scope)))
  (slackit-transient--call-in-view scope #'slackit-compose-cancel-dwim))

(transient-define-suffix slackit-transient-room-quit-window (scope)
  "Quit the exact room view window."
  :inapt-if #'slackit-transient--room-quit-inapt-p
  (interactive (list (slackit-transient--room-scope)))
  (slackit-transient--quit-view scope))

(transient-define-suffix slackit-transient-room-describe-mode (scope)
  "Describe the exact room view's mode."
  :inapt-if #'slackit-transient--room-inapt-p
  (interactive (list (slackit-transient--room-scope)))
  (slackit-transient--call-in-view scope #'describe-mode))

(transient-define-suffix slackit-transient-actions-open-thread (scope)
  "Open the thread owned by the exact captured message."
  :inapt-if #'slackit-transient--actions-message-inapt-p
  (interactive (list (slackit-transient--actions-scope)))
  (let* ((message (slackit-transient--require-message scope))
         (view (slackit-transient--require-room scope))
         (app (appkit-surface-app view))
         (conversation-id
          (slackit-transient-scope-conversation-id scope))
         (ts (slackit-transient-scope-message-ts scope))
         (root-ts (or (alist-get 'thread_ts message) ts)))
    (slackit-thread-open app conversation-id root-ts t)))

(defun slackit-transient--require-owned-message (scope)
  "Return SCOPE's canonical message after enforcing self ownership."
  (let ((message (slackit-transient--require-message scope)))
    (unless (slackit-transient--message-owned-p scope)
      (user-error "slackit: this action requires your own message"))
    message))

(transient-define-suffix slackit-transient-actions-edit (scope)
  "Edit the exact captured owned message in its source composer."
  :inapt-if #'slackit-transient--actions-edit-inapt-p
  (interactive (list (slackit-transient--actions-scope)))
  (let* ((message (slackit-transient--require-owned-message scope))
         (view (slackit-transient--require-room scope))
         (app (appkit-surface-app view))
         (state (slackit-runtime-state app))
         (ts (slackit-transient-scope-message-ts scope)))
    (with-current-buffer (appkit-surface-buffer view)
      (slackit-transient--require-room scope)
      (unless (appkit-chatbuf-composer-idle-p)
        (user-error "slackit: finish or clear the current composer first"))
      (appkit-chatbuf-aux-set
       (list :aux-type 'edit
             :message-id ts
             :title "Edit message"
             :preview (alist-get 'text message)))
      (appkit-chatbuf-input-set-text
       (slackit-completion-decode-wire
        state (or (alist-get 'text message) "")))
      (appkit-chatbuf-focus-input))))

(transient-define-suffix slackit-transient-actions-delete (scope)
  "Delete the exact captured owned message after confirmation."
  :inapt-if #'slackit-transient--actions-owned-inapt-p
  (interactive (list (slackit-transient--actions-scope)))
  (slackit-transient--require-owned-message scope)
  (when (yes-or-no-p "Delete this Slack message? ")
    ;; Input can run arbitrary command-loop hooks.  Revalidate after it.
    (slackit-transient--require-owned-message scope)
    (let* ((view (slackit-transient--require-room scope))
           (app (appkit-surface-app view))
           (conversation-id
            (slackit-transient-scope-conversation-id scope))
           (ts (slackit-transient-scope-message-ts scope))
           (key (list 'delete conversation-id ts))
           (existing (gethash key (slackit-runtime-operations app))))
      (when (slackit-runtime-operation-current-p app existing)
        (user-error "slackit: delete is already in flight"))
      (let ((operation (slackit-runtime-operation-begin app key)))
        (slackit-api-delete-message
         app conversation-id ts
         :on-success
         (apply-partially #'slackit-actions--delete-success
                          app operation conversation-id ts)
         :on-error
         (lambda (error-data)
           (when (slackit-runtime-operation-current-p app operation)
             (slackit-runtime-operation-end app operation)
             (message "slackit: delete failed: %s"
                      (or (plist-get error-data :code)
                          "request_failed")))))))))

(transient-define-suffix slackit-transient-actions-react (scope name)
  "Toggle reaction NAME on the exact captured message."
  :inapt-if #'slackit-transient--actions-message-inapt-p
  (interactive
   (list (slackit-transient--actions-scope)
         (read-string "Reaction name (without colons): ")))
  (unless (and (stringp name)
               (string-match-p "\\`[+[:alnum:]_-]+\\'" name))
    (user-error "slackit: invalid reaction name"))
  ;; Revalidate after prompting so a replacement app cannot inherit the write.
  (slackit-transient--require-message scope)
  (let* ((view (slackit-transient--require-room scope))
         (app (appkit-surface-app view)))
    (slackit-reaction-toggle
     app
     (slackit-transient-scope-conversation-id scope)
     (slackit-transient-scope-message-ts scope)
     name)))

(transient-define-suffix slackit-transient-actions-mark-read (scope)
  "Mark the exact room read through the captured message."
  :inapt-if #'slackit-transient--actions-read-inapt-p
  (interactive (list (slackit-transient--actions-scope)))
  (slackit-transient--require-message scope)
  (let* ((view (slackit-transient--require-room scope))
         (view-id (slackit-transient-scope-view-id scope))
         (app (appkit-surface-app view))
         (ts (slackit-transient-scope-message-ts scope)))
    (unless (eq (car-safe view-id) 'room)
      (user-error "slackit: read marking is unavailable in thread views"))
    (slackit-read-mark
     app (slackit-transient-scope-conversation-id scope) ts)
    (message "slackit: marking read through %s" ts)))

(transient-define-suffix slackit-transient-actions-copy-text (scope)
  "Copy the exact captured message's current canonical text."
  :inapt-if #'slackit-transient--actions-message-inapt-p
  (interactive (list (slackit-transient--actions-scope)))
  (let* ((message (slackit-transient--require-message scope))
         (text (or (alist-get 'text message) "")))
    (kill-new text)
    (message "slackit: message text copied")))

(defun slackit-transient--call-media-action (scope action)
  "Call captured media ACTION from exact room SCOPE."
  (slackit-transient--require-room scope)
  (let ((context (slackit-transient-scope-media-context scope)))
    (unless context
      (user-error "slackit: no media at captured point"))
    (appkit-media-card-call-action action context)))

(transient-define-suffix slackit-transient-actions-media-open (scope)
  "Open or play captured media."
  :inapt-if #'slackit-transient--actions-media-open-inapt-p
  (interactive (list (slackit-transient--actions-scope)))
  (slackit-transient--call-media-action scope 'open))

(transient-define-suffix slackit-transient-actions-media-download (scope)
  "Download captured media."
  :inapt-if #'slackit-transient--actions-media-download-inapt-p
  (interactive (list (slackit-transient--actions-scope)))
  (slackit-transient--call-media-action scope 'download))

(transient-define-suffix slackit-transient-actions-media-cancel (scope)
  "Cancel captured media download."
  :inapt-if #'slackit-transient--actions-media-cancel-inapt-p
  (interactive (list (slackit-transient--actions-scope)))
  (slackit-transient--call-media-action scope 'cancel))

(transient-define-suffix slackit-transient-actions-media-save (scope)
  "Save captured media locally."
  :inapt-if #'slackit-transient--actions-media-save-inapt-p
  (interactive (list (slackit-transient--actions-scope)))
  (slackit-transient--call-media-action scope 'save-as))
(transient-define-suffix slackit-transient-user-refresh (scope)
  "Refresh the exact captured user profile."
  :inapt-if #'slackit-transient--user-view-inapt-p
  (interactive (list (slackit-transient--user-scope)))
  (slackit-transient--call-in-view scope #'slackit-user-refresh))

(transient-define-suffix slackit-transient-user-message (scope)
  "Open a direct message with the exact captured user."
  :inapt-if #'slackit-transient--user-view-inapt-p
  (interactive (list (slackit-transient--user-scope)))
  (slackit-transient--call-in-view scope #'slackit-user-open-chat))

(transient-define-suffix slackit-transient-user-avatar (scope)
  "Open the exact captured user's cached avatar."
  :inapt-if #'slackit-transient--user-view-inapt-p
  (interactive (list (slackit-transient--user-scope)))
  (slackit-transient--call-in-view scope #'slackit-user-open-avatar))

(transient-define-suffix slackit-transient-user-copy-mention (scope)
  "Copy an exact mention for the captured user."
  :inapt-if #'slackit-transient--user-view-inapt-p
  (interactive (list (slackit-transient--user-scope)))
  (slackit-transient--call-in-view scope #'slackit-user-copy-mention))

(transient-define-suffix slackit-transient-user-copy-id (scope)
  "Copy the exact captured Slack member ID."
  :inapt-if #'slackit-transient--user-view-inapt-p
  (interactive (list (slackit-transient--user-scope)))
  (slackit-transient--call-in-view scope #'slackit-user-copy-id))

(transient-define-suffix slackit-transient-user-quit-window (scope)
  "Quit the exact captured user view window."
  :inapt-if #'slackit-transient--user-quit-inapt-p
  (interactive (list (slackit-transient--user-scope)))
  (slackit-transient--quit-view scope))

;;;###autoload(autoload 'slackit-root-transient "slackit-transient" nil t)
(transient-define-prefix slackit-root-transient (scope)
  "Open commands scoped to one exact Slackit account root."
  [:description slackit-transient--root-description]
  [["Conversation"
    ("RET" "Open captured conversation" slackit-transient-root-open)]
   ["Account"
    ("g" "Refresh this account" slackit-transient-root-refresh)
    ("s" "Stop this account" slackit-transient-root-stop)]
   ["View"
    ("q" "Quit window" slackit-transient-root-quit-window)
    ("?" "Describe mode" slackit-transient-root-describe-mode)]]
  (interactive (list (slackit-transient--capture-root-scope)))
  (unless (and (slackit-transient-scope-p scope)
               (equal (slackit-transient-scope-view-id scope) '(root)))
    (user-error "slackit: invalid root menu scope"))
  (slackit-transient--require-view scope)
  (transient-setup 'slackit-root-transient nil nil :scope scope))

;;;###autoload(autoload 'slackit-room-transient "slackit-transient" nil t)
(transient-define-prefix slackit-room-transient (scope)
  "Open commands scoped to one exact Slackit room or thread."
  [:description slackit-transient--room-description]
  [["Message"
    ("m" "Message at captured row…" slackit-transient-room-message-actions)
    (">" "Mark read through captured row" slackit-transient-room-mark-read)]
   ["Room / thread"
    ("g" "Refresh" slackit-transient-room-refresh)
    ("o" "Load older" slackit-transient-room-load-older)]
   ["Composer"
    ("c" "Send" slackit-transient-room-submit)
    ("a" "Attach…" slackit-transient-room-attach)
    ("d" "Remove attachment" slackit-transient-room-remove-attachment)
    ("k" "Cancel upload/context" slackit-transient-room-cancel-context)]
   ["View"
    ("q" "Quit window" slackit-transient-room-quit-window)
    ("?" "Describe mode" slackit-transient-room-describe-mode)]]
  (interactive (list (slackit-transient--capture-room-scope)))
  (unless (slackit-transient-scope-p scope)
    (user-error "slackit: invalid room menu scope"))
  (slackit-transient--require-room scope)
  (transient-setup 'slackit-room-transient nil nil :scope scope))

;;;###autoload(autoload 'slackit-actions-transient "slackit-transient" nil t)
(transient-define-prefix slackit-actions-transient (scope)
  "Open actions scoped to one exact canonical Slack message."
  [:description slackit-transient--actions-description]
  [["Message"
    ("T" "Open thread" slackit-transient-actions-open-thread)
    ("e" "Edit" slackit-transient-actions-edit)
    ("d" "Delete…" slackit-transient-actions-delete)
    ("r" "Toggle reaction…" slackit-transient-actions-react)
    ("m" "Mark read through here" slackit-transient-actions-mark-read)
    ("w" "Copy text" slackit-transient-actions-copy-text)]
   ["Media"
    ("o" "Open / play" slackit-transient-actions-media-open)
    ("D" "Download" slackit-transient-actions-media-download)
    ("x" "Cancel download" slackit-transient-actions-media-cancel)
    ("s" "Save as…" slackit-transient-actions-media-save)]]
  (interactive (list (slackit-transient--capture-room-scope t)))
  (unless (slackit-transient-scope-p scope)
    (user-error "slackit: invalid message menu scope"))
  (slackit-transient--require-message scope)
  (transient-setup 'slackit-actions-transient nil nil :scope scope))

;;;###autoload(autoload 'slackit-user-transient "slackit-transient" nil t)
(transient-define-prefix slackit-user-transient (scope)
  "Open commands scoped to one exact Slack user profile."
  [:description slackit-transient--user-description]
  [["User"
    ("m" "Message" slackit-transient-user-message)
    ("g" "Refresh profile" slackit-transient-user-refresh)
    ("a" "Open avatar" slackit-transient-user-avatar)]
   ["Copy"
    ("w" "Copy mention" slackit-transient-user-copy-mention)
    ("Y" "Copy member ID" slackit-transient-user-copy-id)]
   ["View"
    ("q" "Quit window" slackit-transient-user-quit-window)]]
  (interactive (list (slackit-transient--capture-user-scope)))
  (unless (slackit-transient--user-view-valid-p scope)
    (user-error "slackit: invalid user menu scope"))
  (transient-setup 'slackit-user-transient nil nil :scope scope))

(provide 'slackit-transient)

;;; slackit-transient.el ends here
