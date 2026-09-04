;;; slackit-runtime.el --- Slackit account ownership runtime -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; One Appkit application, canonical state, credential bundle, transport
;; generation, request table, and view registry per stable local Slack account.

;;; Code:

(require 'appkit-app)
(require 'appkit-surface)
(require 'appkit-projection)
(require 'appkit-command)
(require 'appkit-source)
(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-surface)
(require 'slackit-state)

(cl-defstruct (slackit-runtime-model (:constructor slackit-runtime-model-create))
  state transport operations realtime-p app user-queue user-active)

(cl-defstruct (slackit-credential
               (:constructor slackit-credential-create))
  token
  cookie
  team-id
  user-id)

(cl-defstruct (slackit-transport
               (:constructor slackit-transport-create))
  credential
  generation
  connection-generation
  websocket
  connection
  websocket-url
  hello-timer
  heartbeat-timer
  pong-timer
  reconnect-timer
  reconnect-attempt
  next-message-id
  ready-p
  stopping-p
  source-emit)

(cl-defstruct (slackit-operation
               (:constructor slackit-operation-create))
  key
  nonce
  generation
  model
  view
  composer-revision
  payload)

(defun slackit-runtime-media-error-text ()
  "Return the current Surface's safe committed media error, if any."
  (when-let* ((surface (appkit-current-surface))
              ((appkit-surface-live-p surface))
              (error (plist-get (plist-get (appkit-surface-model surface) :media) :error)))
    (format "Media error: %s" error)))

(put 'slackit-runtime--host-key 'permanent-local t)
(put 'slackit-runtime--host-app 'permanent-local t)

(defvar slackit-runtime--hosts (make-hash-table :test #'equal)
  "Owned presentation hosts keyed by stable account and Surface identity.")

(defvar-local slackit-runtime--host-key nil)
(defvar-local slackit-runtime--host-app nil)

(defun slackit-runtime--host-buffer (app identity mode)
  "Return only an exact stopped presentation host for APP and IDENTITY."
  (let* ((key (list (appkit-app-identity app) identity))
         (buffer (gethash key slackit-runtime--hosts)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (equal key slackit-runtime--host-key)
                   (derived-mode-p mode)
                   (null (appkit-current-surface))
                   (or (eq app slackit-runtime--host-app)
                       (not (appkit-app-live-p slackit-runtime--host-app))))
          buffer)))))

(defun slackit-runtime--remember-host (surface)
  "Remember SURFACE's exact host without retaining live callback authority."
  (let* ((app (appkit-surface-app surface))
         (key (list (appkit-app-identity app) (copy-tree (appkit-surface-identity surface))))
         (buffer (appkit-surface-buffer surface)))
    (with-current-buffer buffer
      (setq-local slackit-runtime--host-key key slackit-runtime--host-app app)
      (puthash key buffer slackit-runtime--hosts)
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (when (eq buffer (gethash key slackit-runtime--hosts))
                    (remhash key slackit-runtime--hosts))) nil t))))

(defun slackit-runtime--initialize-mode (mode)
  "Invoke MODE or the owned derived mode, restoring its structured draft."
  (let* ((owned (and slackit-runtime--host-key (derived-mode-p mode)))
         (actual-mode (if owned major-mode mode))
         (chat (and owned (derived-mode-p 'appkit-chatbuf-mode)))
         (input (and chat (appkit-chatbuf-input-state)))
         (aux (and chat (copy-tree (appkit-chatbuf-aux-state))))
         (history (and chat (appkit-chatbuf-input-history-elements))))
    (funcall actual-mode)
    (when owned
      (let ((inhibit-read-only t) (buffer-undo-list t)) (erase-buffer)))
    (when chat
      (appkit-chatbuf-input-state-set input)
      (appkit-chatbuf-aux-set aux)
      (dolist (entry (reverse history)) (appkit-chatbuf-input-history-push entry)))))

(defconst slackit-runtime--user-concurrency 16
  "Bounded user acquisition slots, leaving App Effect capacity for startup.")

(defun slackit-runtime--queue-user (model operation)
  "Retain OPERATION once until an account user-acquisition slot is available."
  (unless (or (memq operation (slackit-runtime-model-user-queue model))
              (memq operation (slackit-runtime-model-user-active model)))
    (setf (slackit-runtime-model-user-queue model)
          (nconc (slackit-runtime-model-user-queue model) (list operation)))))

(defun slackit-runtime--start-queued-users (app model)
  "Return closed commands admitting queued users within the App Effect bound."
  (let (commands)
    (while (and (slackit-runtime-model-user-queue model)
                (< (length (slackit-runtime-model-user-active model))
                   slackit-runtime--user-concurrency))
      (let ((operation (pop (slackit-runtime-model-user-queue model))))
        (when (slackit-runtime-operation-current-p app operation)
          (push operation (slackit-runtime-model-user-active model))
          (push (appkit-command-start-effect (slackit-runtime--user-effect app operation)) commands))))
    (nreverse commands)))

(defun slackit-runtime--surface-sources (model)
  "Describe only the transport-bearing presentation Sources desired by MODEL."
  (when (plist-get model :audio-inputs)
    (slackit-media-sources model)))

(defun slackit-runtime--user-effect (app operation)
  "Describe a finite account/profile-owned user acquisition."
  (appkit-effect-create
   :key (slackit-operation-key operation) :input (list app operation)
   :start
   (lambda (_context input _observe resolve reject)
     (pcase-let ((`(,app ,operation) input))
       (let ((request
               (slackit-api-user-info
                app (slackit-operation-payload operation)
                :owner (or (slackit-operation-view operation) app)
                :on-success resolve :on-error reject)))
         (when (slackit-http-request-p request)
           (appkit-cancellation-create
            :kind 'transport :cancel (lambda () (slackit-http-cancel request)))))))
   :cancellation-requirement 'transport
   :success (lambda (input body) (list 'slackit-user-result input body nil))
   :failure (lambda (input error) (list 'slackit-user-result input nil error))))

(defun slackit-runtime--user-result (app operation body error-data)
  "Commit a current user result and return its domain changes."
  (when (slackit-runtime-operation-current-p app operation)
    (let ((id (slackit-operation-payload operation))
          (user (alist-get 'user body)))
      (slackit-runtime-operation-end app operation)
      (when (slackit-runtime--user-info-owner-current-p app operation)
        (if (and user (equal id (alist-get 'id user)))
            (progn
              (slackit-state-put-user (slackit-runtime-state app) user)
              (list (list :kind 'user :user-id id)))
          (list (list :kind 'user-profile-error :user-id id
                      :surface (slackit-operation-view operation)
                      :code (or (plist-get error-data :code) "invalid_response"))))))))

(defun slackit-runtime--needed-users (app changes)
  "Find unknown users referenced by committed visible domain CHANGES."
  (let ((state (slackit-runtime-state app)) ids)
    (dolist (change changes)
      (pcase (plist-get change :kind)
        ((or 'bootstrap 'conversation)
         (dolist (id (slackit-state-joined-conversation-ids state))
           (when-let* ((user (alist-get 'user (slackit-state-conversation state id))))
             (push user ids))))
        ((or 'message-create 'message-update)
         (when-let* ((message (slackit-state-message
                               state (plist-get change :conversation-id) (plist-get change :ts)))
                     (user (alist-get 'user message)))
           (push user ids)))))
    (seq-filter (lambda (id)
                  (and (stringp id) (not (string-prefix-p "B" id))
                       (not (slackit-state-user state id))
                       (not (slackit-runtime-user-pending-p app id))))
                (delete-dups ids))))

(defun slackit-runtime--resource-result (surface kind)
  "Return replacing visible Resource demands and dependent row interests."
  (let* ((app (appkit-surface-app surface))
         (state (slackit-runtime-state app))
         (id (appkit-surface-identity surface))
         (demands (make-hash-table :test #'equal))
         (interests (make-hash-table :test #'equal)) order)
    (cl-labels
        ((retain (row demand)
           (when demand
             (let ((key (appkit-resource-demand-key demand)))
               (unless (gethash key demands)
                 (puthash key demand demands)
                 (push key order))
               (puthash key (cons row (delete row (gethash key interests))) interests))))
         (images (row message)
           (when (appkit-media-inline-image-rendering-available-p)
             (dolist (item (slackit-media--message-items app message))
               (retain row (slackit-media-demand app item)))
             (dolist (name (slackit-emoji--message-names message))
               (when-let* ((url (slackit-emoji--custom-url app name)))
                 (retain row (slackit-media-demand
                              app (list :source url :resource-key
                                        (slackit-emoji--image-resource-key app name url)))))))))
      (pcase kind
        ((or 'room 'thread)
         (dolist (message
                  (slackit-history-slice-messages
                   (if (eq kind 'thread)
                       (delq nil (mapcar
                                  (lambda (ts) (slackit-state-message state (cadr id) ts))
                                  (slackit-state-reply-keys state (cadr id) (nth 2 id))))
                     (slackit-room--all-messages state (cadr id)))))
           (let ((row (alist-get 'ts message))
                 (subject (slackit-render-avatar-subject state message)))
             (when (and slackit-show-avatars (display-graphic-p) subject)
               (retain row (slackit-avatar-demand app subject)))
             (images row message))))
        ('user
         (when-let* ((user (slackit-state-user state (cadr id))))
           (when (and slackit-show-avatars (display-graphic-p))
             (retain 'profile (slackit-avatar-demand app user)))
           (images 'profile (list (cons 'text
                                        (alist-get 'status_emoji (alist-get 'profile user)))))))))
    (appkit-render-result-create
     :resource-demands (mapcar (lambda (key) (gethash key demands)) (nreverse order))
     :resource-interest-update
     (appkit-resource-interest-update-create
      :mode 'replace
      :entries (let (entries)
                 (maphash (lambda (key rows)
                            (push (appkit-resource-interest-create :key key :row-keys rows)
                                  entries)) interests)
                 entries)))))

(defvar slackit-runtime--surface-types (make-hash-table :test #'eq)
  "Stable Generated Surface types keyed by protocol presentation kind.")

(defun slackit-runtime--renderer (kind)
  "Create KIND's native Generated Renderer and Resource companion."
  (cl-labels
      ((render (surface _app-view _model request)
         (if (and (memq kind '(room thread))
                  (appkit-projection-change-resources request)
                  (not (or (appkit-projection-change-full-p request)
                           (appkit-projection-change-keys request)
                           (appkit-projection-change-rekeys request)
                           (appkit-projection-change-geometry-p request)
                           (appkit-projection-change-frame-p request)
                           (appkit-projection-change-position request))))
             (progn
               (appkit-chat-timeline-invalidate
                (appkit-chat-timeline-dependent-keys
                 (appkit-projection-change-resources request)))
               (appkit-render-result-create
                :resource-interest-update
                (appkit-resource-interest-update-create :mode 'unchanged)))
           (when (appkit-surface-live-p surface)
             (pcase kind
               ('room (slackit-room--render (appkit-projection-change-keys request)
                                            (appkit-projection-change-resources request)))
               ('thread (slackit-thread--render (appkit-projection-change-keys request)
                                                (appkit-projection-change-resources request)))
               ('root (appkit-directory-reconcile
                       (appkit-directory-surface)
                       (append (slackit-root--entries (slackit-runtime-state (appkit-surface-app surface)))
                               (when-let* ((error (slackit-runtime-media-error-text)))
                                 (list (appkit-directory-entry-create
                                        :key '(media-error) :role 'note :label error :stamp error))))
                       :force-keys (appkit-projection-change-keys request) :preserve-position-p t))
               ('user
                (slackit-user-render)
                (when-let* ((error (slackit-runtime-media-error-text)))
                  (let ((inhibit-read-only t))
                    (save-excursion (goto-char (point-max)) (insert "\n" error "\n")))))))
           (slackit-runtime--resource-result surface kind))))
    (appkit-generated-renderer-create
     :mount
     (lambda (surface _app-view model)
       (pcase (plist-get model :identity)
         ('(root) (slackit-root--setup surface))
         (`(room ,conversation-id)
          (setq-local slackit-room--conversation-id conversation-id))
         (`(thread ,conversation-id ,root-ts)
          (setq-local slackit-room--conversation-id conversation-id
                      slackit-thread--root-ts root-ts))
         (`(user ,user-id) (setq-local slackit-user--user-id user-id))))
     :unmount
     (lambda (_surface)
       (when (memq kind '(room thread))
         (appkit-chat-history-request-cancel)
         (setq-local buffer-read-only nil)))
     :merge #'appkit-projection-change-merge :render #'render
     :resource-request (lambda (keys)
                         (appkit-projection-change-create :resources keys))
     :recover (lambda (surface app-view model _request)
                (render surface app-view model (appkit-projection-change-create :full-p t))))))

(defun slackit-runtime--surface-type (kind mode)
  "Return stable KIND type with derived MODE preserved."
  (or (gethash kind slackit-runtime--surface-types)
      (puthash kind
               (appkit-surface-type-create
                :name mode :mode (lambda () (slackit-runtime--initialize-mode mode)) :init #'slackit-runtime--surface-init
                :update #'slackit-runtime--surface-update :sources #'slackit-runtime--surface-sources
                :renderer-factory (lambda (_surface) (slackit-runtime--renderer kind)))
               slackit-runtime--surface-types)))

(defun slackit-runtime--app-update (context model message)
  "Commit account MESSAGE and route closed Surface commands."
  (if (eq (car-safe message) 'slackit-bootstrap)
      (slackit-bootstrap-update context model message)
    (let ((app (slackit-runtime-model-app model)) changes commands)
      (pcase message
        (`(slackit-cancel-effects ,keys)
         (dolist (key keys) (push (appkit-command-cancel-effect key) commands))
         (setf (slackit-runtime-model-user-active model)
               (seq-remove (lambda (operation) (member (slackit-operation-key operation) keys))
                           (slackit-runtime-model-user-active model))
               (slackit-runtime-model-user-queue model)
               (seq-remove (lambda (operation) (member (slackit-operation-key operation) keys))
                           (slackit-runtime-model-user-queue model))))
        (`(slackit-user-request ,operation)
         (slackit-runtime--queue-user model operation))
        (`(slackit-user-result (,result-app ,operation) ,body ,error-data)
         (when (eq app result-app)
           (setf (slackit-runtime-model-user-active model)
                 (delq operation (slackit-runtime-model-user-active model)))
           (setq changes (slackit-runtime--user-result app operation body error-data))))
        (`(slackit-emoji-start ,operation)
         (push (appkit-command-start-effect (slackit-emoji-catalog-effect app operation)) commands))
        (`(slackit-emoji-result (,result-app ,operation) ,body)
         (when (and (eq app result-app) (slackit-runtime-operation-current-p app operation))
           (slackit-runtime-operation-end app operation)
           (when body
             (slackit-state-set-emojis (slackit-runtime-model-state model) (alist-get 'emoji body))
             (setq changes (list (list :kind 'emoji))))))
        (`(slackit-changes ,value) (setq changes value))
        (`(slackit-event ,event)
         (setq changes (slackit-state-apply-event (slackit-runtime-model-state model) event)))
        ('slackit-realtime-start (setf (slackit-runtime-model-realtime-p model) t))
        (`(slackit-resource ,key)
         (when app
           (maphash (lambda (_id entry)
                      (let ((surface (cdr entry)))
                        (when (appkit-surface-live-p surface)
                          (push (appkit-command-post-message
                                 :target (plist-get (appkit-surface-model surface) :address)
                                 :message (list 'slackit-render
                                                (appkit-projection-change-create :resources (list key)))
                                 :delivery 'report) commands))))
                    (appkit-app-surfaces app)))))
      (when changes
        (dolist (id (slackit-runtime--needed-users app changes))
          (slackit-runtime--queue-user
           model (slackit-runtime-operation-begin app (list 'user id) nil nil id)))
        (maphash
         (lambda (_id entry)
           (let* ((surface (cdr entry))
                  (relevant (and (appkit-surface-live-p surface)
                                 (seq-filter
                                  (lambda (change)
                                    (slackit-runtime--surface-interested-p surface change))
                                  changes))))
             (when relevant
               (push (appkit-command-post-message
                      :target (plist-get (appkit-surface-model surface) :address)
                      :message (list 'slackit-changes relevant) :delivery 'report)
                     commands))))
         (appkit-app-surfaces app)))
      (appkit-next :model model :render appkit-render-none
                   :commands (append (nreverse commands)
                                     (slackit-runtime--start-queued-users app model))))))

(defun slackit-runtime--sources (model)
  "Describe MODEL's live account realtime stream."
  (when (slackit-runtime-model-realtime-p model)
    (let ((app (slackit-runtime-model-app model)))
      (list (appkit-source-spec-create
             :key 'realtime :identity (slackit-runtime-generation app)
             :input app :start #'slackit-realtime--source-start
             :event (lambda (_app event) (list 'slackit-event event))
             :closed (lambda (_app &rest _reason) '(slackit-changes ((:kind connection))))
             :pending-limit 512 :cancellation-requirement 'transport)))))

(defun slackit-runtime--surface-interested-p (surface change)
  "Whether SURFACE projects the domain identity in CHANGE."
  (let* ((id (appkit-surface-identity surface))
         (kind (plist-get change :kind))
         (conversation (plist-get change :conversation-id)))
    (and (or (null (plist-get change :surface))
             (eq surface (plist-get change :surface)))
         (or (eq (car id) 'root)
             (memq kind '(connection bootstrap user user-profile-error emoji conversation))
             (and conversation (equal conversation (cadr id)))))))

(defun slackit-runtime--surface-init (context input)
  "Capture the exact route and immutable Surface identity in INPUT."
  (appkit-next :model (list :identity input
                            :address (appkit-transition-context-owner-address context))
               :render appkit-render-none))

(defun slackit-runtime--surface-update (context model message)
  "Commit Surface controller changes and one explicit projection request."
  (pcase (car-safe message)
    ('slackit-user-request
     (appkit-next :model model :render appkit-render-none
                  :commands (list (appkit-command-start-effect
                                   (slackit-runtime--user-effect
                                    (appkit-surface-app (appkit-current-surface)) (cadr message))))))
    ('slackit-user-result
     (appkit-next :model model :render appkit-render-none
                  :commands (list (appkit-command-post-message
                                   :target (appkit-transition-context-parent-address context)
                                   :message message :delivery 'report))))
    ('slackit-media (slackit-media-update context model message))
    ((or 'slackit-audio-state 'slackit-audio-closed 'slackit-audio-intent)
     (slackit-media-audio-update model message))
    ('slackit-render (appkit-next :model model :render (cadr message)))
    ((or 'slackit-change 'slackit-changes)
     (let ((surface (appkit-current-surface))
           (events (if (eq (car message) 'slackit-change) (list (cadr message)) (cadr message)))
           keys resources)
       (dolist (event events)
         (pcase (car (plist-get model :identity))
           ('room (slackit-room--apply-event surface event))
           ('thread (slackit-thread--apply-event surface event))
           ('user (slackit-user--accept-events (list event))))
         (when-let* ((ts (plist-get event :ts))) (push ts keys))
         (when-let* ((user (plist-get event :user-id))) (push (list :user user) resources))
         (when-let* ((conversation (plist-get event :conversation-id)))
           (push (list :conversation conversation) resources))
         (when (eq (plist-get event :kind) 'emoji)
           (push (slackit-emoji-resource-key (appkit-surface-app surface)) resources)))
       (appkit-next :model model
                    :render (appkit-projection-change-create
                             :full-p t :frame-p t :keys (delete-dups keys)
                             :resources (delete-dups resources)))))
    (_ (appkit-next :model model :render appkit-render-none))))

(defun slackit-runtime-render (surface &optional request)
  "Request committed presentation of live SURFACE."
  (when (appkit-surface-live-p surface)
    (appkit-surface-send surface
                         (list 'slackit-render
                               (or request (appkit-projection-change-create :full-p t))))))

(defun slackit-runtime-deliver (surface event)
  "Deliver a presentation EVENT to its exact live SURFACE."
  (when (appkit-surface-live-p surface)
    (appkit-surface-send surface (list 'slackit-change event))))

(defun slackit-runtime-operations (app)
  "Return APP's account-owned operation fences."
  (slackit-runtime-model-operations (appkit-app-model app)))

(defun slackit-runtime-account-p (app)
  "Whether APP is a Slackit canonical account."
  (and (appkit-app-p app)
       (eq (appkit-app-type-name (appkit-app-type app)) 'slackit-account)))

(defun slackit-runtime--app-init (_context input)
  "Initialize the canonical account INPUT."
  (appkit-next :model input :render appkit-render-none))

(declare-function slackit-api-user-info "slackit-api"
                  (app user-id &rest arguments))

(defvar slackit-runtime--accounts (make-hash-table :test #'equal)
  "Stable local account ID to live Slackit Appkit application.")

(defvar slackit-runtime--operation-nonce 0
  "Monotonic process-local Slackit operation nonce.")

(defun slackit-runtime-account (account-id)
  "Return live Slackit application for ACCOUNT-ID, or nil."
  (let ((app (gethash account-id slackit-runtime--accounts)))
    (and (appkit-app-live-p app) app)))

(defun slackit-runtime-accounts ()
  "Return all live Slackit account applications."
  (let (apps)
    (maphash (lambda (_id app)
               (when (appkit-app-live-p app) (push app apps)))
             slackit-runtime--accounts)
    (sort apps (lambda (left right)
                 (string< (format "%s" (appkit-app-identity left))
                          (format "%s" (appkit-app-identity right)))))))

(defun slackit-runtime-state (app)
  "Return APP's sole canonical protocol state."
  (slackit-runtime-model-state (appkit-app-model app)))

(defun slackit-runtime-transport (app)
  "Return APP's account-owned transport."
  (slackit-runtime-model-transport (appkit-app-model app)))

(defun slackit-runtime-credential (app)
  "Return the credential owned by Slackit APP, or nil."
  (slackit-transport-credential (slackit-runtime-transport app)))

(defun slackit-runtime-credential-cookie-value (app name)
  "Return APP credential cookie NAME without exposing sibling cookies."
  (unless (and (stringp name)
               (string-match-p "\\`[[:alnum:]-]+\\'" name))
    (error "slackit: invalid credential cookie name"))
  (let ((cookie
         (when-let* ((credential (slackit-runtime-credential app)))
           (slackit-credential-cookie credential))))
    (when (stringp cookie)
      (if (and (equal name "d")
               (not (string-match-p "=" cookie)))
          cookie
        (when (string-match
               (format "\\(?:\\`\\|;[[:space:]]*\\)%s=\\([^;]+\\)"
                       (regexp-quote name))
               cookie)
          (match-string 1 cookie))))))

(defun slackit-runtime-bind-credential-identity (app team-id user-id)
  "Bind authenticated TEAM-ID and USER-ID to APP's credential.

Return non-nil after binding missing identity fields.  Return nil without
mutation when either value is empty or conflicts with an already pinned
browser-session identity."
  (let* ((transport (slackit-runtime-transport app))
         (credential (slackit-transport-credential transport))
         (expected-team-id
          (and credential (slackit-credential-team-id credential)))
         (expected-user-id
          (and credential (slackit-credential-user-id credential))))
    (when (and credential
               (stringp team-id)
               (not (string-empty-p team-id))
               (stringp user-id)
               (not (string-empty-p user-id))
               (or (null expected-team-id) (equal expected-team-id team-id))
               (or (null expected-user-id) (equal expected-user-id user-id)))
      (setf (slackit-credential-team-id credential) team-id
            (slackit-credential-user-id credential) user-id)
      t)))

(defun slackit-runtime-generation (app)
  "Return APP's account-lifecycle callback generation."
  (slackit-transport-generation (slackit-runtime-transport app)))

(defun slackit-runtime-connection-generation (app)
  "Return APP's realtime connection-attempt generation."
  (slackit-transport-connection-generation
   (slackit-runtime-transport app)))

(defun slackit-runtime-current-p (app generation)
  "Return non-nil when APP and GENERATION still own callback publication."
  (and (appkit-app-live-p app)
       (let ((transport (slackit-runtime-transport app)))
         (and (slackit-transport-p transport)
              (not (slackit-transport-stopping-p transport))
              (= generation (slackit-transport-generation transport))))))

(defun slackit-runtime--clear-credential (transport)
  "Erase credential references held by TRANSPORT."
  (when-let* ((credential (slackit-transport-credential transport)))
    (setf (slackit-credential-token credential) nil
          (slackit-credential-cookie credential) nil
          (slackit-credential-team-id credential) nil
          (slackit-credential-user-id credential) nil))
  (setf (slackit-transport-credential transport) nil
        (slackit-transport-websocket-url transport) nil))

(defun slackit-runtime--app-shutdown (app)
  "Complete cleanup for stopped Slackit APP."
  (let ((transport (slackit-runtime-transport app))
        (account-id (appkit-app-identity app)))
    (when (slackit-transport-p transport)
      (setf (slackit-transport-websocket transport) nil
            (slackit-transport-connection transport) nil
            (slackit-transport-hello-timer transport) nil
            (slackit-transport-heartbeat-timer transport) nil
            (slackit-transport-pong-timer transport) nil
            (slackit-transport-reconnect-timer transport) nil
            (slackit-transport-ready-p transport) nil)
      (slackit-runtime--clear-credential transport))
    (when (fboundp 'slackit-media--clear-app-specs)
      (slackit-media--clear-app-specs app))
    (setf (slackit-runtime-model-user-queue (appkit-app-model app)) nil
          (slackit-runtime-model-user-active (appkit-app-model app)) nil)
    (clrhash (slackit-runtime-operations app))
    (when (eq app (gethash account-id slackit-runtime--accounts))
      (remhash account-id slackit-runtime--accounts))))

(defconst slackit-runtime--app-type
  (appkit-app-type-create
   :name 'slackit-account :init #'slackit-runtime--app-init
   :update #'slackit-runtime--app-update :sources #'slackit-runtime--sources
   :shutdown #'slackit-runtime--app-shutdown))

(defun slackit-runtime-start-account (account-id credential)
  "Start or return account ACCOUNT-ID using secret CREDENTIAL plist."
  (unless (and (stringp account-id) (not (string-empty-p account-id)))
    (user-error "slackit: account ID must be a non-empty string"))
  (or (slackit-runtime-account account-id)
      (let* ((token (plist-get credential :token))
             (cookie (plist-get credential :cookie))
             (team-id (plist-get credential :team-id))
             (user-id (plist-get credential :user-id)))
        (unless (and (stringp token) (not (string-empty-p token)))
          (user-error "slackit: account %s has no token" account-id))
        (let* ((state (slackit-state-create))
               (transport
                (slackit-transport-create
                 :credential (slackit-credential-create
                              :token token
                              :cookie cookie
                              :team-id team-id
                              :user-id user-id)
                 :generation 1
                 :connection-generation 0
                 :reconnect-attempt 0
                 :next-message-id 1
                 :ready-p nil
                 :stopping-p nil))
               (app (appkit-app-start
                     slackit-runtime--app-type
                     :identity account-id
                     :command-limit 64 :folded-command-limit 2048
                     :input (slackit-runtime-model-create
                             :state state :transport transport
                             :operations (make-hash-table :test #'equal)))))
          (setf (slackit-runtime-model-app (appkit-app-model app)) app)
          (puthash account-id app slackit-runtime--accounts)
          app))))

(defun slackit-runtime-revoke-generation (app)
  "Revoke account callbacks and realtime connection ownership for APP."
  (let ((transport (slackit-runtime-transport app)))
    (setf (slackit-transport-stopping-p transport) t
          (slackit-transport-ready-p transport) nil
          (slackit-transport-generation transport)
          (1+ (slackit-transport-generation transport))
          (slackit-transport-connection-generation transport)
          (1+ (slackit-transport-connection-generation transport)))
    (slackit-state-set-connection-status
     (slackit-runtime-state app) 'stopped)
    (slackit-transport-generation transport)))

(defun slackit-runtime-begin-generation (app)
  "Begin a fresh account lifecycle and realtime generation for live APP."
  (let ((transport (slackit-runtime-transport app)))
    (setf (slackit-transport-generation transport)
          (1+ (slackit-transport-generation transport))
          (slackit-transport-connection-generation transport)
          (1+ (slackit-transport-connection-generation transport))
          (slackit-transport-stopping-p transport) nil
          (slackit-transport-ready-p transport) nil
          (slackit-transport-reconnect-attempt transport) 0)
    (slackit-state-set-connection-status
     (slackit-runtime-state app) 'connecting)
    (slackit-transport-generation transport)))

(defun slackit-runtime-stop-account (account-or-id)
  "Stop Slackit ACCOUNT-OR-ID after revoking callback publication."
  (let ((app (if (appkit-app-p account-or-id)
                 account-or-id
               (slackit-runtime-account account-or-id))))
    (when (appkit-app-live-p app)
      (slackit-runtime-revoke-generation app)
      (appkit-app-close app)
      t)))

(defun slackit-runtime-stop-all ()
  "Stop every live Slackit account."
  (dolist (app (slackit-runtime-accounts))
    (slackit-runtime-stop-account app))
  t)

(defun slackit-runtime--secret-values (app)
  "Return secret strings currently owned by APP."
  (let* ((transport (and (appkit-app-p app) (slackit-runtime-transport app)))
         (credential (and (slackit-transport-p transport)
                          (slackit-transport-credential transport))))
    (delq nil
          (list (and credential (slackit-credential-token credential))
                (and credential (slackit-credential-cookie credential))
                (and transport (slackit-transport-websocket-url transport))))))

(defun slackit-runtime-redact (app value)
  "Return printable VALUE with APP secrets and credential syntax redacted."
  (let ((text (format "%s" (or value ""))))
    (dolist (secret (slackit-runtime--secret-values app))
      (when (and (stringp secret) (not (string-empty-p secret)))
        (setq text (replace-regexp-in-string
                    (regexp-quote secret) "[REDACTED]" text t t))))
    (dolist (pattern '("\\bBearer[[:space:]]+[^[:space:]\r\n]+"
                       "\\bxox[a-z]-[[:alnum:]-]+"
                       "[?&]token=[^&#[:space:]]+"
                       "\\bd=[^;[:space:]]+"))
      (setq text (replace-regexp-in-string pattern "[REDACTED]" text t)))
    text))

(defun slackit-runtime-operation-begin
    (app key &optional view composer-revision payload)
  "Begin latest account-owned operation KEY in APP."
  (let* ((table (slackit-runtime-operations app))
         (operation
          (slackit-operation-create
           :key key
           :nonce (cl-incf slackit-runtime--operation-nonce)
           :generation (slackit-runtime-generation app)
           :model (appkit-app-model app)
           :view view
           :composer-revision composer-revision
           :payload payload)))
    (puthash key operation table)
    operation))

(defun slackit-runtime-operation-current-p (app operation)
  "Return non-nil when OPERATION is still latest and current in APP."
  (and (appkit-app-p app)
       (slackit-operation-p operation)
       (eq (slackit-operation-model operation) (appkit-app-model app))
       (slackit-runtime-current-p app
                                  (slackit-operation-generation operation))
       (eq operation
           (gethash (slackit-operation-key operation)
                    (slackit-runtime-operations app)))))

(defun slackit-runtime-operation-end (app operation)
  "Retire current OPERATION from APP and return non-nil when retired."
  (when (slackit-runtime-operation-current-p app operation)
    (remhash (slackit-operation-key operation) (slackit-runtime-operations app))
    t))

(defun slackit-runtime--user-view-current-p (app view user-id)
  "Return non-nil when VIEW is APP's exact live USER-ID profile view."
  (let ((view-id (list 'user user-id)))
    (and (appkit-surface-live-p view)
         (eq (appkit-surface-app view) app)
         (equal (appkit-surface-identity view) view-id)
         (eq view (appkit-app-surface app view-id)))))

(defun slackit-runtime--user-info-owner-current-p (app operation)
  "Return non-nil when OPERATION's optional profile view remains current."
  (let ((view (slackit-operation-view operation))
        (user-id (slackit-operation-payload operation)))
    (or (null view)
        (slackit-runtime--user-view-current-p app view user-id))))

(defun slackit-runtime-user-pending-p (app user-id)
  "Return non-nil when APP owns a current USER-ID profile request."
  (let ((operation
         (and (appkit-app-live-p app)
              (gethash (list 'user user-id)
                       (slackit-runtime-operations app)))))
    (and (slackit-runtime-operation-current-p app operation) operation)))

(cl-defun slackit-runtime-ensure-user (app user-id &key force view)
  "Acquire USER-ID under the exact account or initiating profile Surface."
  (when (and (appkit-app-live-p app) (stringp user-id)
             (not (string-empty-p user-id)) (not (string-prefix-p "B" user-id))
             (or force (not (slackit-state-user (slackit-runtime-state app) user-id))))
    (let* ((key (list 'user user-id))
           (surface (and (slackit-runtime--user-view-current-p app view user-id) view))
           (pending (gethash key (slackit-runtime-operations app))))
      (if (and (slackit-runtime-operation-current-p app pending)
               (or (null surface) (eq surface (slackit-operation-view pending))))
          pending
        (when (slackit-runtime-operation-current-p app pending)
          (appkit-app-send app (list 'slackit-cancel-effects (list key))))
        (let ((operation (slackit-runtime-operation-begin app key surface nil user-id)))
          (if surface
              (appkit-surface-send surface (list 'slackit-user-request operation))
            (appkit-app-send app (list 'slackit-user-request operation)))
          operation)))))

(defun slackit-runtime-publish-changes (app changes)
  "Commit CHANGES publication through APP's closed routing commands."
  (when (appkit-app-live-p app)
    (appkit-app-send app (list 'slackit-changes changes)))
  changes)

(defun slackit-runtime-publish-resource (app resource)
  "Publish RESOURCE presentation changes through APP."
  (when (appkit-app-live-p app)
    (appkit-app-send app (list 'slackit-resource resource)))
  resource)

(defun slackit-runtime-reduce-event (app event)
  "Commit normalized EVENT through its exact App."
  (when (appkit-app-live-p app)
    (appkit-app-send app (list 'slackit-event event))))

(defun slackit-runtime-publish-bootstrap (app)
  "Publish current APP bootstrap state to its views."
  (slackit-runtime-publish-changes
   app (list (list :kind 'bootstrap))))

(provide 'slackit-runtime)

;;; slackit-runtime.el ends here
