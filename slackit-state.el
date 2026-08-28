;;; slackit-state.el --- Canonical Slackit account state -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Account-local protocol facts, mutation revisions, tombstones, indexes, and
;; idempotent reducers.  This module never renders or mutates buffers.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'slackit-normalize)

(cl-defstruct (slackit-account-state
               (:constructor slackit-state--create))
  team
  self
  users
  emojis
  conversations
  conversation-order
  messages
  top-level
  replies
  tombstones
  message-revisions
  read-state
  revision
  connection-status
  bootstrap-identity-complete-p
  bootstrap-users-complete-p
  bootstrap-conversations-complete-p
  bootstrap-error)

(defun slackit-state-create ()
  "Return a new empty canonical Slackit account state."
  (slackit-state--create
   :users (make-hash-table :test #'equal)
   :emojis (make-hash-table :test #'equal)
   :conversations (make-hash-table :test #'equal)
   :conversation-order nil
   :messages (make-hash-table :test #'equal)
   :top-level (make-hash-table :test #'equal)
   :replies (make-hash-table :test #'equal)
   :tombstones (make-hash-table :test #'equal)
   :message-revisions (make-hash-table :test #'equal)
   :read-state (make-hash-table :test #'equal)
   :revision 0
   :connection-status 'stopped
   :bootstrap-identity-complete-p nil
   :bootstrap-users-complete-p nil
   :bootstrap-conversations-complete-p nil
   :bootstrap-error nil))

(defun slackit-state--touch (state)
  "Advance and return STATE's mutation revision."
  (setf (slackit-account-state-revision state)
        (1+ (slackit-account-state-revision state))))

(defun slackit-state-bootstrap-reset (state)
  "Reset STATE bootstrap readiness before a fresh cursor walk."
  (setf (slackit-account-state-bootstrap-identity-complete-p state) nil
        (slackit-account-state-bootstrap-users-complete-p state) nil
        (slackit-account-state-bootstrap-conversations-complete-p state) nil
        (slackit-account-state-bootstrap-error state) nil)
  (slackit-state--touch state))

(defun slackit-state-bootstrap-ready-p (state)
  "Return non-nil when STATE has complete identity and directory data."
  (and (slackit-account-state-bootstrap-identity-complete-p state)
       (slackit-account-state-bootstrap-users-complete-p state)
       (slackit-account-state-bootstrap-conversations-complete-p state)
       (null (slackit-account-state-bootstrap-error state))))

(defun slackit-state-set-bootstrap-complete (state kind)
  "Mark bootstrap KIND complete in STATE."
  (pcase kind
    ('identity
     (setf (slackit-account-state-bootstrap-identity-complete-p state) t))
    ('users (setf (slackit-account-state-bootstrap-users-complete-p state) t))
    ('conversations
     (setf (slackit-account-state-bootstrap-conversations-complete-p state) t))
    (_ (error "slackit: unknown bootstrap kind %S" kind)))
  (setf (slackit-account-state-bootstrap-error state) nil)
  (slackit-state--touch state))

(defun slackit-state-set-bootstrap-error (state error-code)
  "Record redacted bootstrap ERROR-CODE in STATE."
  (setf (slackit-account-state-bootstrap-error state) error-code)
  (slackit-state--touch state))

(defun slackit-state-set-connection-status (state status)
  "Set canonical connection STATUS in STATE."
  (unless (eq status (slackit-account-state-connection-status state))
    (setf (slackit-account-state-connection-status state) status)
    (slackit-state--touch state))
  status)

(defun slackit-state-put-team-self (state team self)
  "Store normalized TEAM and SELF facts in STATE."
  (setf (slackit-account-state-team state) (slackit-normalize-object team)
        (slackit-account-state-self state) (slackit-normalize-user self))
  (slackit-state--touch state))

(defun slackit-state-self-id (state)
  "Return the current user's Slack ID in STATE."
  (slackit-normalize-get (slackit-account-state-self state) 'id))

(defun slackit-state-put-user (state user)
  "Upsert normalized Slack USER into STATE and return its ID."
  (let* ((normalized (slackit-normalize-user user))
         (id (slackit-normalize-get normalized 'id)))
    (when (and id (not (string-empty-p id)))
      (puthash id normalized (slackit-account-state-users state))
      (slackit-state--touch state)
      id)))

(defun slackit-state-put-users (state users)
  "Upsert Slack USERS into STATE and return their IDs."
  (delq nil (mapcar (lambda (user) (slackit-state-put-user state user)) users)))

(defun slackit-state-user (state user-id)
  "Return canonical user USER-ID from STATE."
  (and user-id (gethash user-id (slackit-account-state-users state))))

(defun slackit-state-set-emojis (state emojis)
  "Replace STATE's account custom EMOJIS and return their count."
  (let ((table (slackit-account-state-emojis state)))
    (clrhash table)
    (dolist (entry emojis)
      (let ((name (cond
                   ((symbolp (car-safe entry)) (symbol-name (car entry)))
                   ((stringp (car-safe entry)) (car entry))))
            (value (cdr-safe entry)))
        (when (and name
                   (string-match-p "\\`[+[:alnum:]_-]+\\'" name)
                   (stringp value)
                   (not (string-empty-p value)))
          (puthash name value table))))
    (slackit-state--touch state)
    (hash-table-count table)))

(defun slackit-state-emoji (state name)
  "Return custom emoji value for exact NAME in STATE, or nil."
  (and (stringp name)
       (gethash name (slackit-account-state-emojis state))))

(defun slackit-state-user-name (state user-id)
  "Return a stable display name for USER-ID in STATE."
  (let* ((user (slackit-state-user state user-id))
         (profile (slackit-normalize-get user 'profile)))
    (or (cl-loop
         for value in
         (list (slackit-normalize-get profile 'display_name)
               (slackit-normalize-get profile 'real_name)
               (slackit-normalize-get user 'real_name)
               (slackit-normalize-get user 'name)
               user-id)
         when (and (stringp value) (not (string-blank-p value)))
         return value)
        "unknown")))

(defun slackit-state--merge-alists (old new)
  "Return OLD with fields present in NEW replaced."
  (let ((result (copy-tree old)))
    (dolist (entry new result)
      (setq result (assq-delete-all (car entry) result))
      (push (copy-tree entry) result))))

(defun slackit-state-put-conversation (state conversation)
  "Upsert Slack CONVERSATION into STATE and return its ID."
  (let* ((normalized (slackit-normalize-conversation conversation))
         (id (slackit-normalize-get normalized 'id))
         (old (and id (gethash id (slackit-account-state-conversations state)))))
    (when (and id (not (string-empty-p id)))
      (puthash id (if old
                      (slackit-state--merge-alists old normalized)
                    normalized)
               (slackit-account-state-conversations state))
      (unless (member id (slackit-account-state-conversation-order state))
        (setf (slackit-account-state-conversation-order state)
              (append (slackit-account-state-conversation-order state)
                      (list id))))
      (when-let* ((last-read (slackit-normalize-get normalized 'last_read)))
        (slackit-state-mark-read state id (format "%s" last-read)))
      (slackit-state--touch state)
      id)))

(defun slackit-state-put-conversations (state conversations)
  "Upsert Slack CONVERSATIONS into STATE and return their IDs."
  (delq nil
        (mapcar (lambda (conversation)
                  (slackit-state-put-conversation state conversation))
                conversations)))

(defun slackit-state-conversation (state conversation-id)
  "Return canonical CONVERSATION-ID from STATE."
  (and conversation-id
       (gethash conversation-id
                (slackit-account-state-conversations state))))

(defun slackit-state-conversation-name (state conversation-id)
  "Return display name for CONVERSATION-ID in STATE."
  (let* ((conversation (slackit-state-conversation state conversation-id))
         (user-id (slackit-normalize-get conversation 'user)))
    (cond
     ((slackit-normalize-get conversation 'name)
      (slackit-normalize-get conversation 'name))
     (user-id (slackit-state-user-name state (format "%s" user-id)))
     (conversation-id conversation-id)
     (t "unknown"))))

(defun slackit-state-conversation-label (state conversation-id)
  "Return presentation label for CONVERSATION-ID in STATE."
  (let ((conversation
         (slackit-state-conversation state conversation-id))
        (name (slackit-state-conversation-name state conversation-id)))
    (if (or (slackit-normalize-get conversation 'is_im)
            (slackit-normalize-get conversation 'is_mpim))
        name
      (concat "#" name))))

(defun slackit-state-joined-conversation-ids (state)
  "Return visible joined conversation IDs in stable STATE order."
  (seq-filter
   (lambda (id)
     (let ((conversation (slackit-state-conversation state id)))
       (and conversation
            (not (slackit-normalize-get conversation 'is_archived))
            (not (eq (slackit-normalize-get conversation 'is_member :missing)
                     nil)))))
   (slackit-account-state-conversation-order state)))

(defun slackit-state--message-key (conversation-id ts)
  "Return canonical message key for CONVERSATION-ID and TS."
  (cons conversation-id ts))

(defun slackit-state--message-table (state conversation-id &optional create)
  "Return STATE message table for CONVERSATION-ID, optionally CREATE it."
  (or (gethash conversation-id (slackit-account-state-messages state))
      (when create
        (let ((table (make-hash-table :test #'equal)))
          (puthash conversation-id table (slackit-account-state-messages state))
          table))))

(defun slackit-state-message (state conversation-id ts)
  "Return canonical message TS in CONVERSATION-ID from STATE."
  (when-let* ((table (slackit-state--message-table state conversation-id)))
    (gethash ts table)))

(defun slackit-state--sorted-insert (key keys)
  "Insert string KEY into sorted unique KEYS."
  (cond
   ((null keys) (list key))
   ((equal key (car keys)) keys)
   ((string< key (car keys)) (cons key keys))
   (t
    (let ((tail keys))
      (while (and (cdr tail) (string< (cadr tail) key))
        (setq tail (cdr tail)))
      (unless (and (cdr tail) (equal key (cadr tail)))
        (setcdr tail (cons key (cdr tail))))
      keys))))

(defun slackit-state--index-remove (table key value)
  "Remove VALUE from TABLE's sequence at KEY."
  (let ((values (delete value (gethash key table))))
    (if values (puthash key values table) (remhash key table))))

(defun slackit-state--remove-message-indexes (state conversation-id ts old)
  "Remove OLD message TS from all STATE indexes."
  (slackit-state--index-remove
   (slackit-account-state-top-level state) conversation-id ts)
  (when-let* ((root-ts (slackit-normalize-get old 'thread_ts)))
    (slackit-state--index-remove
     (slackit-account-state-replies state)
     (cons conversation-id root-ts)
     ts)))

(defun slackit-state--add-message-indexes (state conversation-id ts message)
  "Add MESSAGE TS to its STATE projection indexes."
  (let* ((thread-ts (slackit-normalize-get message 'thread_ts))
         (subtype (slackit-normalize-get message 'subtype))
         (reply-p (and thread-ts (not (equal thread-ts ts))))
         (top-level-p (or (not reply-p) (equal subtype "thread_broadcast"))))
    (when top-level-p
      (puthash conversation-id
               (slackit-state--sorted-insert
                ts (gethash conversation-id
                            (slackit-account-state-top-level state)))
               (slackit-account-state-top-level state)))
    (when reply-p
      (let ((key (cons conversation-id thread-ts)))
        (puthash key
                 (slackit-state--sorted-insert
                  ts (gethash key (slackit-account-state-replies state)))
                 (slackit-account-state-replies state))))
    (list :reply-p reply-p :top-level-p top-level-p :root-ts thread-ts)))

(defun slackit-state--upsert-normalized-message
    (state conversation-id normalized &optional revision)
  "Upsert NORMALIZED in CONVERSATION-ID and return a change descriptor.

REVISION, when non-nil, is the page settlement revision applied to the
message; ordinary realtime and write results allocate a new revision."
  (let ((ts (slackit-normalize-get normalized 'ts)))
    (when (and conversation-id ts)
      (let* ((table (slackit-state--message-table state conversation-id t))
             (old (gethash ts table))
             (merged (if old
                         (slackit-state--merge-alists old normalized)
                       normalized))
             (effective-revision (or revision (slackit-state--touch state))))
        (when old
          (slackit-state--remove-message-indexes state conversation-id ts old))
        (puthash ts merged table)
        (remhash (slackit-state--message-key conversation-id ts)
                 (slackit-account-state-tombstones state))
        (puthash (slackit-state--message-key conversation-id ts)
                 effective-revision
                 (slackit-account-state-message-revisions state))
        (append (list :kind (if old 'message-update 'message-create)
                      :conversation-id conversation-id
                      :ts ts)
                (slackit-state--add-message-indexes
                 state conversation-id ts merged))))))

(defun slackit-state-upsert-message (state conversation-id message &optional revision)
  "Normalize and upsert MESSAGE in CONVERSATION-ID.

Return a canonical change descriptor.  REVISION, when non-nil, is the page
settlement revision; ordinary realtime and write results allocate one."
  (slackit-state--upsert-normalized-message
   state conversation-id
   (slackit-normalize-message message conversation-id)
   revision))

(defun slackit-state-message-revision (state conversation-id ts)
  "Return STATE revision for message identity CONVERSATION-ID and TS."
  (gethash
   (slackit-state--message-key conversation-id ts)
   (slackit-account-state-message-revisions state)))

(defun slackit-state-merge-write-snapshot
    (state conversation-id message captured-revision)
  "Merge HTTP write MESSAGE without crossing newer canonical mutations.

CAPTURED-REVISION is the target message revision observed when an edit began.
For a newly posted message it is nil, so an RTM echo that arrived first remains
authoritative.  No HTTP write response may clear an observed tombstone."
  (let* ((normalized (slackit-normalize-message message conversation-id))
         (ts (slackit-normalize-get normalized 'ts))
         (key (and ts (slackit-state--message-key conversation-id ts)))
         (current-revision
          (and key
               (gethash key
                        (slackit-account-state-message-revisions state))))
         (tombstoned-p
          (and key (gethash key (slackit-account-state-tombstones state)))))
    (when (and ts
               (not tombstoned-p)
               (if captured-revision
                   (or (null current-revision)
                       (<= current-revision captured-revision))
                 (null current-revision)))
      (slackit-state--upsert-normalized-message
       state conversation-id normalized))))

(defun slackit-state-delete-message (state conversation-id ts)
  "Delete message TS from CONVERSATION-ID and record a tombstone."
  (when (and conversation-id ts)
    (let* ((table (slackit-state--message-table state conversation-id))
           (old (and table (gethash ts table)))
           (revision (slackit-state--touch state)))
      (when old
        (slackit-state--remove-message-indexes state conversation-id ts old)
        (remhash ts table))
      (puthash (slackit-state--message-key conversation-id ts)
               revision
               (slackit-account-state-tombstones state))
      (puthash (slackit-state--message-key conversation-id ts)
               revision
               (slackit-account-state-message-revisions state))
      (list :kind 'message-delete
            :conversation-id conversation-id
            :ts ts
            :root-ts (and old (slackit-normalize-get old 'thread_ts))))))

(defun slackit-state-merge-message-page (state conversation-id messages captured-revision)
  "Merge history MESSAGES without crossing CAPTURED-REVISION mutations."
  (let ((page-revision (slackit-state--touch state))
        changes)
    (dolist (message messages (nreverse changes))
      (let* ((normalized (slackit-normalize-message message conversation-id))
             (ts (slackit-normalize-get normalized 'ts))
             (key (slackit-state--message-key conversation-id ts))
             (message-revision
              (gethash key
                       (slackit-account-state-message-revisions state)
                       -1))
             (tombstoned-p
              (gethash key (slackit-account-state-tombstones state))))
        (when (and ts
                   (<= message-revision captured-revision)
                   (not tombstoned-p))
          (when-let* ((change
                       (slackit-state--upsert-normalized-message
                        state conversation-id normalized page-revision)))
            (push change changes)))))))

(defun slackit-state-top-level-keys (state conversation-id)
  "Return a copy of top-level message keys for CONVERSATION-ID."
  (copy-sequence
   (gethash conversation-id (slackit-account-state-top-level state))))

(defun slackit-state-reply-keys (state conversation-id root-ts)
  "Return root plus reply keys for CONVERSATION-ID and ROOT-TS."
  (let ((root (and (slackit-state-message state conversation-id root-ts)
                   (list root-ts))))
    (append root
            (copy-sequence
             (gethash (cons conversation-id root-ts)
                      (slackit-account-state-replies state))))))

(defun slackit-state--reaction-index (reactions name)
  "Return reaction NAME index in REACTIONS, or nil."
  (cl-position name reactions
               :test #'equal
               :key (lambda (reaction)
                      (slackit-normalize-get reaction 'name))))

(defun slackit-state-apply-reaction (state conversation-id ts name user-id add-p)
  "Apply idempotent reaction event to message TS and return descriptor."
  (when-let* ((message (slackit-state-message state conversation-id ts)))
    (let* ((reactions (copy-tree (or (slackit-normalize-get message 'reactions) nil)))
           (index (slackit-state--reaction-index reactions name))
           (reaction (and index (nth index reactions)))
           (users (copy-sequence (or (slackit-normalize-get reaction 'users) nil)))
           (present-p (member user-id users))
           (changed-p (if add-p (not present-p) present-p)))
      (when changed-p
        (if add-p
            (if reaction
                (progn
                  (setcdr (assq 'users reaction) (cons user-id users))
                  (setcdr (assq 'count reaction)
                           (1+ (or (slackit-normalize-get reaction 'count) 0))))
              (push `((name . ,name) (count . 1) (users . (,user-id))) reactions))
          (let ((count (max 0 (1- (or (slackit-normalize-get reaction 'count) 0)))))
            (setcdr (assq 'users reaction) (delete user-id users))
            (setcdr (assq 'count reaction) count)
            (when (zerop count)
              (setq reactions (delete reaction reactions)))))
        (setq message (assq-delete-all 'reactions (copy-tree message)))
        (push (cons 'reactions reactions) message)
        (slackit-state-upsert-message state conversation-id message)
        (list :kind 'reaction
              :conversation-id conversation-id
              :ts ts
              :reaction name)))))

(defun slackit-state-mark-read (state conversation-id ts)
  "Advance CONVERSATION-ID read frontier in STATE monotonically to TS."
  (let ((current (gethash conversation-id
                          (slackit-account-state-read-state state))))
    (when (and ts (or (null current) (string< current ts)))
      (puthash conversation-id ts (slackit-account-state-read-state state))
      (slackit-state--touch state)
      (list :kind 'read
            :conversation-id conversation-id
            :ts ts))))

(defun slackit-state-read-ts (state conversation-id)
  "Return known read timestamp for CONVERSATION-ID, or nil."
  (gethash conversation-id (slackit-account-state-read-state state)))

(defun slackit-state-apply-event (state event)
  "Reduce normalized EVENT into STATE and return change descriptors."
  (pcase (plist-get event :kind)
    ('hello
     (slackit-state-set-connection-status state 'ready)
     (list (list :kind 'connection)))
    ((or 'message-create 'message-change)
     (when-let* ((change (slackit-state-upsert-message
                          state
                          (plist-get event :conversation-id)
                          (plist-get event :message))))
       (list change)))
    ('message-delete
     (when-let* ((change (slackit-state-delete-message
                          state
                          (plist-get event :conversation-id)
                          (plist-get event :ts))))
       (list change)))
    ('reaction-add
     (when-let* ((change (slackit-state-apply-reaction
                          state
                          (plist-get event :conversation-id)
                          (plist-get event :ts)
                          (plist-get event :reaction)
                          (plist-get event :user-id)
                          t)))
       (list change)))
    ('reaction-remove
     (when-let* ((change (slackit-state-apply-reaction
                          state
                          (plist-get event :conversation-id)
                          (plist-get event :ts)
                          (plist-get event :reaction)
                          (plist-get event :user-id)
                          nil)))
       (list change)))
    ('conversation-mark
     (when-let* ((change (slackit-state-mark-read
                          state
                          (plist-get event :conversation-id)
                          (plist-get event :ts))))
       (list change)))
    ('user-upsert
     (when-let* ((id (slackit-state-put-user state (plist-get event :user))))
       (list (list :kind 'user :user-id id))))
    ('conversation-upsert
     (when-let* ((id (slackit-state-put-conversation
                      state (plist-get event :conversation))))
       (list (list :kind 'conversation :conversation-id id))))
    ('conversation-archive
     (let* ((id (plist-get event :conversation-id))
            (conversation (copy-tree (slackit-state-conversation state id))))
       (when conversation
         (setq conversation (assq-delete-all 'is_archived conversation))
         (push (cons 'is_archived (plist-get event :archived-p)) conversation)
         (slackit-state-put-conversation state conversation)
         (list (list :kind 'conversation :conversation-id id)))))
    ('conversation-leave
     (let* ((id (plist-get event :conversation-id))
            (conversation (copy-tree (slackit-state-conversation state id))))
       (when conversation
         (setq conversation (assq-delete-all 'is_member conversation))
         (push '(is_member . nil) conversation)
         (slackit-state-put-conversation state conversation)
         (list (list :kind 'conversation :conversation-id id)))))
    (_ nil)))

(provide 'slackit-state)

;;; slackit-state.el ends here
