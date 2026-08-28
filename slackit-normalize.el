;;; slackit-normalize.el --- Slack payload normalization -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Convert Slack Web API and RTM payloads into stable plain alists and
;; classify nested message envelopes before state reduction.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defun slackit-normalize--key (key)
  "Return canonical symbol for JSON object KEY."
  (cond
   ((keywordp key) (intern (substring (symbol-name key) 1)))
   ((symbolp key) key)
   ((stringp key) (intern key))
   (t key)))

(defun slackit-normalize--alist-p (value)
  "Return non-nil when VALUE is a JSON-style alist."
  (and (consp value)
       (cl-every (lambda (entry)
                   (and (consp entry)
                        (or (symbolp (car entry))
                            (stringp (car entry)))))
                 value)))

(defun slackit-normalize-object (value)
  "Recursively normalize JSON VALUE to alists, lists, and scalar values."
  (cond
   ((hash-table-p value)
    (let (result)
      (maphash (lambda (key item)
                 (push (cons (slackit-normalize--key key)
                             (slackit-normalize-object item))
                       result))
               value)
      (nreverse result)))
   ((vectorp value)
    (mapcar #'slackit-normalize-object (append value nil)))
   ((slackit-normalize--alist-p value)
    (mapcar (lambda (entry)
              (cons (slackit-normalize--key (car entry))
                    (slackit-normalize-object (cdr entry))))
            value))
   ((listp value)
    (mapcar #'slackit-normalize-object value))
   ((memq value '(:false :json-false)) nil)
   (t value)))

(defun slackit-normalize-json (text)
  "Decode JSON TEXT as a normalized Slack alist."
  (slackit-normalize-object
   (json-parse-string text
                      :object-type 'alist
                      :array-type 'list
                      :null-object nil
                      :false-object nil)))

(defun slackit-normalize-get (object key &optional default)
  "Return KEY from normalized alist OBJECT, or DEFAULT."
  (if (listp object)
      (let ((entry (assq key object)))
        (if entry (cdr entry) default))
    default))

(defun slackit-normalize--string-field (object key)
  "Return OBJECT's KEY as a string when present."
  (when-let* ((value (slackit-normalize-get object key)))
    (format "%s" value)))

(defun slackit-normalize--safe-file (file)
  "Return non-fetching canonical summary for Slack FILE."
  (let ((normalized (slackit-normalize-object file)))
    (delq nil
          (mapcar (lambda (key)
                    (when-let* ((value (slackit-normalize-get normalized key)))
                      (cons key value)))
                  '(id name title mimetype filetype size permalink
                       permalink_public mode pretty_type)))))

(defun slackit-normalize--reaction (reaction)
  "Return canonical Slack REACTION alist."
  (let ((normalized (slackit-normalize-object reaction)))
    `((name . ,(slackit-normalize--string-field normalized 'name))
      (count . ,(or (slackit-normalize-get normalized 'count) 0))
      (users . ,(mapcar (lambda (id) (format "%s" id))
                        (or (slackit-normalize-get normalized 'users) nil))))))

(defun slackit-normalize-message (message &optional conversation-id)
  "Return canonical Slack MESSAGE for CONVERSATION-ID."
  (let* ((result (copy-tree (slackit-normalize-object message)))
         (channel (or conversation-id
                      (slackit-normalize--string-field result 'channel)))
         (ts (slackit-normalize--string-field result 'ts))
         (thread-ts (slackit-normalize--string-field result 'thread_ts))
         (user (slackit-normalize--string-field result 'user))
         (bot-id (slackit-normalize--string-field result 'bot_id))
         (files (slackit-normalize-get result 'files))
         (reactions (slackit-normalize-get result 'reactions)))
    (dolist (key '(channel ts thread_ts user bot_id files reactions))
      (setq result (assq-delete-all key result)))
    (append
     `((channel . ,channel)
       (ts . ,ts)
       (thread_ts . ,thread-ts)
       (user . ,user)
       (bot_id . ,bot-id)
       (files . ,(mapcar #'slackit-normalize--safe-file (or files nil)))
       (reactions . ,(mapcar #'slackit-normalize--reaction
                              (or reactions nil))))
     result)))

(defun slackit-normalize-user (user)
  "Return canonical Slack USER."
  (let ((result (slackit-normalize-object user)))
    (cons (cons 'id (slackit-normalize--string-field result 'id))
          (assq-delete-all 'id result))))

(defun slackit-normalize-conversation (conversation)
  "Return canonical Slack CONVERSATION."
  (let ((result (slackit-normalize-object conversation)))
    (cons (cons 'id (slackit-normalize--string-field result 'id))
          (assq-delete-all 'id result))))

(defun slackit-normalize--message-event (payload)
  "Classify normalized Slack message event PAYLOAD."
  (let* ((subtype (slackit-normalize-get payload 'subtype))
         (channel (slackit-normalize--string-field payload 'channel)))
    (cond
     ((equal subtype "message_changed")
      (list :kind 'message-change
            :conversation-id channel
            :message (slackit-normalize-message
                      (slackit-normalize-get payload 'message) channel)
            :previous (slackit-normalize-message
                       (slackit-normalize-get payload 'previous_message) channel)))
     ((equal subtype "message_deleted")
      (list :kind 'message-delete
            :conversation-id channel
            :ts (or (slackit-normalize--string-field payload 'deleted_ts)
                    (slackit-normalize--string-field
                     (slackit-normalize-get payload 'previous_message) 'ts))))
     ((equal subtype "message_replied")
      (list :kind 'message-change
            :conversation-id channel
            :message (slackit-normalize-message
                      (slackit-normalize-get payload 'message) channel)))
     (t
      (list :kind 'message-create
            :conversation-id channel
            :message (slackit-normalize-message payload channel))))))

(defun slackit-normalize-event (event)
  "Classify Slack RTM EVENT into a reducer descriptor plist."
  (let* ((payload (slackit-normalize-object event))
         (type (slackit-normalize-get payload 'type)))
    (cond
     ((equal type "hello") (list :kind 'hello))
     ((equal type "pong")
      (list :kind 'pong :reply-to (slackit-normalize-get payload 'reply_to)))
     ((equal type "reconnect_url")
      (list :kind 'reconnect-url :url (slackit-normalize-get payload 'url)))
     ((equal type "message") (slackit-normalize--message-event payload))
     ((member type '("reaction_added" "reaction_removed"))
      (let ((item (slackit-normalize-get payload 'item)))
        (list :kind (if (equal type "reaction_added")
                        'reaction-add
                      'reaction-remove)
              :conversation-id (slackit-normalize--string-field item 'channel)
              :ts (slackit-normalize--string-field item 'ts)
              :reaction (slackit-normalize--string-field payload 'reaction)
              :user-id (slackit-normalize--string-field payload 'user))))
     ((member type '("channel_marked" "group_marked" "im_marked"))
      (list :kind 'conversation-mark
            :conversation-id (slackit-normalize--string-field payload 'channel)
            :ts (slackit-normalize--string-field payload 'ts)))
     ((member type '("team_join" "user_change"))
      (list :kind 'user-upsert
            :user (slackit-normalize-user
                   (slackit-normalize-get payload 'user))))
     ((member type '("channel_joined" "group_joined"))
      (list :kind 'conversation-upsert
            :conversation (slackit-normalize-conversation
                           (slackit-normalize-get payload 'channel))))
     ((member type '("channel_rename" "group_rename"))
      (list :kind 'conversation-upsert
            :conversation (slackit-normalize-conversation
                           (slackit-normalize-get payload 'channel))))
     ((member type '("channel_archive" "group_archive"
                     "channel_unarchive" "group_unarchive"))
      (list :kind 'conversation-archive
            :conversation-id (slackit-normalize--string-field payload 'channel)
            :archived-p (member type '("channel_archive" "group_archive"))))
     ((member type '("channel_left" "group_left"))
      (list :kind 'conversation-leave
            :conversation-id (slackit-normalize--string-field payload 'channel)))
     (t (list :kind 'ignored :type type)))))

(provide 'slackit-normalize)

;;; slackit-normalize.el ends here
