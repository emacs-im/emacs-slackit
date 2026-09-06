;;; slackit-decode.el --- Slack inbound payload decoding -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Decode Slack Web API and Web/Desktop realtime payloads into canonical plain
;; alists, then classify nested message envelopes before state reduction.

;;; Code:

(require 'json)
(require 'subr-x)

(defun slackit-decode-json (text)
  "Decode JSON TEXT to symbol-key alists, list arrays, and scalar values."
  (json-parse-string text
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object nil))

(defun slackit-decode--string-field (object key)
  "Return OBJECT's KEY as a string when present."
  (when-let* ((value (alist-get key object)))
    (format "%s" value)))

(defun slackit-decode--safe-file (file)
  "Return canonical presentation and fetch metadata for Slack FILE."
  (delq nil
        (mapcar (lambda (key)
                  (when-let* ((value (alist-get key file)))
                    (cons key value)))
                '(id name title mimetype filetype size permalink
                  permalink_public mode pretty_type url_private
                  url_private_download duration_ms audio_wave_samples
                  thumb_video thumb_video_w thumb_video_h
                  mp4 mp4_low hls media_display_type
                  is_external external_type external_url
                  thumb_64 thumb_80 thumb_160
                  thumb_360 thumb_360_w thumb_360_h
                  thumb_480 thumb_480_w thumb_480_h
                  thumb_720 thumb_720_w thumb_720_h
                  thumb_960 thumb_960_w thumb_960_h
                  thumb_1024 thumb_1024_w thumb_1024_h
                  original_w original_h))))

(defun slackit-decode--reaction (reaction)
  "Return canonical Slack REACTION alist."
  `((name . ,(slackit-decode--string-field reaction 'name))
    (count . ,(or (alist-get 'count reaction) 0))
    (users . ,(mapcar (lambda (id) (format "%s" id))
                      (or (alist-get 'users reaction) nil)))))

(defun slackit-decode-message (message &optional conversation-id)
  "Return canonical Slack MESSAGE for CONVERSATION-ID."
  (let* ((result (copy-tree message))
         (channel (or conversation-id
                      (slackit-decode--string-field result 'channel)))
         (ts (slackit-decode--string-field result 'ts))
         (thread-ts (slackit-decode--string-field result 'thread_ts))
         (user (slackit-decode--string-field result 'user))
         (bot-id (slackit-decode--string-field result 'bot_id))
         (files (alist-get 'files result))
         (reactions (alist-get 'reactions result)))
    (dolist (key '(channel ts thread_ts user bot_id files reactions))
      (setq result (assq-delete-all key result)))
    (append
     `((channel . ,channel)
       (ts . ,ts)
       (thread_ts . ,thread-ts)
       (user . ,user)
       (bot_id . ,bot-id)
       (files . ,(mapcar #'slackit-decode--safe-file (or files nil)))
       (reactions . ,(mapcar #'slackit-decode--reaction
                             (or reactions nil))))
     result)))

(defun slackit-decode-user (user)
  "Return canonical Slack USER."
  (let ((result (copy-tree user)))
    (cons (cons 'id (slackit-decode--string-field result 'id))
          (assq-delete-all 'id result))))

(defun slackit-decode-conversation (conversation)
  "Return canonical Slack CONVERSATION."
  (let ((result (copy-tree conversation)))
    (cons (cons 'id (slackit-decode--string-field result 'id))
          (assq-delete-all 'id result))))

(defun slackit-decode--message-event (payload)
  "Classify normalized Slack message event PAYLOAD."
  (let* ((subtype (alist-get 'subtype payload))
         (channel (slackit-decode--string-field payload 'channel)))
    (cond
     ((equal subtype "message_changed")
      (list :kind 'message-change
            :conversation-id channel
            :message (slackit-decode-message
                      (alist-get 'message payload) channel)
            :previous (slackit-decode-message
                       (alist-get 'previous_message payload) channel)))
     ((equal subtype "message_deleted")
      (list :kind 'message-delete
            :conversation-id channel
            :ts (or (slackit-decode--string-field payload 'deleted_ts)
                    (slackit-decode--string-field
                     (alist-get 'previous_message payload) 'ts))))
     ((equal subtype "message_replied")
      (list :kind 'message-change
            :conversation-id channel
            :message (slackit-decode-message
                      (alist-get 'message payload) channel)))
     (t
      (list :kind 'message-create
            :conversation-id channel
            :message (slackit-decode-message payload channel))))))

(defun slackit-decode-event (event)
  "Classify Slack realtime EVENT into a reducer descriptor plist."
  (let* ((payload event)
         (type (alist-get 'type payload)))
    (cond
     ((equal type "hello") (list :kind 'hello))
     ((equal type "pong")
      (list :kind 'pong :reply-to (alist-get 'reply_to payload)))
     ((equal type "reconnect_url")
      (list :kind 'ignored :type type))
     ((equal type "message") (slackit-decode--message-event payload))
     ((member type '("reaction_added" "reaction_removed"))
      (let ((item (alist-get 'item payload)))
        (list :kind (if (equal type "reaction_added")
                        'reaction-add
                      'reaction-remove)
              :conversation-id (slackit-decode--string-field item 'channel)
              :ts (slackit-decode--string-field item 'ts)
              :reaction (slackit-decode--string-field payload 'reaction)
              :user-id (slackit-decode--string-field payload 'user))))
     ((member type '("channel_marked" "group_marked" "im_marked"
                     "mpim_marked"))
      (list :kind 'conversation-mark
            :conversation-id (slackit-decode--string-field payload 'channel)
            :ts (slackit-decode--string-field payload 'ts)))
     ((member type '("team_join" "user_change"))
      (list :kind 'user-upsert
            :user (slackit-decode-user
                   (alist-get 'user payload))))
     ((member type '("channel_joined" "group_joined"))
      (list :kind 'conversation-upsert
            :conversation (slackit-decode-conversation
                           (alist-get 'channel payload))))
     ((member type '("channel_rename" "group_rename"))
      (list :kind 'conversation-upsert
            :conversation (slackit-decode-conversation
                           (alist-get 'channel payload))))
     ((member type '("channel_archive" "group_archive"
                     "channel_unarchive" "group_unarchive"))
      (list :kind 'conversation-archive
            :conversation-id (slackit-decode--string-field payload 'channel)
            :archived-p (member type '("channel_archive" "group_archive"))))
     ((member type '("channel_left" "group_left"))
      (list :kind 'conversation-leave
            :conversation-id (slackit-decode--string-field payload 'channel)))
     (t (list :kind 'ignored :type type)))))

(provide 'slackit-decode)

;;; slackit-decode.el ends here
