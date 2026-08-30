;;; slackit-render.el --- Safe Slack message rendering -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Protocol-owned Slack mrkdwn/reference expansion and explicit unsupported
;; content fallbacks.  No renderer starts network or media work.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'slackit-customize)
(require 'appkit-chat-avatar)
(require 'appkit-chat-ins)
(require 'appkit-name-color)
(require 'appkit-ui)
(require 'appkit-view)
(require 'slackit-code)
(require 'slackit-avatar)
(require 'slackit-emoji)
(require 'slackit-media)
(require 'slackit-normalize)
(require 'slackit-state)

(declare-function slackit-reaction-toggle
                  "slackit-reaction"
                  (app conversation-id ts name))
(declare-function slackit-thread-open
                  "slackit-thread"
                  (app conversation-id root-ts &optional select))
(declare-function slackit-room-open
                  "slackit-room" (app conversation-id &optional select))
(declare-function slackit-user-open
                  "slackit-user" (app user-id &optional select))

(defun slackit-render-decode-entities (text)
  "Decode Slack's supported HTML entities in TEXT exactly once."
  (replace-regexp-in-string
   "&\\(?:amp\\|lt\\|gt\\|quot\\|#39\\);"
   (lambda (entity)
     (pcase entity
       ("&amp;" "&")
       ("&lt;" "<")
       ("&gt;" ">")
       ("&quot;" "\"")
       ("&#39;" "'")))
   (or text "") t t))

(defun slackit-render--safe-link-p (url)
  "Return non-nil when URL has an explicitly supported safe scheme."
  (and (stringp url)
       (string-match-p "\\`\\(?:https?://\\|mailto:\\)[^[:space:]]+\\'" url)))

(defun slackit-render--insert-link (label url)
  "Insert clickable LABEL for safe URL, otherwise insert plain text."
  (if (slackit-render--safe-link-p url)
      (insert-text-button
       label
       'follow-link t
       'help-echo url
       'action (lambda (_button) (browse-url url)))
    (insert label)))

(defun slackit-render--reference-label (state token)
  "Return display descriptor for Slack reference TOKEN in STATE."
  (cond
   ((string-match "\\`@\\([[:alnum:]]+\\)\\'" token)
    (let ((id (match-string 1 token)))
      (list 'user
            (concat "@" (slackit-state-user-name state id))
            id)))
   ((string-match "\\`#\\([[:alnum:]]+\\)\\(?:|\\(.*\\)\\)?\\'" token)
    (let* ((id (match-string 1 token))
           (fallback (and (match-string 2 token)
                          (slackit-render-decode-entities
                           (match-string 2 token))))
           (known (slackit-state-conversation state id))
           (label (concat "#" (if known
                                  (slackit-state-conversation-name state id)
                                (or fallback id)))))
      (if known
          (list 'conversation label id)
        (list 'text label))))
   ((string-match "\\`!subteam\\^\\([^|]+\\)\\(?:|\\(.*\\)\\)?\\'" token)
    (list 'text
          (if-let* ((fallback (match-string 2 token)))
              (slackit-render-decode-entities fallback)
            (concat "@" (match-string 1 token)))))
   ((string-match "\\`!date\\^[^|]+|\\(.*\\)\\'" token)
    (list 'text (slackit-render-decode-entities (match-string 1 token))))
   ((string-match "\\`!\\(channel\\|here\\|everyone\\)\\'" token)
    (list 'text (concat "@" (match-string 1 token))))
   ((string-match "\\`\\(https?://[^|]+\\|mailto:[^|]+\\)\\(?:|\\(.*\\)\\)?\\'" token)
    (let* ((url (slackit-render-decode-entities (match-string 1 token)))
           (label (and (match-string 2 token)
                       (slackit-render-decode-entities
                        (match-string 2 token)))))
      (list 'link (or label url) url)))
   (t (list 'text (concat "<" token ">")))))

(defun slackit-render--insert-reference (app state token)
  "Insert Slack TOKEN expanded against STATE for exact APP."
  (pcase (slackit-render--reference-label state token)
    (`(link ,label ,url) (slackit-render--insert-link label url))
    (`(user ,label ,user-id)
     (let ((start (point)))
       (insert label)
       (appkit-ui-add-action
        start (point)
        (apply-partially #'slackit-user-open app user-id t)
        :help-echo "Open user profile")))
    (`(conversation ,label ,conversation-id)
     (let ((start (point)))
       (insert label)
       (appkit-ui-add-action
        start (point)
        (apply-partially #'slackit-room-open app conversation-id t)
        :help-echo "Open conversation")))
    (`(text ,label) (insert label))))

(defun slackit-render-insert-text (app state text &optional message)
  "Insert Slack TEXT safely for APP, expanding references through STATE.

When MESSAGE is non-nil, exact Block Kit code descriptors provide authoritative
language metadata for matching fenced spans."
  (let ((position 0)
        (length (length (or text "")))
        (source (or text ""))
        (code-descriptors
         (and message (slackit-code-message-descriptors message))))
    (while (< position length)
      (cond
       ((and (<= (+ position 3) length)
             (string= (substring source position (+ position 3)) "```"))
        (let ((end (string-match "```" source (+ position 3))))
          (if end
              (let* ((code
                      (slackit-render-decode-entities
                       (substring source (+ position 3) end)))
                     (match
                      (slackit-code-consume-descriptor
                       code-descriptors code))
                     (descriptor (plist-get match :descriptor))
                     (language
                      (and descriptor
                           (slackit-code-descriptor-language descriptor))))
                (setq code-descriptors (plist-get match :remaining))
                (insert (slackit-code-block-string app code language))
                (setq position (+ end 3)))
            (insert (slackit-render-decode-entities
                     (substring source position)))
            (setq position length))))
       ((eq (aref source position) ?`)
        (let ((end (cl-position ?` source :start (1+ position))))
          (if end
              (progn
                (insert
                 (slackit-code-inline-string
                  (slackit-render-decode-entities
                   (substring source (1+ position) end))))
                (setq position (1+ end)))
            (insert "`")
            (setq position (1+ position)))))
       ((eq (aref source position) ?<)
        (let ((end (cl-position ?> source :start (1+ position))))
          (if end
              (progn
                (slackit-render--insert-reference
                 app state (substring source (1+ position) end))
                (setq position (1+ end)))
            (insert "<")
            (setq position (1+ position)))))
       (t
        (let* ((next-angle (or (cl-position ?< source :start position) length))
               (next-code (or (cl-position ?` source :start position) length))
               (end (min next-angle next-code)))
          (insert
           (slackit-emoji-substitute
            app
            (slackit-render-decode-entities
             (substring source position end))))
          (setq position end)))))))

(defun slackit-render-reference-dependencies (text)
  "Return Appkit resource keys referenced by Slack TEXT."
  (let ((position 0)
        result)
    (while (and (stringp text)
                (string-match "<\\([@#]\\)\\([[:alnum:]]+\\)" text position))
      (push (list (if (equal (match-string 1 text) "@")
                      :user :conversation)
                  (match-string 2 text))
            result)
      (setq position (match-end 0)))
    (delete-dups (nreverse result))))

(defun slackit-render--sender-id (message)
  "Return stable sender identity from MESSAGE, or nil."
  (or (slackit-normalize-get message 'user)
      (slackit-normalize-get message 'bot_id)))

(defun slackit-render--sender-name (state message)
  "Return sender display name for MESSAGE in STATE."
  (or (when-let* ((user (slackit-normalize-get message 'user)))
        (slackit-state-user-name state user))
      (slackit-normalize-get
       (slackit-normalize-get message 'bot_profile) 'name)
      (slackit-normalize-get message 'username)
      (slackit-normalize-get message 'bot_id)
      "unknown"))

(defun slackit-render-avatar-subject (state message)
  "Return canonical avatar subject for MESSAGE from STATE, or nil."
  (or (when-let* ((user-id (slackit-normalize-get message 'user)))
        (slackit-state-user state user-id))
      (let* ((bot-profile (slackit-normalize-get message 'bot_profile))
             (icons (slackit-normalize-get bot-profile 'icons))
             (bot-id (or (slackit-normalize-get message 'bot_id)
                         (slackit-normalize-get bot-profile 'id))))
        (when (and bot-id icons)
          `((id . ,bot-id)
            (profile
             . ((image_72 . ,(slackit-normalize-get icons 'image_72))
                (image_48 . ,(slackit-normalize-get icons 'image_48))
                (image_32 . ,(slackit-normalize-get icons 'image_32)))))))))

(defun slackit-render--sender-face (state message)
  "Return deterministic highlighted sender face for MESSAGE in STATE."
  (delq nil
        (list
         (appkit-name-color-face
          (or (slackit-render--sender-id message)
              (slackit-render--sender-name state message)))
         'slackit-sender)))

(defun slackit-render--clock (ts &optional short)
  "Return presentation clock for Slack timestamp TS.

When SHORT is non-nil, return only the local hour and minute."
  (condition-case nil
      (format-time-string
       (if short "%H:%M" "%Y-%m-%d %H:%M")
       (seconds-to-time
        (string-to-number (car (split-string (or ts "0") "\\.")))))
    (error (or ts "unknown time"))))

(defun slackit-render--line-fill-column ()
  "Return responsive target width for the current Slackit timeline."
  (or (appkit-view-responsive-width
       slackit-room-auto-fill-margin-columns)
      (and (integerp fill-column) (> fill-column 0) fill-column)
      80))

(defun slackit-render--insert-right-aligned-time
    (text &optional left-prefix-width)
  "Insert timestamp TEXT at the room right edge.

LEFT-PREFIX-WIDTH reserves display-only avatar columns."
  (appkit-chat-ins-insert-right-aligned-text
   text
   (slackit-render--line-fill-column)
   :face 'slackit-timestamp
   :right-align-p slackit-right-align-timestamps
   :left-prefix-width left-prefix-width))

(defun slackit-render--avatar-placeholder (name)
  "Return a compact initials placeholder for sender NAME."
  (let* ((parts (split-string (or name "") "[^[:alnum:]]+" t))
         (first (and parts (substring (car parts) 0 1)))
         (second (and (> (length parts) 1)
                      (substring (cadr parts) 0 1))))
    (format "[%s]" (upcase (concat (or first "?") (or second ""))))))

(defun slackit-render--avatar-prefixes (app state message)
  "Return two-line circular avatar prefixes for APP MESSAGE in STATE."
  (let* ((subject (slackit-render-avatar-subject state message))
         (user-id (slackit-normalize-get message 'user))
         (name (slackit-render--sender-name state message))
         (pixel-size (appkit-chat-avatar-two-line-pixel-size))
         (image
          (and slackit-show-avatars
               (display-graphic-p)
               subject
               (slackit-avatar-cached-image app subject pixel-size)))
         (prefixes
          (appkit-chat-avatar-prefixes
           image
           (slackit-render--avatar-placeholder name)
           :pixel-size pixel-size
           :resize nil)))
    (dolist (key '(:header :first-body))
      (let ((prefix (copy-sequence (plist-get prefixes key))))
        (when (and user-id (stringp prefix) (> (length prefix) 0))
          (add-text-properties
           0 (length prefix)
           (list 'slackit-user-id user-id
                 appkit-ui-action-property
                 (apply-partially #'slackit-user-open app user-id t)
                 'keymap appkit-ui-action-map
                 'help-echo "Open user profile"
                 'mouse-face 'highlight
                 'pointer 'hand)
           prefix))
        (setq prefixes (plist-put prefixes key prefix))))
    prefixes))

(defun slackit-render--insert-divider (text face)
  "Insert full-width room divider TEXT using FACE."
  (appkit-chat-ins-insert-divider-row
   text face (slackit-render--line-fill-column)))

(defun slackit-render--reaction-label (app reaction)
  "Return one display label for APP normalized REACTION."
  (let* ((name (or (slackit-normalize-get reaction 'name) "?"))
         (emoji (slackit-emoji-display-string app name))
         (count (or (slackit-normalize-get reaction 'count) 0)))
    (format "%s %s" (or emoji (format ":%s:" name)) count)))

(defun slackit-render--reaction-selected-p (self-id reaction)
  "Return non-nil when SELF-ID selected normalized REACTION."
  (and self-id
       (member self-id (slackit-normalize-get reaction 'users))))

(defun slackit-render--reaction-help (self-id reaction)
  "Return action help for SELF-ID and normalized REACTION."
  (let ((name (or (slackit-normalize-get reaction 'name) "?")))
    (format "%s :%s:"
            (if (slackit-render--reaction-selected-p self-id reaction)
                "Remove reaction"
              "Add reaction")
            name)))

(defun slackit-render--toggle-reaction
    (app conversation-id ts reaction)
  "Toggle normalized REACTION on exact APP CONVERSATION-ID and TS."
  (when-let* ((name (slackit-normalize-get reaction 'name)))
    (slackit-reaction-toggle app conversation-id ts name)))
(defun slackit-render--open-thread (app conversation-id root-ts)
  "Open APP's exact CONVERSATION-ID thread rooted at ROOT-TS."
  (slackit-thread-open app conversation-id root-ts t))


(defun slackit-render--insert-reactions (app state message)
  "Insert actionable emoji reaction chips for APP MESSAGE using STATE."
  (let ((reactions (slackit-normalize-get message 'reactions))
        (self-id (slackit-state-self-id state))
        (conversation-id (slackit-normalize-get message 'channel))
        (ts (slackit-normalize-get message 'ts)))
    (appkit-chat-ins-insert-reaction-line
     reactions
     :prefix "  "
     :selected-face '(slackit-reaction bold)
     :unselected-face 'slackit-reaction
     :label-function (apply-partially #'slackit-render--reaction-label app)
     :selected-p-function
     (apply-partially #'slackit-render--reaction-selected-p self-id)
     :action-function
     (and conversation-id ts
          (apply-partially
           #'slackit-render--toggle-reaction app conversation-id ts))
     :help-echo-function
     (apply-partially #'slackit-render--reaction-help self-id))))

(defun slackit-render--sequence (value)
  "Return protocol sequence VALUE as a list."
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t nil)))

(defun slackit-render--rich-code-message-p (message)
  "Return non-nil when MESSAGE owns a rich preformatted code element."
  (cl-some
   (lambda (block)
     (and
      (equal "rich_text" (slackit-normalize-get block 'type))
      (cl-some
       (lambda (element)
         (equal "rich_text_preformatted"
                (slackit-normalize-get element 'type)))
       (slackit-render--sequence
        (slackit-normalize-get block 'elements)))))
   (slackit-render--sequence
    (slackit-normalize-get message 'blocks))))

(defun slackit-render--rich-preformatted-text (element)
  "Return exact source text owned by preformatted rich ELEMENT."
  (mapconcat
   (lambda (part)
     (pcase (slackit-normalize-get part 'type)
       ("text" (or (slackit-normalize-get part 'text) ""))
       ("link" (or (slackit-normalize-get part 'text)
                   (slackit-normalize-get part 'url)
                   ""))
       (_ "")))
   (slackit-render--sequence
    (slackit-normalize-get element 'elements))
   ""))

(defun slackit-render--insert-rich-inline (app state element)
  "Insert one supported rich-text inline ELEMENT for APP through STATE."
  (pcase (slackit-normalize-get element 'type)
    ("text"
     (insert
      (slackit-emoji-substitute
       app (or (slackit-normalize-get element 'text) "")))
     t)
    ("user"
     (when-let* ((id (slackit-normalize-get element 'user_id)))
       (slackit-render--insert-reference app state (concat "@" id))
       t))
    ("channel"
     (when-let* ((id (slackit-normalize-get element 'channel_id)))
       (slackit-render--insert-reference app state (concat "#" id))
       t))
    ("link"
     (let ((url (slackit-normalize-get element 'url))
           (label (or (slackit-normalize-get element 'text)
                      (slackit-normalize-get element 'url)
                      "")))
       (slackit-render--insert-link label url)
       t))
    ("emoji"
     (when-let* ((name (slackit-normalize-get element 'name)))
       (insert (or (slackit-emoji-display-string app name)
                   (format ":%s:" name)))
       t))
    (_ nil)))

(defun slackit-render--insert-code-rich-blocks (app state message)
  "Insert code-rich canonical MESSAGE blocks for APP through STATE."
  (let ((inserted-p nil)
        (first-p t))
    (cl-labels
        ((start-block
          ()
          (unless first-p
            (unless (bolp) (insert "\n")))
          (setq first-p nil))
         (insert-section-elements
          (elements)
          (start-block)
          (dolist (element (slackit-render--sequence elements))
            (setq inserted-p
                  (or (slackit-render--insert-rich-inline
                       app state element)
                      inserted-p))))
         (insert-preformatted
          (element)
          (start-block)
          (let ((code
                 (slackit-render--rich-preformatted-text element))
                (language
                 (slackit-normalize-get element 'language)))
            (insert (slackit-code-block-string app code language))
            (setq inserted-p t))))
      (dolist
          (block
           (slackit-render--sequence
            (slackit-normalize-get message 'blocks)))
        (pcase (slackit-normalize-get block 'type)
          ("section"
           (when-let* ((text-object
                        (slackit-normalize-get block 'text))
                       (text (slackit-normalize-get text-object 'text)))
             (start-block)
             (if (equal "mrkdwn"
                        (slackit-normalize-get text-object 'type))
                 (slackit-render-insert-text app state text)
               (insert text))
             (setq inserted-p t)))
          ("rich_text"
           (dolist
               (element
                (slackit-render--sequence
                 (slackit-normalize-get block 'elements)))
             (pcase (slackit-normalize-get element 'type)
               ("rich_text_section"
                (insert-section-elements
                 (slackit-normalize-get element 'elements)))
               ("rich_text_preformatted"
                (insert-preformatted element))))))))
    inserted-p))

(defun slackit-render--insert-primary-content (app state message)
  "Insert MESSAGE subtype marker and canonical primary content."
  (let ((text (or (slackit-normalize-get message 'text) ""))
        (subtype (slackit-normalize-get message 'subtype))
        (code-rich-p (slackit-render--rich-code-message-p message)))
    (when (and subtype
               (not (member subtype '("thread_broadcast" "me_message"))))
      (insert (propertize (format "[%s] " subtype) 'face 'slackit-status)))
    (when (equal subtype "me_message") (insert "* "))
    (cond
     (code-rich-p
      (unless (slackit-render--insert-code-rich-blocks app state message)
        (slackit-render-insert-text app state text message)))
     ((not (string-empty-p text))
      (slackit-render-insert-text app state text message))
     ((and (slackit-normalize-get message 'blocks)
           (not (slackit-media-message-media-only-p message)))
      (insert (propertize "[Unsupported Block Kit content]"
                          'face 'slackit-status))))))

(defun slackit-render--insert-heading
    (app state message sender timestamp header-prefix body-rest-prefix)
  "Insert MESSAGE heading for SENDER and TIMESTAMP in STATE owned by APP."
  (let* ((start (point))
         (user-id (slackit-normalize-get message 'user))
         (sender-start (point)))
    (insert sender)
    (add-text-properties
     sender-start (point)
     (list 'face (slackit-render--sender-face state message)
           'slackit-user-id user-id
           'help-echo sender))
    (when user-id
      (appkit-ui-add-action
       sender-start (point)
       (apply-partially #'slackit-user-open app user-id t)
       :help-echo "Open user profile"))
    (let ((time-span
           (slackit-render--insert-right-aligned-time
            (slackit-render--clock timestamp t)
            (string-width header-prefix))))
      (add-text-properties
       (car time-span) (cdr time-span)
       (list 'help-echo (slackit-render--clock timestamp))))
    (insert "\n")
    (appkit-ui-apply-line-prefix
     start (point)
     (appkit-ui-make-prefix-state header-prefix body-rest-prefix))))

(defun slackit-render-message-row (app state message context)
  "Insert one canonical MESSAGE row for APP from STATE and render CONTEXT."
  (when-let* ((date (plist-get context :date-separator)))
    (slackit-render--insert-divider date 'slackit-date-separator))
  (when (plist-get context :unread-divider)
    (slackit-render--insert-divider
     "Unread messages" 'slackit-unread-divider))
  (let* ((timestamp (slackit-normalize-get message 'ts))
         (sender (slackit-render--sender-name state message))
         (compact (eq (plist-get context :compact) t))
         (avatar-prefixes
          (slackit-render--avatar-prefixes app state message))
         (header-prefix (or (plist-get avatar-prefixes :header) ""))
         (first-body-prefix
          (or (plist-get avatar-prefixes :first-body) "  "))
         (rest-body-prefix
          (or (plist-get avatar-prefixes :rest-body) "  "))
         (body-prefix-state
          (if compact
              (appkit-ui-make-prefix-state
               rest-body-prefix rest-body-prefix)
            (appkit-ui-make-prefix-state
             first-body-prefix rest-body-prefix))))
    (unless compact
      (slackit-render--insert-heading
       app state message sender timestamp header-prefix rest-body-prefix))
    (let ((body-start (point)))
      (slackit-render--insert-primary-content app state message)
      (when compact
        (let ((time-span
               (slackit-render--insert-right-aligned-time
                (slackit-render--clock timestamp t)
                (string-width rest-body-prefix))))
          (add-text-properties
           (car time-span) (cdr time-span)
           (list 'help-echo (slackit-render--clock timestamp)))))
      (insert "\n")
      (appkit-ui-apply-line-prefix
       body-start (point) body-prefix-state))
    ;; Media prefixes replace the last body-indent column with their border.
    ;; Applying the body prefix over the finished card would instead add a
    ;; second four-column indent and misalign the card with message text.
    (let ((appkit-ui-card-indent-prefix-state body-prefix-state)
          (appkit-ui-card-indent-prefix
           (appkit-ui-prefix-string body-prefix-state nil "  ")))
      (slackit-media-insert-message-cards app message))
    (let ((details-start (point)))
      (when-let* ((count (slackit-normalize-get message 'reply_count)))
        (when (> (or count 0) 0)
          (let* ((conversation-id
                  (slackit-normalize-get message 'channel))
                 (root-ts
                  (or (slackit-normalize-get message 'thread_ts)
                      (slackit-normalize-get message 'ts)))
                 (label-start (point)))
            (insert (format "  [%d replies]" count))
            (when (and conversation-id root-ts)
              (appkit-ui-add-action
               label-start (point)
               (apply-partially #'slackit-render--open-thread
                                app conversation-id root-ts)
               :help-echo "Open thread"
               :face 'slackit-status))
            (insert "\n"))))
      (slackit-render--insert-reactions app state message)
      (appkit-ui-apply-line-prefix
       details-start (point) body-prefix-state))))

(provide 'slackit-render)

;;; slackit-render.el ends here
