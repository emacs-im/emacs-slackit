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
(require 'slackit-normalize)
(require 'slackit-state)

(defun slackit-render--decode-entities (text)
  "Decode Slack's supported HTML entities in TEXT."
  (let ((result (or text "")))
    (dolist (entry '(("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">")
                     ("&quot;" . "\"") ("&#39;" . "'")))
      (setq result (replace-regexp-in-string
                    (regexp-quote (car entry)) (cdr entry) result t t)))
    result))

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
      (list 'text (concat "@" (slackit-state-user-name state id)))))
   ((string-match "\\`#\\([[:alnum:]]+\\)\\(?:|\\(.*\\)\\)?\\'" token)
    (let* ((id (match-string 1 token))
           (fallback (match-string 2 token))
           (known (slackit-state-conversation state id)))
      (list 'text (concat "#" (if known
                                  (slackit-state-conversation-name state id)
                                (or fallback id))))))
   ((string-match "\\`!subteam\\^\\([^|]+\\)\\(?:|\\(.*\\)\\)?\\'" token)
    (list 'text (or (match-string 2 token)
                    (concat "@" (match-string 1 token)))))
   ((string-match "\\`!date\\^[^|]+|\\(.*\\)\\'" token)
    (list 'text (match-string 1 token)))
   ((string-match "\\`!\\(channel\\|here\\|everyone\\)\\'" token)
    (list 'text (concat "@" (match-string 1 token))))
   ((string-match "\\`\\(https?://[^|]+\\|mailto:[^|]+\\)\\(?:|\\(.*\\)\\)?\\'" token)
    (let ((url (match-string 1 token))
          (label (match-string 2 token)))
      (list 'link (or label url) url)))
   (t (list 'text (concat "<" token ">")))))

(defun slackit-render--insert-reference (state token)
  "Insert Slack TOKEN expanded against STATE."
  (pcase (slackit-render--reference-label state token)
    (`(link ,label ,url) (slackit-render--insert-link label url))
    (`(text ,label) (insert label))))

(defun slackit-render-insert-text (state text)
  "Insert Slack TEXT safely, expanding references through STATE."
  (let ((position 0)
        (length (length (or text "")))
        (source (or text "")))
    (while (< position length)
      (cond
       ((and (<= (+ position 3) length)
             (string= (substring source position (+ position 3)) "```"))
        (let ((end (string-match "```" source (+ position 3))))
          (if end
              (progn
                (insert (propertize
                         (slackit-render--decode-entities
                          (substring source (+ position 3) end))
                         'face 'font-lock-comment-face))
                (setq position (+ end 3)))
            (insert (slackit-render--decode-entities
                     (substring source position)))
            (setq position length))))
       ((eq (aref source position) ?`)
        (let ((end (cl-position ?` source :start (1+ position))))
          (if end
              (progn
                (insert (propertize
                         (slackit-render--decode-entities
                          (substring source (1+ position) end))
                         'face 'font-lock-constant-face))
                (setq position (1+ end)))
            (insert "`")
            (setq position (1+ position)))))
       ((eq (aref source position) ?<)
        (let ((end (cl-position ?> source :start (1+ position))))
          (if end
              (progn
                (slackit-render--insert-reference
                 state (substring source (1+ position) end))
                (setq position (1+ end)))
            (insert "<")
            (setq position (1+ position)))))
       (t
        (let* ((next-angle (or (cl-position ?< source :start position) length))
               (next-code (or (cl-position ?` source :start position) length))
               (end (min next-angle next-code)))
          (insert (slackit-render--decode-entities
                   (substring source position end)))
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

(defun slackit-render--sender-name (state message)
  "Return sender display name for MESSAGE in STATE."
  (or (when-let* ((user (slackit-normalize-get message 'user)))
        (slackit-state-user-name state user))
      (slackit-normalize-get
       (slackit-normalize-get message 'bot_profile) 'name)
      (slackit-normalize-get message 'username)
      (slackit-normalize-get message 'bot_id)
      "unknown"))

(defun slackit-render--clock (ts)
  "Return presentation clock for Slack timestamp TS."
  (condition-case nil
      (format-time-string
       "%Y-%m-%d %H:%M"
       (seconds-to-time
        (string-to-number (car (split-string (or ts "0") "\\.")))))
    (error (or ts "unknown time"))))

(defun slackit-render--insert-files (message)
  "Insert safe non-fetching file summaries for MESSAGE."
  (dolist (file (or (slackit-normalize-get message 'files) nil))
    (let* ((name (or (slackit-normalize-get file 'name)
                     (slackit-normalize-get file 'title)
                     (slackit-normalize-get file 'id)
                     "file"))
           (type (or (slackit-normalize-get file 'pretty_type)
                     (slackit-normalize-get file 'mimetype)
                     (slackit-normalize-get file 'filetype)))
           (permalink (or (slackit-normalize-get file 'permalink)
                          (slackit-normalize-get file 'permalink_public))))
      (insert "  [file] ")
      (if permalink
          (slackit-render--insert-link name permalink)
        (insert name))
      (when type (insert (format " (%s)" type)))
      (insert "\n"))))

(defun slackit-render--insert-reactions (state message)
  "Insert text reaction summary for MESSAGE using STATE self identity."
  (let ((self-id (slackit-state-self-id state))
        (reactions (slackit-normalize-get message 'reactions)))
    (when reactions
      (insert "  ")
      (dolist (reaction reactions)
        (let* ((name (or (slackit-normalize-get reaction 'name) "?"))
               (count (or (slackit-normalize-get reaction 'count) 0))
               (self-p (member self-id
                               (slackit-normalize-get reaction 'users))))
          (insert (propertize
                   (format ":%s: %s%s  " name count (if self-p "*" ""))
                   'face 'slackit-reaction))))
      (insert "\n"))))

(defun slackit-render-message-row (state message context)
  "Insert one canonical MESSAGE row from STATE and render CONTEXT."
  (when (plist-get context :unread-divider)
    (insert (propertize "──────── unread ────────\n" 'face 'slackit-status)))
  (let* ((ts (slackit-normalize-get message 'ts))
         (sender (slackit-render--sender-name state message))
         (text (or (slackit-normalize-get message 'text) ""))
         (subtype (slackit-normalize-get message 'subtype)))
    (insert (propertize sender 'face 'slackit-sender)
            "  "
            (propertize (slackit-render--clock ts) 'face 'slackit-timestamp)
            "\n")
    (when (and subtype
               (not (member subtype '("thread_broadcast" "me_message"))))
      (insert (propertize (format "[%s] " subtype) 'face 'slackit-status)))
    (if (string-empty-p text)
        (when (slackit-normalize-get message 'blocks)
          (insert (propertize "[Unsupported Block Kit content]"
                              'face 'slackit-status)))
      (when (equal subtype "me_message") (insert "* "))
      (slackit-render-insert-text state text))
    (insert "\n")
    (slackit-render--insert-files message)
    (when-let* ((count (slackit-normalize-get message 'reply_count)))
      (when (> (or count 0) 0)
        (insert (propertize (format "  [%d replies]\n" count)
                            'face 'slackit-status))))
    (slackit-render--insert-reactions state message)))

(provide 'slackit-render)

;;; slackit-render.el ends here
