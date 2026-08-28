;;; slackit-completion.el --- Structured Slack composer completion -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Account-local user/channel completion whose wire identity is the stable
;; Slack ID carried in an Appkit structured input object.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-chatbuf)
(require 'appkit-chat-completion)
(require 'slackit-render)
(require 'slackit-runtime)
(require 'slackit-state)

(declare-function slackit-room-current-app "slackit-room" ())

(defun slackit-completion--state ()
  "Return canonical state for the current Slackit chat buffer."
  (slackit-runtime-state (slackit-room-current-app)))

(defun slackit-completion--user-candidates ()
  "Return collision-safe Appkit candidates for current account users."
  (let ((state (slackit-completion--state))
        candidates)
    (maphash
     (lambda (id user)
       (unless (slackit-normalize-get user 'deleted)
         (let* ((name (slackit-state-user-name state id))
                (real-name (slackit-normalize-get
                            (slackit-normalize-get user 'profile) 'real_name)))
           (push
            (appkit-chat-completion-candidate-create
             :label (format "@%s  [%s]" name id)
             :annotation (and real-name (concat "  " real-name))
             :search-terms (delq nil (list name real-name id))
             :value (list :type 'user :id id :label name)
             :group "Users")
            candidates))))
     (slackit-account-state-users state))
    (sort candidates
          (lambda (left right)
            (string-lessp
             (appkit-chat-completion-candidate-label left)
             (appkit-chat-completion-candidate-label right))))))

(defun slackit-completion--channel-candidates ()
  "Return collision-safe Appkit candidates for joined conversations."
  (let ((state (slackit-completion--state))
        candidates)
    (dolist (id (slackit-state-joined-conversation-ids state))
      (let ((conversation (slackit-state-conversation state id)))
        (unless (or (slackit-normalize-get conversation 'is_im)
                    (slackit-normalize-get conversation 'is_mpim))
          (let ((name (slackit-state-conversation-name state id)))
            (push
             (appkit-chat-completion-candidate-create
              :label (format "#%s  [%s]" name id)
              :search-terms (list name id)
              :value (list :type 'channel :id id :label name)
              :group "Channels")
             candidates)))))
    (sort candidates
          (lambda (left right)
            (string-lessp
             (appkit-chat-completion-candidate-label left)
             (appkit-chat-completion-candidate-label right))))))

(defun slackit-completion--insert-candidate (candidate)
  "Insert CANDIDATE as one semantic Appkit composer object."
  (let* ((value (appkit-chat-completion-candidate-value candidate))
         (type (plist-get value :type))
         (label (plist-get value :label))
         (display (concat (if (eq type 'user) "@" "#") label)))
    (appkit-chatbuf-input-insert
     display
     :object value
     :properties '(face appkit-chatbuf-input-object))))

(defun slackit-completion--capf (trigger candidates)
  "Return CAPF for TRIGGER over CANDIDATES."
  (when-let* ((bounds (appkit-chat-completion-token-bounds trigger)))
    (appkit-chat-completion-capf
     (plist-get bounds :start)
     (plist-get bounds :end)
     candidates
     :insert-function #'slackit-completion--insert-candidate)))

(defun slackit-completion-user-capf ()
  "Complete an @user token in the current composer."
  (slackit-completion--capf ?@ (slackit-completion--user-candidates)))

(defun slackit-completion-channel-capf ()
  "Complete a #channel token in the current composer."
  (slackit-completion--capf ?# (slackit-completion--channel-candidates)))

(defun slackit-completion--insert-read-candidate (prompt candidates)
  "Read one CANDIDATES item with PROMPT and insert it at point."
  (unless (appkit-chatbuf-point-in-input-p)
    (appkit-chatbuf-focus-input))
  (slackit-completion--insert-candidate
   (appkit-chat-completion-read prompt candidates))
  (appkit-chatbuf-input-state-sync))

(defun slackit-completion-user ()
  "Select and insert an exact-ID Slack user object."
  (interactive)
  (slackit-completion--insert-read-candidate
   "Mention user: " (slackit-completion--user-candidates)))

(defun slackit-completion-channel ()
  "Select and insert an exact-ID Slack channel object."
  (interactive)
  (slackit-completion--insert-read-candidate
   "Mention channel: " (slackit-completion--channel-candidates)))


(defun slackit-completion-decode-wire (state text)
  "Return Slack wire TEXT with known references as structured objects."
  (let ((position 0)
        parts)
    (while (and (stringp text)
                (string-match
                 "<\\([@#]\\)\\([[:alnum:]]+\\)\\(?:|[^>]*\\)?>"
                 text position))
      (push (slackit-render-decode-entities
             (substring text position (match-beginning 0)))
            parts)
      (let* ((kind (if (equal (match-string 1 text) "@") 'user 'channel))
             (id (match-string 2 text))
             (label (if (eq kind 'user)
                        (slackit-state-user-name state id)
                      (slackit-state-conversation-name state id)))
             (object (list :type kind :id id :label label))
             (display (concat (if (eq kind 'user) "@" "#") label)))
        (push (appkit-chatbuf-input-object-string display object) parts))
      (setq position (match-end 0))
      (when (and (< position (length text))
                 (eq (aref text position) ?\s))
        (setq position (1+ position))))
    (push (slackit-render-decode-entities
           (substring (or text "") position))
          parts)
    (apply #'concat (nreverse parts))))

(defun slackit-completion-setup ()
  "Install structured Slack completion in the current chat buffer."
  (appkit-chat-completion-setup
   :capf-functions '(slackit-completion-user-capf
                     slackit-completion-channel-capf)))

(provide 'slackit-completion)

;;; slackit-completion.el ends here
