;;; slackit-history.el --- Exact Slack history windows -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; View-owned room/thread cursors and exact Appkit history transitions with
;; captured canonical revisions for delayed-page race protection.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'appkit-chat-history)
(require 'slackit-api)
(require 'slackit-normalize)
(require 'slackit-runtime)
(require 'slackit-state)

(defvar-local slackit-history--cursor nil
  "Protocol cursor for the next room-history or thread-replies page.")

(defvar-local slackit-history--error nil
  "Last redacted Slack history error code, or nil.")

(defun slackit-history-init ()
  "Reset exact Slack history state in the current replacement view buffer."
  (appkit-chat-history-reset-state)
  (setq-local slackit-history--cursor nil
              slackit-history--error nil))

(defun slackit-history--ordered-messages (body)
  "Return BODY messages ordered by Slack timestamp."
  (sort (copy-sequence
         (or (slackit-normalize-get body 'messages) nil))
        (lambda (left right)
          (string< (slackit-normalize-get left 'ts)
                   (slackit-normalize-get right 'ts)))))

(defun slackit-history--next-cursor (body)
  "Return next cursor from Slack history BODY, or nil."
  (let* ((metadata (slackit-normalize-get body 'response_metadata))
         (cursor (slackit-normalize-get metadata 'next_cursor)))
    (and (stringp cursor) (not (string-empty-p cursor)) cursor)))

(defun slackit-history--relevant-keys (state conversation-id root-ts)
  "Return canonical history keys relevant to CONVERSATION-ID and ROOT-TS."
  (if root-ts
      (slackit-state-reply-keys state conversation-id root-ts)
    (slackit-state-top-level-keys state conversation-id)))

(defun slackit-history--key-set (keys)
  "Return an equal-tested membership set containing KEYS."
  (let ((set (make-hash-table :test #'equal)))
    (dolist (key keys set)
      (puthash key t set))))

(defun slackit-history--retained-keys (response-keys relevant-keys)
  "Return RESPONSE-KEYS that remain in canonical RELEVANT-KEYS."
  (let ((relevant (slackit-history--key-set relevant-keys))
        retained)
    (dolist (key response-keys (nreverse retained))
      (when (gethash key relevant)
        (push key retained)))))

(defun slackit-history--request
    (view conversation-id root-ts latest-p)
  "Load one history page for VIEW and CONVERSATION-ID.

ROOT-TS selects replies.  LATEST-P non-nil establishes a new window."
  (when (appkit-view-live-p view)
    (appkit-with-live-view view
      (let* ((app (appkit-view-app view))
             (state (slackit-runtime-state app))
             (captured-revision (slackit-account-state-revision state))
             (captured-key-set
              (slackit-history--key-set
               (slackit-history--relevant-keys
                state conversation-id root-ts)))
             (request-owner
              (appkit-chat-history-request-begin
               (if latest-p 'latest 'older)))
             (cursor (and (not latest-p) slackit-history--cursor))
             (success
              (lambda (body)
                (appkit-with-live-view view
                  (when (appkit-chat-history-request-current-p request-owner)
                    (let* ((messages
                            (slackit-history--ordered-messages body))
                           (response-keys
                            (delq nil
                                  (mapcar
                                   (lambda (message)
                                     (slackit-normalize-get message 'ts))
                                   messages)))
                           (next-cursor
                            (slackit-history--next-cursor body))
                           (old-first
                            (appkit-chat-history-window-first-key))
                           (old-last
                            (appkit-chat-history-window-last-key))
                           (changes
                            (slackit-state-merge-message-page
                             state conversation-id messages
                             captured-revision))
                           (relevant-keys
                            (slackit-history--relevant-keys
                             state conversation-id root-ts))
                           (retained-keys
                            (slackit-history--retained-keys
                             response-keys relevant-keys))
                           (post-dispatch-create-p
                            (seq-some
                             (lambda (key)
                               (not (gethash key captured-key-set)))
                             relevant-keys)))
                      (slackit-runtime-publish-changes app changes)
                      (setq slackit-history--cursor next-cursor
                            slackit-history--error nil)
                      (cond
                       ((and latest-p (null response-keys))
                        (unless post-dispatch-create-p
                          (appkit-chat-history-window-establish-empty)))
                       ((and root-ts latest-p retained-keys)
                        (appkit-chat-history-window-set
                         (car retained-keys)
                         (and next-cursor (car (last retained-keys)))))
                       ((and root-ts retained-keys)
                        (appkit-chat-history-window-set
                         (or old-first (car retained-keys))
                         (and next-cursor (car (last retained-keys)))))
                       ((and latest-p retained-keys)
                        (appkit-chat-history-window-set
                         (car retained-keys) nil))
                       ((and retained-keys
                             (or (null old-last)
                                 (member old-last relevant-keys)))
                        (appkit-chat-history-window-set
                         (car retained-keys) old-last)))
                      (appkit-chat-history-older-loaded-set
                       (null slackit-history--cursor))
                      (appkit-chat-history-request-end request-owner)
                      (appkit-request-sync
                       view :structure t :position t))))))
             (failure
              (lambda (error-data)
                (appkit-with-live-view view
                  (when (appkit-chat-history-request-current-p request-owner)
                    (setq slackit-history--error
                          (or (plist-get error-data :code) "request_failed"))
                    (appkit-chat-history-request-end request-owner)
                    (appkit-request-sync view :part 'frame))))))
        (if root-ts
            (slackit-api-conversation-replies
             app conversation-id root-ts
             :cursor cursor
             :owner view
             :on-success success
             :on-error failure)
          (slackit-api-conversation-history
           app conversation-id
           :cursor cursor
           :owner view
           :on-success success
           :on-error failure))))))

(defun slackit-history-load-latest (view conversation-id &optional root-ts)
  "Load latest exact history for VIEW and CONVERSATION-ID."
  (slackit-history--request view conversation-id root-ts t))

(defun slackit-history-load-older (view conversation-id &optional root-ts)
  "Load the next room-history or thread-replies page for VIEW."
  (appkit-with-live-view view
    (unless (or (appkit-chat-history-loading-p)
                (appkit-chat-history-older-loaded-p)
                (not (appkit-chat-history-window-known-p)))
      (slackit-history--request view conversation-id root-ts nil))))

(defun slackit-history-slice-messages (messages)
  "Return exact visible slice of ordered MESSAGES, or nil when unknown."
  (let ((result
         (appkit-chat-history-window-slice
          messages (lambda (message)
                     (slackit-normalize-get message 'ts)))))
    (and (plist-get result :valid-p)
         (plist-get result :entries))))

(provide 'slackit-history)

;;; slackit-history.el ends here
