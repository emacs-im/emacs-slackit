;;; slackit-emoji.el --- Slack emoji display resolution -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Display-only resolution of Slack emoji shortnames through Appkit's
;; Emacs-native Unicode emoji candidates.  This module never rewrites the
;; canonical message or reaction name held by Slackit.

;;; Code:

(require 'appkit-chat-emoji)
(require 'appkit-chat-completion)
(require 'subr-x)

(defconst slackit-emoji--aliases
  '(("pray" . "folded_hands")
    ("+1" . "thumbs_up")
    ("-1" . "thumbs_down")
    ("thumbsup" . "thumbs_up")
    ("thumbsdown" . "thumbs_down"))
  "Slack shortnames whose Unicode names differ from Appkit's names.")

(defconst slackit-emoji--token-regexp
  ":[+[:alnum:]_-]+:"
  "Regexp matching one complete Slack emoji token.")

(defvar slackit-emoji--glyph-table nil
  "Private immutable cache of Slack shortnames to Unicode glyphs.")

(defun slackit-emoji--build-glyph-table ()
  "Build a Slack shortname to Unicode glyph table from Appkit candidates."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (candidate (appkit-chat-emoji-candidates))
      (when (appkit-chat-completion-candidate-p candidate)
        (let ((label (appkit-chat-completion-candidate-label candidate))
              (glyph (appkit-chat-completion-candidate-insert candidate)))
          (when (and (stringp label)
                     (string-match-p
                      (concat "\\`" slackit-emoji--token-regexp "\\'")
                      label)
                     (stringp glyph)
                     (not (string-empty-p glyph)))
            (puthash (substring label 1 -1)
                     (substring-no-properties glyph)
                     table)))))
    (dolist (alias slackit-emoji--aliases)
      (when-let* ((glyph (gethash (cdr alias) table)))
        (puthash (car alias) glyph table)))
    table))

(defun slackit-emoji--glyph-table ()
  "Return the lazily initialized private Unicode glyph table."
  (or slackit-emoji--glyph-table
      (setq slackit-emoji--glyph-table
            (slackit-emoji--build-glyph-table))))

(defun slackit-emoji--lookup (name)
  "Return cached Unicode glyph for exact Slack shortname NAME, or nil."
  (and (stringp name)
       (gethash name (slackit-emoji--glyph-table))))

(defun slackit-emoji-display-string (name)
  "Return a Unicode display string for Slack shortname NAME, or nil.

NAME has no surrounding colons.  The returned string is detached from the
immutable internal cache, so callers may safely add text properties to it."
  (when-let* ((glyph (slackit-emoji--lookup name)))
    (copy-sequence glyph)))

(defun slackit-emoji-substitute (text)
  "Return display TEXT with recognized Slack emoji tokens replaced once.

Only complete `:[+[:alnum:]_-]+:' tokens are considered.  Unknown tokens are
preserved byte-for-byte, and TEXT itself is never modified."
  (replace-regexp-in-string
   slackit-emoji--token-regexp
   (lambda (token)
     (or (slackit-emoji--lookup (substring token 1 -1))
         token))
   text t t))

(provide 'slackit-emoji)

;;; slackit-emoji.el ends here
