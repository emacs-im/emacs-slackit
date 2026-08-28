;;; slackit-customize.el --- Slackit options and faces -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; User-facing options and presentation definitions for Slackit.

;;; Code:

(require 'auth-source)
(require 'subr-x)

(defgroup slackit nil
  "Appkit-based Slack client."
  :group 'applications
  :prefix "slackit-")

(defcustom slackit-account-ids nil
  "Stable local account IDs offered by `slackit'.

Credentials are resolved at start time by
`slackit-credential-function'; this option must not contain tokens or
cookies."
  :type '(repeat string)
  :group 'slackit)

(defcustom slackit-credential-function #'slackit-auth-source-credential
  "Function called with a local account ID to obtain credentials.

The function returns a plist containing `:token' and optionally
`:cookie'.  Returned values are account-local and must not be logged."
  :type 'function
  :group 'slackit)

(defcustom slackit-http-timeout 30
  "Seconds before a Slack Web API request times out."
  :type 'integer
  :group 'slackit)

(defcustom slackit-read-retry-limit 3
  "Maximum automatic 429 retries for one idempotent read."
  :type 'integer
  :group 'slackit)

(defcustom slackit-history-page-size 100
  "Number of messages requested per room or thread history page."
  :type 'integer
  :group 'slackit)

(defcustom slackit-rtm-hello-timeout 15
  "Seconds an open Slack RTM socket may wait for `hello'."
  :type 'number
  :group 'slackit)

(defcustom slackit-rtm-ping-interval 20
  "Seconds between Slack RTM ping messages."
  :type 'number
  :group 'slackit)

(defcustom slackit-rtm-pong-timeout 10
  "Seconds to wait for the matching Slack RTM pong."
  :type 'number
  :group 'slackit)

(defcustom slackit-reconnect-min-delay 1
  "Minimum seconds before reconnecting Slack RTM."
  :type 'number
  :group 'slackit)

(defcustom slackit-reconnect-max-delay 60
  "Maximum seconds before reconnecting Slack RTM."
  :type 'number
  :group 'slackit)

(defcustom slackit-websocket-host-regexp
  "\\(?:\\`\\|\\.\\)slack\\.com\\'"
  "Regexp accepted for hosts in RTM capability URLs."
  :type 'regexp
  :group 'slackit)

(defface slackit-account-name
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for Slackit account names."
  :group 'slackit)

(defface slackit-room-name
  '((t :inherit font-lock-function-name-face))
  "Face for Slackit room names."
  :group 'slackit)

(defface slackit-sender
  '((t :inherit font-lock-variable-name-face :weight bold))
  "Face for Slackit message senders."
  :group 'slackit)

(defface slackit-timestamp
  '((t :inherit shadow))
  "Face for Slackit message timestamps."
  :group 'slackit)

(defface slackit-reaction
  '((t :inherit font-lock-constant-face))
  "Face for Slackit text reactions."
  :group 'slackit)

(defface slackit-status
  '((t :inherit shadow :slant italic))
  "Face for Slackit loading and connection status."
  :group 'slackit)

(defun slackit-auth-source-credential (account-id)
  "Return credentials for ACCOUNT-ID from auth-source.

The lookup uses host `slack.com' and ACCOUNT-ID as the user.  The
secret is the Slack token.  An optional nonstandard `cookie' field is
accepted for xoxc accounts."
  (let* ((entry (car (auth-source-search :host "slack.com"
                                         :user account-id
                                         :max 1
                                         :require '(:secret))))
         (secret (plist-get entry :secret))
         (token (if (functionp secret) (funcall secret) secret))
         (cookie (plist-get entry :cookie)))
    (unless (and (stringp token) (not (string-empty-p token)))
      (user-error "slackit: no auth-source token for account %s" account-id))
    (list :token token
          :cookie (and (stringp cookie) cookie))))

(provide 'slackit-customize)

;;; slackit-customize.el ends here
