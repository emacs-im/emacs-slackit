;;; slackit-customize.el --- Slackit options and faces -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; User-facing options and presentation definitions for Slackit.

;;; Code:

(require 'subr-x)

(declare-function slackit-auth-credential "slackit-auth" (account-id))

(defgroup slackit nil
  "Appkit-based Slack client."
  :group 'applications
  :prefix "slackit-")

(defcustom slackit-auth-directory
  (locate-user-emacs-file "slackit/accounts/")
  "Directory containing Slackit's private per-account auth files."
  :type 'directory
  :group 'slackit)

(defcustom slackit-login-url "https://my.slack.com/customize"
  "Slack page opened for browser-session login capture.
The customize page exposes the `TS.boot_data' contract consumed by
`slackit-session.js'; the app client landing page does not."
  :type 'string
  :group 'slackit)

(defcustom slackit-login-browser nil
  "Browser executable or browser-session browser name used for Slack login.
When nil, browser-session selects its supported default browser."
  :type '(choice (const :tag "Automatic" nil) string)
  :group 'slackit)

(defcustom slackit-browser-session-profile-root
  (locate-user-emacs-file "slackit/browser-session/")
  "Root containing account-isolated browser-session login profiles.
Slackit appends a hash of the stable local account ID.  This must not name an
ordinary browser profile directory."
  :type 'directory
  :group 'slackit)

(defcustom slackit-account-ids nil
  "Stable local account IDs offered by `slackit'.

Credentials are resolved at start time by
`slackit-credential-function'; this option must not contain tokens or
cookies."
  :type '(repeat string)
  :group 'slackit)

(defcustom slackit-credential-function #'slackit-auth-credential
  "Function called with a local account ID to obtain credentials.

The default reads Slackit's private browser-imported auth file.  Advanced
headless users may replace it with an auth-source resolver.  The function
returns a plist containing `:token' and optionally `:cookie'; returned values
must not be logged."
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

(defcustom slackit-show-avatars (display-graphic-p)
  "When non-nil, show cached Slack profile images in room message rows."
  :type 'boolean
  :group 'slackit)

(defcustom slackit-avatar-cache-directory
  (locate-user-emacs-file "slackit/avatars/")
  "Directory containing account-isolated cached Slack profile images."
  :type 'directory
  :group 'slackit)

(defcustom slackit-avatar-retry-delay 60
  "Seconds before a failed Slack profile image may be requested again."
  :type 'number
  :group 'slackit)

(defcustom slackit-group-messages t
  "When non-nil, compact consecutive messages from the same sender."
  :type 'boolean
  :group 'slackit)

(defcustom slackit-group-messages-timespan 300
  "Maximum seconds between messages eligible for sender compaction."
  :type 'number
  :group 'slackit)

(defcustom slackit-right-align-timestamps t
  "When non-nil, align room message timestamps to the timeline right edge."
  :type 'boolean
  :group 'slackit)

(defcustom slackit-room-auto-fill-margin-columns 2
  "Columns reserved from the responsive room timeline width."
  :type 'integer
  :group 'slackit)

(defcustom slackit-avatar-host-regexp
  "\\(?:\\`\\|\\.\\)\\(?:slack-edge\\.com\\|slack\\.com\\|gravatar\\.com\\)\\'"
  "Regexp accepted for HTTPS Slack profile image hosts."
  :type 'regexp
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

(defface slackit-date-separator
  '((t :inherit shadow :weight bold))
  "Face for Slackit room date separators."
  :group 'slackit)

(defface slackit-unread-divider
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for the Slackit unread-message divider."
  :group 'slackit)

(defface slackit-reaction
  '((t :inherit font-lock-constant-face))
  "Face for Slackit text reactions."
  :group 'slackit)

(defface slackit-status
  '((t :inherit shadow :slant italic))
  "Face for Slackit loading and connection status."
  :group 'slackit)


(provide 'slackit-customize)

;;; slackit-customize.el ends here
