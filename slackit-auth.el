;;; slackit-auth.el --- Private Slack browser authentication -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Provider-owned mapping between browser-session captures and Slackit's
;; private, versioned, per-account credential files.

;;; Code:

(require 'browser-session)
(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'slackit-customize)

(defconst slackit-auth--schema-version 1
  "Version of Slackit's private account auth format.")

(defconst slackit-auth--cookie-names '("d" "d-s" "lc")
  "Slack browser cookies imported by Slackit.")

(defconst slackit-auth--package-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the loaded Slackit package.")

(cl-defstruct (slackit-auth-capture
               (:constructor slackit-auth-capture-create))
  account-id
  file
  process
  restart-running-p
  callback
  errorback
  active-p)

(defvar slackit-auth--captures (make-hash-table :test #'equal)
  "Account ID to current browser-session capture owner.")

(defun slackit-auth--nonblank-string (value)
  "Return VALUE when it is a nonblank string, otherwise nil."
  (and (stringp value) (not (string-blank-p value)) value))

(defun slackit-auth--account-key (account-id)
  "Return filesystem-safe stable key for local ACCOUNT-ID."
  (unless (slackit-auth--nonblank-string account-id)
    (user-error "slackit: account ID must be a nonblank string"))
  (secure-hash 'sha256 account-id))

(defun slackit-auth-file (account-id)
  "Return private provider auth file for local ACCOUNT-ID."
  (expand-file-name
   "auth.json"
   (expand-file-name (slackit-auth--account-key account-id)
                     slackit-auth-directory)))

(defun slackit-auth--profile-root (account-id)
  "Return account-isolated browser profile root for ACCOUNT-ID."
  (expand-file-name
   (slackit-auth--account-key account-id)
   slackit-browser-session-profile-root))

(defun slackit-auth--private-mode-p (file)
  "Return non-nil when FILE is private on the current platform."
  (or (eq system-type 'windows-nt)
      (let ((modes (file-modes file)))
        (and modes (zerop (logand modes #o077))))))

(defun slackit-auth--set-private-modes (file)
  "Restrict FILE to its owner when the platform supports modes."
  (unless (eq system-type 'windows-nt)
    (set-file-modes file #o600)))

(defun slackit-auth--prepare-private-directory (directory)
  "Create DIRECTORY and restrict it to its owner."
  (make-directory directory t)
  (unless (eq system-type 'windows-nt)
    (set-file-modes directory #o700))
  directory)

(defun slackit-auth--decode-file (file description)
  "Decode private JSON FILE or signal a fixed DESCRIPTION error."
  (unless (and (file-readable-p file) (slackit-auth--private-mode-p file))
    (error "%s is missing or not private" description))
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents-literally file)
        (json-parse-string (buffer-string)
                           :object-type 'alist
                           :array-type 'list
                           :null-object nil
                           :false-object nil))
    (error (error "%s is invalid JSON" description))))

(defun slackit-auth--valid-token-p (token)
  "Return non-nil when TOKEN is a safe Slack browser token."
  (and (slackit-auth--nonblank-string token)
       (string-prefix-p "xoxc-" token)
       (not (string-match-p "[[:space:];\r\n]" token))))

(defun slackit-auth--valid-cookie-value-p (value)
  "Return non-nil when cookie VALUE is safe for one header field."
  (and (slackit-auth--nonblank-string value)
       (not (string-match-p "[;\r\n]" value))))

(defun slackit-auth--slack-domain-p (domain)
  "Return non-nil when cookie DOMAIN belongs to slack.com."
  (when-let* ((value (slackit-auth--nonblank-string domain)))
    (let ((normalized (downcase (string-remove-prefix "." value))))
      (or (equal normalized "slack.com")
          (string-suffix-p ".slack.com" normalized)))))

(defun slackit-auth--captured-cookie (capture name)
  "Return unique validated cookie NAME from browser-session CAPTURE."
  (let ((matches
         (cl-loop for cookie in (browser-session-cookies capture)
                  when (equal (alist-get 'name cookie) name)
                  collect cookie)))
    (unless (= (length matches) 1)
      (error "Slack browser session must contain exactly one %s cookie" name))
    (let* ((cookie (car matches))
           (value (alist-get 'value cookie))
           (domain (alist-get 'domain cookie))
           (path (alist-get 'path cookie)))
      (unless (and (slackit-auth--valid-cookie-value-p value)
                   (slackit-auth--slack-domain-p domain)
                   (equal path "/")
                   (eq (alist-get 'secure cookie) t))
        (error "Slack browser session contains an invalid %s cookie" name))
      (when (and (equal name "d")
                 (not (string-prefix-p "xoxd-" value)))
        (error "Slack browser session contains an invalid d cookie"))
      value)))

(defun slackit-auth--valid-identity-p (value)
  "Return non-nil when optional Slack identity VALUE is safe."
  (or (null value)
      (and (stringp value)
           (string-match-p "\\`[[:alnum:]]+\\'" value))))

(defun slackit-auth--capture-payload (account-id capture-file)
  "Map private browser-session CAPTURE-FILE to Slack auth for ACCOUNT-ID."
  (unless (slackit-auth--private-mode-p capture-file)
    (error "Slack browser-session capture is not private"))
  (let* ((capture (browser-session-read capture-file))
         (source (alist-get 'source capture))
         (page (alist-get 'page capture))
         (source-url (alist-get 'url source))
         (token (and (listp page) (alist-get 'token page)))
         (team-id (and (listp page) (alist-get 'teamId page)))
         (user-id (and (listp page) (alist-get 'userId page)))
         (d (slackit-auth--captured-cookie capture "d"))
         (d-s (slackit-auth--captured-cookie capture "d-s"))
         (lc (slackit-auth--captured-cookie capture "lc")))
    (unless (equal source-url slackit-login-url)
      (error "Slack browser-session capture has the wrong source URL"))
    (unless (slackit-auth--valid-token-p token)
      (error "Slack browser session did not provide a valid xoxc token"))
    (unless (and (slackit-auth--valid-identity-p team-id)
                 (slackit-auth--valid-identity-p user-id))
      (error "Slack browser session contains an invalid account identity"))
    `((schema . ,slackit-auth--schema-version)
      (account-id . ,account-id)
      (team-id . ,team-id)
      (user-id . ,user-id)
      (token . ,token)
      (cookies . ((d . ,d) (d-s . ,d-s) (lc . ,lc))))))

(defun slackit-auth--cookie-header (cookies)
  "Return Slack Cookie header value from validated COOKIES alist."
  (mapconcat
   (lambda (name)
     (format "%s=%s" name (alist-get (intern name) cookies)))
   slackit-auth--cookie-names
   "; "))

(defun slackit-auth--validate-payload (account-id payload)
  "Validate provider auth PAYLOAD for local ACCOUNT-ID and return it."
  (unless (and (listp payload)
               (equal (alist-get 'schema payload)
                      slackit-auth--schema-version)
               (equal (alist-get 'account-id payload) account-id)
               (slackit-auth--valid-token-p (alist-get 'token payload)))
    (error "Slackit account auth file has an invalid identity or schema"))
  (let ((cookies (alist-get 'cookies payload)))
    (dolist (name slackit-auth--cookie-names)
      (unless (slackit-auth--valid-cookie-value-p
               (alist-get (intern name) cookies))
        (error "Slackit account auth file has invalid cookies"))))
  (unless (and (slackit-auth--valid-identity-p (alist-get 'team-id payload))
               (slackit-auth--valid-identity-p (alist-get 'user-id payload)))
    (error "Slackit account auth file has an invalid account identity"))
  payload)

(defun slackit-auth--read-payload (account-id)
  "Read and validate private provider auth for ACCOUNT-ID."
  (slackit-auth--validate-payload
   account-id
   (slackit-auth--decode-file
    (slackit-auth-file account-id) "Slackit account auth file")))

(defun slackit-auth-available-p (account-id)
  "Return non-nil when ACCOUNT-ID has a readable private auth file."
  (file-readable-p (slackit-auth-file account-id)))

(defun slackit-auth-credential (account-id)
  "Return runtime credential plist for browser-imported ACCOUNT-ID."
  (let* ((payload (slackit-auth--read-payload account-id))
         (cookies (alist-get 'cookies payload)))
    (list :token (alist-get 'token payload)
          :cookie (slackit-auth--cookie-header cookies)
          :team-id (alist-get 'team-id payload)
          :user-id (alist-get 'user-id payload))))

(defun slackit-auth--write-payload (account-id payload)
  "Atomically write validated PAYLOAD for ACCOUNT-ID with private modes."
  (slackit-auth--validate-payload account-id payload)
  (let* ((file (slackit-auth-file account-id))
         (directory (slackit-auth--prepare-private-directory
                     (file-name-directory file)))
         (temporary (make-temp-file
                     (expand-file-name ".slackit-auth-" directory)
                     nil ".json")))
    (unwind-protect
        (progn
          (slackit-auth--set-private-modes temporary)
          (let ((coding-system-for-write 'utf-8-unix)
                (json-encoding-pretty-print nil))
            (with-temp-file temporary
              (insert (json-encode payload) "\n")))
          (rename-file temporary file t)
          (setq temporary nil)
          (slackit-auth--set-private-modes file)
          file)
      (when (and temporary (file-exists-p temporary))
        (ignore-errors (delete-file temporary))))))

(defun slackit-auth--assert-same-established-identity (account-id payload)
  "Reject PAYLOAD when it changes ACCOUNT-ID's established Slack identity."
  (when (slackit-auth-available-p account-id)
    (let ((current (slackit-auth--read-payload account-id)))
      (dolist (key '(team-id user-id))
        (let ((old (alist-get key current))
              (new (alist-get key payload)))
          (when (and old new (not (equal old new)))
            (error "Slack browser session belongs to a different account")))))))

(defun slackit-auth-import-capture (account-id capture-file)
  "Import private browser-session CAPTURE-FILE for local ACCOUNT-ID.
Return the resulting runtime credential plist."
  (let ((payload (slackit-auth--capture-payload account-id capture-file)))
    (slackit-auth--assert-same-established-identity account-id payload)
    (slackit-auth--write-payload account-id payload)
    (slackit-auth-credential account-id)))

(defun slackit-auth--session-script-file ()
  "Return readable provider-owned Slack session capture script."
  (let* ((loaded (expand-file-name "slackit-session.js"
                                   slackit-auth--package-directory))
         (loaded-el (expand-file-name "slackit-auth.el"
                                      slackit-auth--package-directory))
         (source (when (file-exists-p loaded-el)
                   (expand-file-name
                    "slackit-session.js"
                    (file-name-directory (file-truename loaded-el))))))
    (or (cl-find-if #'file-readable-p
                    (delete-dups (delq nil (list loaded source))))
        (user-error "slackit: Slack browser session script is not readable"))))

(defun slackit-auth--capture-file ()
  "Create and return one private temporary browser-session capture file."
  (let ((directory (slackit-auth--prepare-private-directory
                    (expand-file-name "captures/"
                                      slackit-auth-directory))))
    (let ((file (make-temp-file
                 (expand-file-name ".browser-session-" directory)
                 nil ".json")))
      (slackit-auth--set-private-modes file)
      file)))

(defun slackit-auth--delete-capture-file (file)
  "Delete private temporary capture FILE when it exists."
  (when (and (stringp file) (file-exists-p file))
    (ignore-errors (delete-file file))))

(defun slackit-auth-capturing-account-ids ()
  "Return stable account IDs with active browser-session captures."
  (let (account-ids)
    (maphash
     (lambda (account-id owner)
       (when (slackit-auth-capture-active-p owner)
         (push account-id account-ids)))
     slackit-auth--captures)
    (sort account-ids #'string<)))

(defun slackit-auth-capture-running-p (account-id)
  "Return non-nil while ACCOUNT-ID owns a browser-session capture."
  (when-let* ((owner (gethash account-id slackit-auth--captures)))
    (and (slackit-auth-capture-active-p owner) t)))

(defun slackit-auth--owner-current-p (owner)
  "Return non-nil when capture OWNER still owns its account slot."
  (and (slackit-auth-capture-p owner)
       (slackit-auth-capture-active-p owner)
       (eq owner
           (gethash (slackit-auth-capture-account-id owner)
                    slackit-auth--captures))))

(defun slackit-auth--retire-owner (owner)
  "Retire capture OWNER without invoking callbacks."
  (when (slackit-auth-capture-p owner)
    (setf (slackit-auth-capture-active-p owner) nil)
    (when (eq owner
              (gethash (slackit-auth-capture-account-id owner)
                       slackit-auth--captures))
      (remhash (slackit-auth-capture-account-id owner)
               slackit-auth--captures))))

(defun slackit-auth--deliver-error (owner error)
  "Deliver structured capture ERROR for OWNER without secret material."
  (when-let* ((callback (slackit-auth-capture-errorback owner)))
    (funcall callback error)))

(defun slackit-auth--finish-success (owner _metadata)
  "Import and settle successful browser-session capture OWNER."
  (when (slackit-auth--owner-current-p owner)
    (let ((file (slackit-auth-capture-file owner))
          credential
          failure)
      (condition-case error-data
          (setq credential
                (slackit-auth-import-capture
                 (slackit-auth-capture-account-id owner) file))
        (error
         (setq failure
               `((code . "invalid-slack-session")
                 (message . ,(error-message-string error-data))))))
      (slackit-auth--retire-owner owner)
      (slackit-auth--delete-capture-file file)
      (if failure
          (slackit-auth--deliver-error owner failure)
        (when-let* ((callback (slackit-auth-capture-callback owner)))
          (funcall callback credential))))))

(defun slackit-auth--restart-required-p (error)
  "Return non-nil when browser-session ERROR requests a browser restart."
  (equal (browser-session-error-code error) "browser-restart-required"))

(defun slackit-auth--finish-error (owner error)
  "Settle browser-session ERROR for capture OWNER."
  (when (slackit-auth--owner-current-p owner)
    (let ((account-id (slackit-auth-capture-account-id owner))
          (restart-running-p
           (slackit-auth-capture-restart-running-p owner))
          (callback (slackit-auth-capture-callback owner))
          (errorback (slackit-auth-capture-errorback owner)))
      (slackit-auth--retire-owner owner)
      (slackit-auth--delete-capture-file
       (slackit-auth-capture-file owner))
      (if (and (not restart-running-p)
               (slackit-auth--restart-required-p error)
               (yes-or-no-p
                "Restart the Slack login browser once to enable capture? "))
          (slackit-auth-capture
           account-id
           :restart-running t
           :callback callback
           :errorback errorback)
        (when errorback (funcall errorback error))))))

(cl-defun slackit-auth-capture
    (account-id &key restart-running callback errorback)
  "Capture and import browser credentials for local ACCOUNT-ID.
When RESTART-RUNNING is non-nil, permit browser-session's supported explicit
browser restart.  CALLBACK receives a runtime credential plist.  ERRORBACK
receives a structured non-secret error alist."
  (when (slackit-auth-capture-running-p account-id)
    (user-error "slackit: browser login is already running for %s" account-id))
  (let* ((file (slackit-auth--capture-file))
         (owner (slackit-auth-capture-create
                 :account-id account-id
                 :file file
                 :restart-running-p restart-running
                 :callback callback
                 :errorback errorback
                 :active-p t)))
    (puthash account-id owner slackit-auth--captures)
    (condition-case error-data
        (let ((process
               (browser-session-capture
                :url slackit-login-url
                :cookies slackit-auth--cookie-names
                :output-file file
                :browser slackit-login-browser
                :profile-root (slackit-auth--profile-root account-id)
                :script-file (slackit-auth--session-script-file)
                :restart-running restart-running
                :callback (apply-partially
                           #'slackit-auth--finish-success owner)
                :errorback (apply-partially
                            #'slackit-auth--finish-error owner))))
          (if (slackit-auth--owner-current-p owner)
              (setf (slackit-auth-capture-process owner) process)
            (when (and (processp process) (process-live-p process))
              (delete-process process)))
          process)
      (error
       (slackit-auth--retire-owner owner)
       (slackit-auth--delete-capture-file file)
       (signal (car error-data) (cdr error-data))))))

(defun slackit-auth-cancel-capture (account-id)
  "Cancel ACCOUNT-ID's current browser-session capture."
  (when-let* ((owner (gethash account-id slackit-auth--captures)))
    (slackit-auth--retire-owner owner)
    (when-let* ((process (slackit-auth-capture-process owner)))
      (when (and (processp process) (process-live-p process))
        (delete-process process)))
    (slackit-auth--delete-capture-file
     (slackit-auth-capture-file owner))
    t))

(defun slackit-auth-cancel-all ()
  "Cancel every in-progress Slackit browser-session capture."
  (let (account-ids)
    (maphash (lambda (account-id _owner) (push account-id account-ids))
             slackit-auth--captures)
    (dolist (account-id account-ids)
      (slackit-auth-cancel-capture account-id)))
  t)

(defun slackit-auth-clear (account-id)
  "Delete provider auth for ACCOUNT-ID without changing its browser profile."
  (when (slackit-auth-capture-running-p account-id)
    (user-error "slackit: browser login is running for %s" account-id))
  (let ((file (slackit-auth-file account-id)))
    (when (file-exists-p file)
      (delete-file file)
      (let ((directory (file-name-directory file)))
        (when (and (file-directory-p directory)
                   (null (directory-files directory nil
                                          directory-files-no-dot-files-regexp)))
          (delete-directory directory)))
      t)))

(provide 'slackit-auth)

;;; slackit-auth.el ends here
