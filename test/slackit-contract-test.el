;;; slackit-contract-test.el --- Slackit contract tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'slackit)

(cl-defmacro slackit-test-with-app ((variable id) &rest body)
  "Run BODY with fresh Slackit app VARIABLE named ID, then clean it up."
  (declare (indent 1))
  `(let* ((,variable
           (slackit-runtime-start-account
            ,id (list :token "xoxp-CANARY-TOKEN" :cookie "xoxd-CANARY-COOKIE")))
          (buffers nil))
     (unwind-protect
         (progn ,@body)
       (when (appkit-app-p ,variable)
         (maphash (lambda (_id view)
                    (when (buffer-live-p (appkit-view-buffer view))
                      (push (appkit-view-buffer view) buffers)))
                  (appkit-app-view-registry ,variable))
         (slackit-runtime-stop-account ,variable))
       (dolist (buffer buffers)
         (when (buffer-live-p buffer) (kill-buffer buffer))))))

(cl-defmacro slackit-test-with-auth-directory ((root) &rest body)
  "Run BODY with isolated Slackit auth/profile storage under ROOT."
  (declare (indent 1))
  `(let* ((,root (make-temp-file "slackit-auth-test-" t))
          (slackit-auth-directory (expand-file-name "accounts/" ,root))
          (slackit-browser-session-profile-root
           (expand-file-name "profiles/" ,root))
          (slackit-login-url "https://my.slack.com/customize"))
     (unwind-protect
         (progn ,@body)
       (slackit-auth-cancel-all)
       (when (file-directory-p ,root)
         (delete-directory ,root t)))))

(cl-defun slackit-test--capture-payload
    (&key
     (token "xoxc-CAPTURE-CANARY")
     (domain ".slack.com")
     (team-id "T1")
     (user-id "U1")
     cookies)
  "Return synthetic browser-session payload with optional overrides."
  `((schema . 1)
    (source . ((browser . "fake")
               (url . "https://my.slack.com/customize")
               (user_agent . "test")))
    (cookies
     . ,(or cookies
            (mapcar
             (lambda (entry)
               `((name . ,(car entry))
                 (value . ,(cdr entry))
                 (domain . ,domain)
                 (path . "/")
                 (expires . 0)
                 (secure . t)
                 (httpOnly . t)))
             '(("d" . "xoxd-COOKIE-CANARY")
               ("d-s" . "DS-COOKIE-CANARY")
               ("lc" . "LC-COOKIE-CANARY")))))
    (page . ((token . ,token)
             (teamId . ,team-id)
             (userId . ,user-id)))))

(defun slackit-test--write-private-json (file payload)
  "Write synthetic private PAYLOAD to FILE."
  (let ((json-encoding-pretty-print nil))
    (with-temp-file file
      (insert (json-encode payload) "\n")))
  (unless (eq system-type 'windows-nt)
    (set-file-modes file #o600))
  file)

(ert-deftest slackit-contract-login-url-exposes-token-bootstrap ()
  (should
   (equal "https://my.slack.com/customize"
          (default-value 'slackit-login-url))))

(ert-deftest slackit-contract-browser-capture-imports-private-account-auth ()
  (slackit-test-with-auth-directory (root)
    (let ((capture (make-temp-file
                    (expand-file-name "capture-" root) nil ".json")))
      (slackit-test--write-private-json
       capture (slackit-test--capture-payload))
      (let* ((credential (slackit-auth-import-capture "work" capture))
             (auth-file (slackit-auth-file "work"))
             (cookie (plist-get credential :cookie)))
        (should (slackit-auth-available-p "work"))
        (should (string-prefix-p "xoxc-" (plist-get credential :token)))
        (should (string-match-p "\\`d=[^;]+; d-s=[^;]+; lc=[^;]+\\'" cookie))
        (unless (eq system-type 'windows-nt)
          (should (zerop (logand (file-modes auth-file) #o077))))
        (should (equal "T1" (plist-get credential :team-id)))
        (should (equal "U1" (plist-get credential :user-id))))
      (delete-file capture))))

(ert-deftest slackit-contract-browser-capture-rejects-cross-origin-cookie ()
  (slackit-test-with-auth-directory (root)
    (let* ((capture (make-temp-file
                     (expand-file-name "capture-" root) nil ".json"))
           (payload (slackit-test--capture-payload
                     :domain ".slack.com.evil.invalid"))
           error-text)
      (slackit-test--write-private-json capture payload)
      (condition-case error-data
          (slackit-auth-import-capture "work" capture)
        (error (setq error-text (error-message-string error-data))))
      (should error-text)
      (should-not (string-match-p "CAPTURE-CANARY" error-text))
      (should-not (string-match-p "COOKIE-CANARY" error-text))
      (should-not (slackit-auth-available-p "work"))
      (delete-file capture))))

(ert-deftest slackit-contract-browser-capture-preserves-established-identity ()
  (slackit-test-with-auth-directory (root)
    (let ((first (make-temp-file
                  (expand-file-name "capture-first-" root) nil ".json"))
          (second (make-temp-file
                   (expand-file-name "capture-second-" root) nil ".json")))
      (slackit-test--write-private-json
       first (slackit-test--capture-payload :team-id "T1" :user-id "U1"))
      (slackit-auth-import-capture "work" first)
      (slackit-test--write-private-json
       second (slackit-test--capture-payload :team-id "T2" :user-id "U2"))
      (should-error (slackit-auth-import-capture "work" second))
      (let ((credential (slackit-auth-credential "work")))
        (should (equal "T1" (plist-get credential :team-id)))
        (should (equal "U1" (plist-get credential :user-id))))
      (delete-file first)
      (delete-file second))))

(ert-deftest slackit-contract-browser-capture-lifecycle-deletes-temporary-file ()
  (slackit-test-with-auth-directory (_root)
    (let (arguments output credential)
      (cl-letf (((symbol-function 'browser-session-capture)
                 (lambda (&rest supplied)
                   (setq arguments supplied
                         output (plist-get supplied :output-file))
                   (slackit-test--write-private-json
                    output (slackit-test--capture-payload))
                   (funcall (plist-get supplied :callback)
                            '((browser . "fake")))
                   nil)))
        (slackit-auth-capture
         "work" :callback (lambda (value) (setq credential value))))
      (should credential)
      (should (equal '("d" "d-s" "lc")
                     (plist-get arguments :cookies)))
      (should (string-suffix-p
               (slackit-auth--account-key "work")
               (directory-file-name (plist-get arguments :profile-root))))
      (should (file-readable-p (plist-get arguments :script-file)))
      (should-not (file-exists-p output))
      (should-not (slackit-auth-capture-running-p "work")))))

(ert-deftest slackit-contract-browser-capture-failure-cleans-ownership ()
  (slackit-test-with-auth-directory (_root)
    (let (output failure)
      (cl-letf (((symbol-function 'browser-session-capture)
                 (lambda (&rest supplied)
                   (setq output (plist-get supplied :output-file))
                   (funcall
                    (plist-get supplied :errorback)
                    '((code . "auth-required")
                      (message . "Login required")))
                   nil)))
        (slackit-auth-capture
         "work" :errorback (lambda (error) (setq failure error))))
      (should (equal "auth-required" (alist-get 'code failure)))
      (should-not (file-exists-p output))
      (should-not (slackit-auth-capture-running-p "work")))))

(ert-deftest slackit-contract-login-command-starts-imported-account ()
  (let ((slackit-account-ids nil)
        capture-callback
        started)
    (cl-letf (((symbol-function 'slackit-auth-capture-running-p)
               (lambda (_account-id) nil))
              ((symbol-function 'slackit-auth-capture)
               (lambda (_account-id &rest arguments)
                 (setq capture-callback (plist-get arguments :callback))
                 'capture-process))
              ((symbol-function 'slackit-start-account)
               (lambda (account-id credential)
                 (setq started (list account-id credential)))))
      (slackit-login "work")
      (funcall capture-callback
               '(:token "xoxc-LOGIN-CANARY"
                 :cookie "d=xoxd-LOGIN-CANARY; d-s=DS; lc=LC")))
    (should (equal "work" (car started)))
    (should (member "work" slackit-account-ids))))

(ert-deftest slackit-contract-account-state-is-isolated ()
  (slackit-test-with-app (left "left")
    (slackit-test-with-app (right "right")
      (slackit-state-upsert-message
       (slackit-runtime-state left) "C1"
       '((ts . "1710000000.000001") (text . "left")))
      (slackit-state-upsert-message
       (slackit-runtime-state right) "C1"
       '((ts . "1710000000.000001") (text . "right")))
      (should (equal "left"
                     (slackit-normalize-get
                      (slackit-state-message
                       (slackit-runtime-state left)
                       "C1" "1710000000.000001")
                      'text)))
      (should (equal "right"
                     (slackit-normalize-get
                      (slackit-state-message
                       (slackit-runtime-state right)
                       "C1" "1710000000.000001")
                      'text)))
      (should-not (eq (appkit-app-state left) (appkit-app-state right))))))

(ert-deftest slackit-contract-realtime-reconnect-preserves-account-work ()
  (slackit-test-with-app (app "generation")
    (let* ((account-generation (slackit-runtime-generation app))
           (connection-generation
            (slackit-runtime-connection-generation app))
           (operation
            (slackit-runtime-operation-begin app '(bootstrap))))
      (slackit-realtime--begin-attempt app)
      (should (= account-generation (slackit-runtime-generation app)))
      (should (slackit-runtime-current-p app account-generation))
      (should (slackit-runtime-operation-current-p app operation))
      (should (= (1+ connection-generation)
                 (slackit-runtime-connection-generation app)))
      (let ((first-connection-generation
             (slackit-runtime-connection-generation app)))
        (slackit-realtime--begin-attempt app)
        (should (= account-generation (slackit-runtime-generation app)))
        (should-not
         (slackit-realtime--generation-current-p
          app first-connection-generation))
        (should
         (slackit-realtime--generation-current-p
          app (slackit-runtime-connection-generation app)))))))

(ert-deftest slackit-contract-stale-http-response-retires-handle ()
  (slackit-test-with-app (app "stale-http")
    (let* ((request
            (slackit-http-request-create
             :app app
             :owner app
             :generation (slackit-runtime-generation app)
             :active-p t))
           (handle
            (appkit-register-handle
             app 'slackit-http request #'slackit-http--cancel-request)))
      (setf (slackit-http-request-handle request) handle)
      (slackit-runtime-begin-generation app)
      (slackit-http--handle-success request nil)
      (should-not (slackit-http-request-active-p request))
      (should-not (appkit-handle-alive-p handle))
      (should-not (memq handle (appkit-app-handles app))))))

(ert-deftest slackit-contract-root-projection-is-repeatable ()
  (slackit-test-with-app (app "root-projection")
    (let ((state (slackit-runtime-state app)))
      (slackit-state-put-conversation
       state '((id . "C1") (name . "general") (is_member . t)))
      (let* ((first
              (mapcar #'appkit-directory-entry-key
                      (slackit-root--entries state)))
             (second
              (mapcar #'appkit-directory-entry-key
                      (slackit-root--entries state))))
        (should (equal first second))
        (should (= (length second)
                   (length (delete-dups (copy-sequence second)))))))))

(ert-deftest slackit-contract-credentials-are-origin-bound-and-redacted ()
  (slackit-test-with-app (app "security")
    (let ((headers (slackit-http--headers app 'get)))
      (should (equal "Bearer xoxp-CANARY-TOKEN"
                     (cdr (assoc "Authorization" headers))))
      (should (equal "d=xoxd-CANARY-COOKIE"
                     (cdr (assoc "Cookie" headers)))))
    (should (equal "https://slack.com/api/users.list"
                   (slackit-http--endpoint-url "users.list")))
    (should-error (slackit-http--endpoint-url "https://evil.invalid/steal"))
    (let ((redacted
           (slackit-runtime-redact
            app
            "Bearer xoxp-CANARY-TOKEN d=xoxd-CANARY-COOKIE ?token=secret")))
      (should-not (string-match-p "CANARY" redacted))
      (should-not (string-match-p "token=secret" redacted)))
    (should (slackit-realtime-valid-url-p
             "wss://wss-primary.slack.com/?token=capability"))
    (should-not (slackit-realtime-valid-url-p
                 "wss://slack.com.evil.invalid/?token=capability"))
    (should-not (slackit-realtime-valid-url-p
                 "https://wss-primary.slack.com/"))
    (should-not (slackit-realtime-valid-url-p
                 "wss://wss-backup.slack.com/?token=capability"))))

(ert-deftest slackit-contract-pagination-consumes-every-cursor ()
  (let ((calls nil)
        (pages nil)
        complete)
    (cl-letf (((symbol-function 'slackit-api-request)
               (lambda (_app endpoint &rest arguments)
                 (let* ((parameters (plist-get arguments :parameters))
                        (cursor (alist-get 'cursor parameters nil nil #'eq))
                        (success (plist-get arguments :on-success)))
                   (push (list endpoint cursor) calls)
                   (funcall
                    success
                    (if cursor
                        '((ok . t)
                          (channels . (((id . "C2"))))
                          (response_metadata . ((next_cursor . ""))))
                      '((ok . t)
                        (channels . (((id . "C1"))))
                        (response_metadata . ((next_cursor . "next"))))))))))
      (slackit-api-conversations-list-all
       'fake-app
       :on-page (lambda (items) (push items pages))
       :on-complete (lambda () (setq complete t))))
    (should complete)
    (should (= 2 (length calls)))
    (should (equal '(nil "next") (mapcar #'cadr (nreverse calls))))
    (should (= 2 (length pages)))))

(ert-deftest slackit-contract-bootstrap-readiness-is-complete-not-first-page ()
  (slackit-test-with-app (app "bootstrap")
    (let ((state (slackit-runtime-state app)))
      (slackit-state-bootstrap-reset state)
      (slackit-state-set-bootstrap-complete state 'identity)
      (slackit-state-set-bootstrap-complete state 'users)
      (should-not (slackit-state-bootstrap-ready-p state))
      (slackit-state-set-bootstrap-complete state 'conversations)
      (should (slackit-state-bootstrap-ready-p state)))))

(ert-deftest slackit-contract-bootstrap-binds-identity-before-realtime ()
  (slackit-test-with-app (app "lazy-bootstrap")
    (let (users-list-called
          (realtime-starts 0))
      (cl-letf
          (((symbol-function 'slackit-api-auth-test)
            (lambda (_app &rest arguments)
              (funcall
               (plist-get arguments :on-success)
               '((ok . t) (team_id . "T1") (team . "Team")
                 (user_id . "U0") (user . "self")))))
           ((symbol-function 'slackit-api-conversations-list-all)
            (lambda (_app &rest arguments)
              (funcall (plist-get arguments :on-page) nil)
              (funcall (plist-get arguments :on-complete))))
           ((symbol-function 'slackit-api-users-list-all)
            (lambda (&rest _arguments)
              (setq users-list-called t))))
        (slackit-bootstrap-account
         app
         (lambda (ready-app)
           (should (eq app ready-app))
           (cl-incf realtime-starts))))
      (let* ((state (slackit-runtime-state app))
             (credential
              (slackit-transport-credential
               (slackit-runtime-transport app))))
        (should-not users-list-called)
        (should (= 1 realtime-starts))
        (should (equal "T1" (slackit-credential-team-id credential)))
        (should (equal "U0" (slackit-credential-user-id credential)))
        (should (slackit-state-bootstrap-ready-p state))
        (should (slackit-state-user state "U0"))
        (should-not
         (gethash '(bootstrap) (appkit-app-request-table app)))))))

(ert-deftest slackit-contract-lazy-user-lookups-are-deduplicated ()
  (slackit-test-with-app (app "lazy-user")
    (let (calls success)
      (cl-letf (((symbol-function 'slackit-api-user-info)
                 (lambda (_app user-id &rest arguments)
                   (push user-id calls)
                   (setq success (plist-get arguments :on-success)))))
        (let ((first (slackit-runtime-ensure-user app "U1"))
              (second (slackit-runtime-ensure-user app "U1")))
          (should (eq first second))
          (should (equal '("U1") calls))
          (funcall success
                   '((ok . t)
                     (user . ((id . "U1") (name . "alice")))))
          (should (equal "alice"
                         (slackit-state-user-name
                          (slackit-runtime-state app) "U1")))
          (should-not (slackit-runtime-ensure-user app "U1"))
          (should-not
           (gethash '(user "U1") (appkit-app-request-table app))))))))

(ert-deftest slackit-contract-avatar-cache-is-private-and-deduplicated ()
  (let* ((root (make-temp-file "slackit-avatar-test-" t))
         (slackit-avatar-cache-directory
          (expand-file-name "avatars/" root))
         (user
          '((id . "U1")
            (profile
             . ((image_48
                 . "https://ca.slack-edge.com/avatar-canary.png")))))
         calls
         success)
    (unwind-protect
        (slackit-test-with-app (app "avatar")
          (clrhash slackit-avatar--fetches)
          (clrhash slackit-avatar--failures)
          (slackit-avatar--prepare-cache-directory)
          (let ((resource-key (slackit-avatar-resource-key app user)))
            (should resource-key)
            (should-not
             (string-match-p "avatar-canary"
                             (prin1-to-string resource-key)))
            (should-not
             (slackit-avatar-resource-key
              app
              '((id . "U2")
                (profile
                 . ((image_48 . "https://evil.invalid/avatar.png"))))))
            (cl-letf
                (((symbol-function
                   'appkit-media-cache-image-resource-async)
                  (lambda (resource cache-base callback _error &rest arguments)
                    (push (list resource cache-base arguments) calls)
                    (setq success callback)
                    nil)))
              (let ((first
                     (slackit-avatar--ensure-fetch
                      app user resource-key))
                    (second
                     (slackit-avatar--ensure-fetch
                      app user resource-key)))
                (should first)
                (should-not second)
                (should (= 1 (length calls)))
                (should-not
                 (string-match-p
                  "CANARY-TOKEN\\|CANARY-COOKIE\\|Authorization\\|Cookie"
                  (prin1-to-string calls)))
                (let* ((file (concat (cadar calls) ".png"))
                       (stale
                        (expand-file-name
                         (format "%s-stale.png"
                                 (slackit-avatar--cache-scope resource-key))
                         slackit-avatar-cache-directory)))
                  (with-temp-file stale (insert "old-image"))
                  (with-temp-file file (insert "synthetic-image"))
                  (funcall success file)
                  (should-not (file-exists-p stale))
                  (unless (memq system-type '(ms-dos windows-nt cygwin))
                    (should (= #o700
                               (logand #o777
                                       (file-modes
                                        slackit-avatar-cache-directory))))
                    (should (= #o600
                               (logand #o777 (file-modes file))))))))))
      (clrhash slackit-avatar--fetches)
      (clrhash slackit-avatar--failures)
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest slackit-contract-room-rows-use-rich-chat-layout ()
  (slackit-test-with-app (app "rich-room")
    (let* ((state (slackit-runtime-state app))
           (first
            '((ts . "1710000000.000001")
              (user . "U1")
              (text . "first message")))
           (second
            '((ts . "1710000060.000001")
              (user . "U1")
              (text . "second message")))
           (first-context
            (slackit-room--message-context nil first))
           (second-context
            (slackit-room--message-context first second)))
      (slackit-state-put-user
       state '((id . "U1") (name . "alice")
               (profile . ((display_name . "Alice")))))
      (should (plist-get first-context :date-separator))
      (should (eq t (plist-get second-context :compact)))
      (with-temp-buffer
        (let ((slackit-show-avatars nil)
              (slackit-right-align-timestamps t)
              (fill-column 60))
          (cl-letf (((symbol-function 'appkit-view-responsive-width)
                     (lambda (&rest _arguments) 60))
                    ((symbol-function
                      'appkit-chat-avatar-two-line-pixel-size)
                     (lambda () 32)))
            (slackit-render-message-row app state first first-context)
            (slackit-render-message-row app state second second-context)))
        (goto-char (point-min))
        (should (search-forward "Alice" nil t))
        (let* ((name-position (match-beginning 0))
               (face (get-text-property name-position 'face))
               (prefix (get-text-property name-position 'line-prefix)))
          (should (memq 'slackit-sender face))
          (should (memq (appkit-name-color-face "U1") face))
          (should (stringp prefix))
          (should (string-match-p "\\[A\\]" prefix)))
        (goto-char (point-min))
        (should (= 1 (how-many "Alice" (point-min) (point-max))))
        (should (search-forward "first message" nil t))
        (should (search-forward "second message" nil t))
        (goto-char (point-min))
        (should (text-property-search-forward
                 'face 'slackit-timestamp
                 (lambda (value expected)
                   (if (listp value)
                       (memq expected value)
                     (eq value expected)))))))))

(ert-deftest slackit-contract-chat-views-enable-responsive-geometry ()
  (slackit-test-with-app (app "responsive-view")
    (slackit-state-put-conversation
     (slackit-runtime-state app)
     '((id . "C1") (name . "general") (is_member . t)))
    (cl-letf (((symbol-function 'slackit-history-load-latest)
               (lambda (&rest _arguments) nil)))
      (let ((view (slackit-room-open app "C1" nil)))
        (should (memq 'geometry (appkit-view-parts view)))
        (with-current-buffer (appkit-view-buffer view)
          (should appkit-view--responsive-geometry-p))))))


(ert-deftest slackit-contract-geometry-redraws-timestamps-at-new-width ()
  (with-temp-buffer
    (let ((invalidations (appkit-invalidations-create))
          rendered-keys)
      (setf (appkit-invalidations-parts invalidations) '(geometry)
            (appkit-invalidations-entry-keys invalidations) '("changed"))
      (cl-letf (((symbol-function 'appkit-view-responsive-width)
                 (lambda (&rest _arguments) 44))
                ((symbol-function 'appkit-chat-timeline-live-p)
                 (lambda () t))
                ((symbol-function 'appkit-chat-timeline-keys)
                 (lambda () '("one" "two")))
                ((symbol-function 'appkit-view-pending-events-snapshot)
                 (lambda (_view) nil))
                ((symbol-function 'appkit-view-acknowledge-events)
                 (lambda (&rest _arguments) nil))
                ((symbol-function 'slackit-room--render)
                 (lambda (keys _resources)
                   (setq rendered-keys keys))))
        (slackit-room--sync 'fake-view invalidations))
      (should (= 44 fill-column))
      (should (equal '("changed" "one" "two") rendered-keys))))
  (slackit-test-with-app (app "responsive-time")
    (let ((state (slackit-runtime-state app))
          (message
           '((ts . "1710000000.000001")
             (user . "U1")
             (text . "message"))))
      (slackit-state-put-user
       state '((id . "U1") (profile . ((display_name . "Alice")))))
      (with-temp-buffer
        (let ((slackit-show-avatars nil)
              (slackit-right-align-timestamps t)
              (inhibit-read-only t))
          (cl-labels
              ((render-target
                (width)
                (erase-buffer)
                (cl-letf (((symbol-function 'appkit-view-responsive-width)
                           (lambda (&rest _arguments) width))
                          ((symbol-function
                            'appkit-chat-avatar-two-line-pixel-size)
                           (lambda () 32)))
                  (slackit-render-message-row
                   app state message
                   (slackit-room--message-context nil message)))
                (let* ((positions
                        (number-sequence
                         (point-min) (max (point-min) (1- (point-max)))))
                       (timestamp-position
                        (seq-find
                         (lambda (position)
                           (eq (get-text-property position 'face)
                               'slackit-timestamp))
                         positions))
                       (timestamp-line-start
                        (and timestamp-position
                             (save-excursion
                               (goto-char timestamp-position)
                               (line-beginning-position))))
                       (spacer-position
                        (and timestamp-position
                             (seq-find
                              (lambda (position)
                                (let ((display
                                       (get-text-property position 'display)))
                                  (and (listp display)
                                       (eq (car display) 'space)
                                       (eq (cadr display) :align-to))))
                              (number-sequence
                               timestamp-line-start timestamp-position)))))
                  (nth 2
                       (get-text-property spacer-position 'display)))))
            (should (= 35 (render-target 40)))
            (should (= 55 (render-target 60)))))))))

(ert-deftest slackit-contract-delayed-history-cannot-overwrite-or-resurrect ()
  (let* ((state (slackit-state-create))
         (captured (slackit-account-state-revision state)))
    (slackit-state-upsert-message
     state "C1" '((ts . "1710000000.000001") (text . "live edit")))
    (slackit-state-merge-message-page
     state "C1"
     '(((ts . "1710000000.000001") (text . "stale page")))
     captured)
    (should (equal "live edit"
                   (slackit-normalize-get
                    (slackit-state-message state "C1" "1710000000.000001")
                    'text)))
    (setq captured (slackit-account-state-revision state))
    (slackit-state-delete-message state "C1" "1710000000.000001")
    (slackit-state-merge-message-page
     state "C1"
     '(((ts . "1710000000.000001") (text . "resurrected")))
     captured)
    (should-not (slackit-state-message state "C1" "1710000000.000001"))))

(ert-deftest slackit-contract-thread-envelope-classification-is-exact ()
  (let ((state (slackit-state-create)))
    (slackit-state-apply-event
     state
     (slackit-normalize-event
      '((type . "message") (channel . "C1")
        (ts . "1710000000.000002") (thread_ts . "1710000000.000001")
        (text . "reply"))))
    (should-not (member "1710000000.000002"
                        (slackit-state-top-level-keys state "C1")))
    (should (member "1710000000.000002"
                    (slackit-state-reply-keys
                     state "C1" "1710000000.000001")))
    (slackit-state-apply-event
     state
     (slackit-normalize-event
      '((type . "message") (subtype . "thread_broadcast")
        (channel . "C1") (ts . "1710000000.000003")
        (thread_ts . "1710000000.000001") (text . "broadcast"))))
    (should (= 1 (cl-count "1710000000.000003"
                           (slackit-state-top-level-keys state "C1")
                           :test #'equal)))
    (should (= 1 (cl-count "1710000000.000003"
                           (slackit-state-reply-keys
                            state "C1" "1710000000.000001")
                           :test #'equal)))))

(ert-deftest slackit-contract-structured-composer-serializes-exact-ids ()
  (let* ((user (appkit-chatbuf-input-object-string
                "@same" '(:type user :id "U111" :label "same")))
         (channel (appkit-chatbuf-input-object-string
                   "#same" '(:type channel :id "C222" :label "same")))
         (input (concat "hello " user "and " channel "<&>")))
    (should (equal "hello <@U111> and <#C222> &lt;&amp;&gt;"
                   (slackit-compose-serialize input)))))

(ert-deftest slackit-contract-reaction-intent-is-serialized-and-event-owned ()
  (slackit-test-with-app (app "reaction")
    (let* ((state (slackit-runtime-state app))
           add-success remove-success
           (add-count 0)
           (remove-count 0))
      (slackit-state-put-team-self
       state '((id . "T1")) '((id . "U1") (name . "self")))
      (slackit-state-upsert-message
       state "C1" '((ts . "1.000001") (user . "U2") (text . "x")))
      (cl-letf (((symbol-function 'slackit-api-add-reaction)
                 (lambda (_app _channel _ts _name &rest arguments)
                   (cl-incf add-count)
                   (setq add-success (plist-get arguments :on-success))))
                ((symbol-function 'slackit-api-remove-reaction)
                 (lambda (_app _channel _ts _name &rest arguments)
                   (cl-incf remove-count)
                   (setq remove-success (plist-get arguments :on-success)))))
        (slackit-reaction-toggle app "C1" "1.000001" "wave")
        (slackit-reaction-toggle app "C1" "1.000001" "wave")
        (should (= 1 add-count))
        (should (= 0 remove-count))
        (funcall add-success '((ok . t)))
        (should (= 1 remove-count))
        (funcall remove-success '((ok . t)))
        (should-not (slackit-normalize-get
                     (slackit-state-message state "C1" "1.000001")
                     'reactions))))))

(ert-deftest slackit-contract-read-marks-coalesce-without-fabrication ()
  (slackit-test-with-app (app "read")
    (let ((callbacks nil)
          (calls nil)
          (state (slackit-runtime-state app)))
      (cl-letf (((symbol-function 'slackit-api-mark-conversation)
                 (lambda (_app _conversation ts &rest arguments)
                   (push ts calls)
                   (push (plist-get arguments :on-success) callbacks))))
        (slackit-read-mark app "C1" "1.000001")
        (slackit-read-mark app "C1" "1.000003")
        (should (equal '("1.000001") calls))
        (should-not (slackit-state-read-ts state "C1"))
        (funcall (car callbacks) '((ok . t)))
        (should (equal '("1.000003" "1.000001") calls))
        (should-not (slackit-state-read-ts state "C1"))
        (funcall (car callbacks) '((ok . t)))
        (should-not (gethash '(mark "C1")
                             (appkit-app-request-table app)))))))

(ert-deftest slackit-contract-stop-revokes-operations-and-secrets ()
  (let* ((app (slackit-runtime-start-account
               "stop"
               (list :token "xoxp-STOP-CANARY"
                     :cookie "xoxd-STOP-CANARY")))
         (transport (slackit-runtime-transport app))
         (credential (slackit-transport-credential transport))
         (generation (slackit-runtime-generation app)))
    (slackit-runtime-operation-begin app '(write))
    (should (= 1 (hash-table-count (appkit-app-request-table app))))
    (slackit-runtime-stop-account app)
    (should-not (appkit-app-live-p app))
    (should-not (slackit-runtime-current-p app generation))
    (should (= 0 (hash-table-count (appkit-app-request-table app))))
    (should-not (slackit-credential-token credential))
    (should-not (slackit-credential-cookie credential))
    (should-not (slackit-runtime-account "stop"))))

(ert-deftest slackit-contract-appkit-root-room-thread-smoke ()
  (slackit-test-with-app (app "surface")
    (let ((state (slackit-runtime-state app)))
      (slackit-state-put-team-self
       state '((id . "T1") (name . "Team"))
       '((id . "U1") (name . "self")))
      (slackit-state-put-user
       state '((id . "U2") (name . "alice")
               (profile . ((display_name . "Alice")))))
      (slackit-state-put-conversation
       state '((id . "C1") (name . "general")
               (is_channel . t) (is_member . t)))
      (slackit-state-set-bootstrap-complete state 'identity)
      (slackit-state-set-bootstrap-complete state 'users)
      (slackit-state-set-bootstrap-complete state 'conversations)
      (let ((root (slackit-root-open app nil)))
        (with-current-buffer (appkit-view-buffer root)
          (should (derived-mode-p 'slackit-root-mode))
          (should (string-match-p "#general" (buffer-string)))))
      (cl-letf (((symbol-function 'slackit-api-conversation-history)
                 (lambda (_app _conversation &rest arguments)
                   (funcall
                    (plist-get arguments :on-success)
                    '((messages
                       . (((ts . "1.000001") (user . "U2")
                           (text . "hello <@U2>"))))
                      (response_metadata . ((next_cursor . "")))))))
                ((symbol-function 'slackit-api-conversation-replies)
                 (lambda (_app _conversation _root &rest arguments)
                   (funcall
                    (plist-get arguments :on-success)
                    '((messages
                       . (((ts . "1.000001") (user . "U2")
                           (text . "hello <@U2>"))
                          ((ts . "1.000002") (thread_ts . "1.000001")
                           (user . "U1") (text . "reply"))))
                      (response_metadata . ((next_cursor . ""))))))))
        (let ((room (slackit-room-open app "C1" nil)))
          (with-current-buffer (appkit-view-buffer room)
            (should (derived-mode-p 'slackit-room-mode))
            (should (string-match-p "hello @Alice" (buffer-string)))
            (should (appkit-chatbuf-prompt-button-live-p))))
        (let ((thread (slackit-thread-open app "C1" "1.000001" nil)))
          (with-current-buffer (appkit-view-buffer thread)
            (should (derived-mode-p 'slackit-thread-mode))
            (should (string-match-p "reply" (buffer-string)))
            (should (appkit-chatbuf-prompt-button-live-p))))))))

(ert-deftest slackit-contract-emoji-renders-body-and-actionable-reactions ()
  (slackit-test-with-app (app "emoji")
    (let* ((state (slackit-runtime-state app))
           (message
            '((channel . "C1")
              (ts . "1.000001")
              (user . "U2")
              (text . "please :pray: :laughing: :slightly_smiling_face: :+1::skin-tone-3: :unknown:")
              (reactions
               . (((name . "laughing") (count . 2) (users . ("U1")))
                  ((name . "+1::skin-tone-3") (count . 1) (users . ("U2"))))))))
      (slackit-state-put-team-self
       state '((id . "T1")) '((id . "U1") (name . "self")))
      (slackit-state-put-user state '((id . "U2") (name . "alice")))
      (with-temp-buffer
        (let ((slackit-show-avatars nil)
              toggled)
          (cl-letf (((symbol-function 'slackit-reaction-toggle)
                     (lambda (&rest arguments)
                       (setq toggled arguments))))
            (slackit-render-message-row
             app state message
             (slackit-room--message-context nil message))
            (should (string-match-p
                     "🙏.*😆.*🙂.*👍🏼.*:unknown:" (buffer-string)))
            (should (string-match-p "😆 2 👍🏼 1" (buffer-string)))
            (let ((button (next-button (point-min))))
              (should button)
              (button-activate button)
              (should (equal (list app "C1" "1.000001" "laughing")
                             toggled)))))
        (should (equal
                 "please :pray: :laughing: :slightly_smiling_face: :+1::skin-tone-3: :unknown:"
                 (slackit-normalize-get message 'text)))))))

(ert-deftest slackit-contract-avatar-prefers-circular-derived-image ()
  (let ((file (make-temp-file "slackit-round-avatar-" nil ".png")))
    (unwind-protect
        (slackit-test-with-app (app "round-avatar")
          (let* ((user
                  '((id . "U1")
                    (profile
                     . ((image_72
                         . "https://ca.slack-edge.com/avatar.png")))))
                 (key (slackit-avatar-resource-key app user))
                 (mtime (file-attribute-modification-time
                         (file-attributes file))))
            (clrhash slackit-avatar--sources)
            (clrhash slackit-avatar--image-cache)
            (puthash key (list file mtime) slackit-avatar--sources)
            (cl-letf (((symbol-function
                        'appkit-media-circular-image-from-file)
                       (lambda (source size)
                         (should (equal source file))
                         (should (= size 32))
                         'circular-image))
                      ((symbol-function 'create-image)
                       (lambda (&rest _arguments)
                         (ert-fail "square fallback should not run"))))
              (should (eq 'circular-image
                          (slackit-avatar-cached-image app user 32))))))
      (clrhash slackit-avatar--sources)
      (clrhash slackit-avatar--image-cache)
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest slackit-contract-file-images-use-origin-bound-private-previews ()
  (slackit-test-with-app (app "media")
    (let* ((root (make-temp-file "slackit-media-test-" t))
           (slackit-media-cache-directory
            (expand-file-name "media/" root))
           (public-url "https://cdn.example.invalid/public-image.png")
           (private-url
            "https://files.slack.com/files-pri/T1-F1/private-image.png")
           (thumbnail-url
            "https://files.slack.com/files-tmb/T1-F1/private-image_1024.png")
           (message
            `((channel . "C1")
              (ts . "1.000001")
              (blocks
               . (((type . "image")
                   (block_id . "B1")
                   (title . ((type . "plain_text")
                             (text . "Public image")))
                   (alt_text . "preview")
                   (image_url . ,public-url))))
              (files
               . (((id . "F1")
                   (name . "private.png")
                   (mimetype . "image/png")
                   (permalink . "https://workspace.slack.com/files/U1/F1")
                   (url_private . ,private-url)
                   (thumb_1024 . ,thumbnail-url))))))
           public-source
           private-source
           private-headers
           opened-file)
      (unwind-protect
          (progn
            (clrhash slackit-media--fetches)
            (clrhash slackit-media--failures)
            (clrhash slackit-media--image-cache)
            (setq message (slackit-normalize-message message "C1"))
            (should
             (equal thumbnail-url
                    (slackit-normalize-get
                     (car (slackit-normalize-get message 'files))
                     'thumb_1024)))
            (cl-letf
                (((symbol-function
                   'appkit-media-inline-image-rendering-available-p)
                  (lambda () t))
                 ((symbol-function
                   'appkit-media-cache-image-resource-async)
                  (lambda (resource _cache _success _failure &rest arguments)
                    (should-not arguments)
                    (setq public-source (alist-get 'url resource))
                    nil))
                 ((symbol-function 'plz)
                  (lambda (method url &rest arguments)
                    (should (eq method 'get))
                    (should-not (member "--location" plz-curl-default-args))
                    (setq private-source url
                          private-headers (plist-get arguments :headers))
                    (let* ((as (plist-get arguments :as))
                           (file (cadr as)))
                      (with-temp-file file
                        (set-buffer-multibyte nil)
                        (insert "synthetic image"))
                      (funcall (plist-get arguments :then) file))
                    nil))
                 ((symbol-function 'appkit-media-preview-image-from-file)
                  (lambda (_file) 'decoded-private-image))
                 ((symbol-function 'appkit-media-insert-image-slices)
                  (lambda (image &rest _arguments)
                    (should (eq image 'decoded-private-image))
                    (insert "[decoded private preview]")))
                 ((symbol-function 'appkit-media-open-resource)
                  (lambda (resource &rest arguments)
                    (should (eq 'image (plist-get arguments :kind)))
                    (should (equal "slackit"
                                   (plist-get arguments :client-label)))
                    (should-not (alist-get 'url resource))
                    (setq opened-file (alist-get 'file resource)))))
              (slackit-media-ensure-message app message)
              (should (equal public-url public-source))
              (should (equal thumbnail-url private-source))
              (should
               (equal "Bearer xoxp-CANARY-TOKEN"
                      (cdr (assoc "Authorization" private-headers))))
              (should
               (equal "d=xoxd-CANARY-COOKIE"
                      (cdr (assoc "Cookie" private-headers))))
              (should (equal "https://app.slack.com/"
                             (cdr (assoc "Referer" private-headers))))
              (should-not (assoc "Origin" private-headers))
              (should-not
               (string-match-p
                (regexp-quote private-url)
                (prin1-to-string
                 (slackit-media-message-resource-keys app message))))
              (let* ((private-item
                      (seq-find
                       (lambda (item)
                         (eq (plist-get item :class) 'file))
                       (slackit-media--message-items app message)))
                     (context (slackit-media--item-context private-item))
                     (action (plist-get context :open-action))
                     (context-text (prin1-to-string context))
                     (key (plist-get private-item :resource-key))
                     (cached-file (slackit-media--cached-file key)))
                (should (functionp action))
                (should (file-regular-p cached-file))
                (unless (memq system-type '(ms-dos windows-nt cygwin))
                  (should (= #o600
                             (logand #o777 (file-modes cached-file)))))
                (should-not
                 (string-match-p (regexp-quote private-url) context-text))
                (should-not
                 (string-match-p (regexp-quote thumbnail-url) context-text))
                (funcall action)
                (should (equal cached-file opened-file)))
              (should-not
               (slackit-media--private-source-p
                "https://files.slack.com.attacker.invalid/files-tmb/T1-F1/x.png"))
              (with-temp-buffer
                (slackit-media-insert-message-cards app message)
                (should (string-match-p "Public image" (buffer-string)))
                (should (string-match-p "private.png" (buffer-string)))
                (should (string-match-p
                         "decoded private preview" (buffer-string)))
                (should-not (string-match-p
                             (regexp-quote private-url)
                             (buffer-string))))))
        (clrhash slackit-media--fetches)
        (clrhash slackit-media--failures)
        (clrhash slackit-media--image-cache)
        (when (file-directory-p root) (delete-directory root t))))))

(ert-deftest slackit-contract-custom-emoji-catalog-resolves-aliases-and-images ()
  (slackit-test-with-app (app "custom-emoji")
    (let (published ensured-key ensured-url)
      (cl-letf
          (((symbol-function 'slackit-api-emoji-list)
            (lambda (_app &rest arguments)
              (funcall
               (plist-get arguments :on-success)
               '((emoji
                  . ((happy . "alias:laughing")
                     (party
                      . "https://emoji.slack-edge.com/T1/party/hash.gif")))))))
           ((symbol-function 'slackit-runtime-publish-resource)
            (lambda (_app resource) (setq published resource)))
           ((symbol-function 'slackit-media-ensure-public-image)
            (lambda (_app key url)
              (setq ensured-key key ensured-url url)))
           ((symbol-function 'slackit-media-cached-image)
            (lambda (_key) 'raw-custom-image))
           ((symbol-function 'appkit-chat-avatar-line-pixel-height)
            (lambda () 18))
           ((symbol-function 'appkit-chat-avatar-resize-image)
            (lambda (image size)
              (should (eq image 'raw-custom-image))
              (should (= size 18))
              'inline-custom-image)))
        (slackit-emoji-load-catalog app)
        (should (equal published (slackit-emoji-resource-key app)))
        (should-not
         (gethash '(emoji-catalog) (appkit-app-request-table app)))
        (should (equal "😆"
                       (slackit-emoji-display-string app "laughing")))
        (should (equal "😆"
                       (slackit-emoji-display-string app "happy")))
        (let ((display (slackit-emoji-display-string app "party")))
          (should (equal " " (substring-no-properties display)))
          (should (eq 'inline-custom-image
                      (get-text-property 0 'display display))))
        (let* ((candidates (slackit-emoji-completion-candidates app))
               (laughing
                (seq-find
                 (lambda (candidate)
                   (equal ":laughing:"
                          (appkit-chat-completion-candidate-label candidate)))
                 candidates))
               (party
                (seq-find
                 (lambda (candidate)
                   (equal ":party:"
                          (appkit-chat-completion-candidate-label candidate)))
                 candidates)))
          (should laughing)
          (should party)
          (should
           (equal ":laughing:"
                  (appkit-chat-completion-candidate-insert laughing))))
        (let ((message '((text . ":party:"))))
          (slackit-emoji-ensure-message app message)
          (should
           (equal "https://emoji.slack-edge.com/T1/party/hash.gif"
                  ensured-url))
          (should (member ensured-key
                          (slackit-emoji-message-resource-keys app message))))))))

(ert-deftest slackit-contract-entity-decoding-is-one-pass ()
  (should
   (equal "&lt; < > &"
          (slackit-render-decode-entities
           "&amp;lt; &lt; &gt; &amp;"))))

(ert-deftest slackit-contract-adjacent-identical-mentions-stay-distinct ()
  (let* ((mention
          (appkit-chatbuf-input-object-string
           "@same" '(:type user :id "U1" :label "same")))
         (input (concat mention mention)))
    (should (equal "<@U1> <@U1> "
                   (slackit-compose-serialize input)))))
(ert-deftest slackit-contract-http-write-snapshot-cannot-regress-realtime ()
  (let ((state (slackit-state-create)))
    (slackit-state-upsert-message
     state "C1" '((ts . "1.000001") (text . "old")))
    (let ((captured
           (slackit-state-message-revision state "C1" "1.000001")))
      (slackit-state-upsert-message
       state "C1" '((ts . "1.000001") (text . "newer realtime edit")))
      (should-not
       (slackit-state-merge-write-snapshot
        state "C1" '((ts . "1.000001") (text . "stale HTTP edit"))
        captured))
      (should
       (equal "newer realtime edit"
              (slackit-normalize-get
               (slackit-state-message state "C1" "1.000001") 'text))))
    (slackit-state-delete-message state "C1" "1.000001")
    (should-not
     (slackit-state-merge-write-snapshot
      state "C1" '((ts . "1.000001") (text . "late HTTP receipt")) nil))
    (should-not (slackit-state-message state "C1" "1.000001"))))

(ert-deftest slackit-contract-thread-cursor-appends-newer-replies ()
  (slackit-test-with-app (app "thread-pages")
    (let ((state (slackit-runtime-state app)))
      (slackit-state-put-team-self
       state '((id . "T1")) '((id . "U1") (name . "self")))
      (slackit-state-put-conversation
       state '((id . "C1") (name . "general")
               (is_channel . t) (is_member . t)))
      (slackit-state-upsert-message
       state "C1" '((channel . "C1") (ts . "1.000001")
                    (user . "U1") (text . "root")))
      (cl-letf
          (((symbol-function 'slackit-api-conversation-replies)
            (lambda (_app _conversation _root &rest arguments)
              (let ((cursor (plist-get arguments :cursor)))
                (funcall
                 (plist-get arguments :on-success)
                 (if cursor
                     '((messages
                        . (((channel . "C1") (ts . "1.000003")
                            (thread_ts . "1.000001") (user . "U1")
                            (text . "later reply"))))
                       (response_metadata . ((next_cursor . ""))))
                   '((messages
                      . (((channel . "C1") (ts . "1.000001")
                          (user . "U1") (text . "root"))
                         ((channel . "C1") (ts . "1.000002")
                          (thread_ts . "1.000001") (user . "U1")
                          (text . "early reply"))))
                     (response_metadata . ((next_cursor . "next"))))))))))
        (let ((view (slackit-thread-open app "C1" "1.000001" nil)))
          (with-current-buffer (appkit-view-buffer view)
            (should (equal "1.000001"
                           (appkit-chat-history-window-first-key)))
            (should (equal "1.000002"
                           (appkit-chat-history-window-last-key)))
            (slackit-room-load-older)
            (appkit-sync-invalidations view)
            (should-not (appkit-chat-history-window-last-key))
            (should (equal '("1.000001" "1.000002" "1.000003")
                           (appkit-chat-timeline-keys)))
            (should (string-match-p "later reply" (buffer-string)))))))))

(ert-deftest slackit-contract-dm-labels-never-pretend-to-be-channels ()
  (let ((state (slackit-state-create)))
    (slackit-state-put-user state '((id . "U2") (name . "alice")))
    (slackit-state-put-conversation
     state '((id . "D1") (user . "U2") (is_im . t) (is_member . t)))
    (should (equal "alice" (slackit-state-conversation-label state "D1")))
    (should-not
     (string-prefix-p "#"
                      (slackit-room--header state "D1")))))

(ert-deftest slackit-contract-capture-rejects-unsafe-origin-and-missing-identity ()
  (slackit-test-with-auth-directory (root)
    (let ((slackit-login-url "http://my.slack.com/customize")
          called)
      (cl-letf (((symbol-function 'browser-session-capture)
                 (lambda (&rest _arguments) (setq called t))))
        (should-error (slackit-auth-capture "work") :type 'user-error)
        (should-not called)))
    (let ((capture (make-temp-file
                    (expand-file-name "capture-" root) nil ".json")))
      (slackit-test--write-private-json
       capture (slackit-test--capture-payload :team-id nil))
      (should-error (slackit-auth-import-capture "work" capture))
      (should-not (slackit-auth-available-p "work"))
      (delete-file capture))))

(ert-deftest slackit-contract-browser-capture-is-globally-serialized ()
  (slackit-test-with-auth-directory (_root)
    (cl-letf (((symbol-function 'browser-session-capture)
               (lambda (&rest _arguments) nil)))
      (slackit-auth-capture "first")
      (should-error (slackit-auth-capture "second") :type 'user-error)
      (should (slackit-auth-capture-running-p "first"))
      (should-not (slackit-auth-capture-running-p "second")))))

(ert-deftest slackit-contract-pinned-auth-identity-rejects-bootstrap-mismatch ()
  (let* ((app
          (slackit-runtime-start-account
           "identity"
           (list :token "xoxp-CANARY"
                 :cookie "xoxd-CANARY"
                 :team-id "T1"
                 :user-id "U1")))
         (operation
          (slackit-runtime-operation-begin app '(bootstrap))))
    (unwind-protect
        (progn
          (slackit--bootstrap-identity-success
           app operation
           '((team_id . "T2") (user_id . "U2")
             (team . "wrong") (user . "wrong"))
           (lambda (_app)
             (ert-fail "identity mismatch must not start realtime")))
          (should
           (equal "identity_mismatch"
                  (slackit-account-state-bootstrap-error
                   (slackit-runtime-state app))))
          (should-not
           (slackit-state-self-id (slackit-runtime-state app))))
      (slackit-runtime-stop-account app))))

(ert-deftest slackit-contract-evil-preserves-native-motions-and-composer ()
  (unless (require 'evil nil t)
    (ert-skip "Evil development dependency unavailable"))
  (require 'slackit-evil)
  (slackit-evil-setup)
  (with-temp-buffer
    (slackit-room-mode)
    (evil-normal-state)
    (slackit-room-timeline-mode 1)
    (appkit-evil-normalize-keymaps)
    (should (eq (key-binding (kbd "g g")) #'evil-goto-first-line))
    (should (eq (key-binding (kbd "e")) #'evil-forward-word-end))
    (should (eq (key-binding (kbd "i")) #'appkit-chatbuf-focus-input))
    (should (eq (key-binding (kbd "E")) #'slackit-actions-edit))
    (slackit-room-timeline-mode -1)
    (appkit-evil-normalize-keymaps)
    (should (eq (key-binding (kbd "i")) #'evil-insert))))

(ert-deftest slackit-contract-transients-retain-exact-message-scope ()
  (slackit-test-with-app (app "transient")
    (let ((state (slackit-runtime-state app))
          (kill-ring nil))
      (slackit-state-put-team-self
       state '((id . "T1")) '((id . "U1") (name . "self")))
      (slackit-state-put-conversation
       state '((id . "C1") (name . "general")
               (is_channel . t) (is_member . t)))
      (cl-letf
          (((symbol-function 'slackit-api-conversation-history)
            (lambda (_app _conversation &rest arguments)
              (funcall
               (plist-get arguments :on-success)
               '((messages
                  . (((channel . "C1") (ts . "1.000001")
                      (user . "U1") (text . "scoped text"))))
                 (response_metadata . ((next_cursor . ""))))))))
        (let* ((view (slackit-room-open app "C1" nil))
               scope)
          (with-current-buffer (appkit-view-buffer view)
            (goto-char (point-min))
            (let ((match
                   (text-property-search-forward
                    slackit-message-key-property "1.000001" #'equal)))
              (should match)
              (goto-char (prop-match-beginning match)))
            (setq scope (slackit-transient--capture-room-scope t)))
          (with-temp-buffer
            (slackit-transient-actions-copy-text scope))
          (should (equal "scoped text" (current-kill 0 t)))
          (slackit-runtime-stop-account app)
          (should-error
           (slackit-transient-actions-copy-text scope)
           :type 'user-error))))))

(ert-deftest slackit-contract-mpim-mark-events-normalize-read-state ()
  (should
   (equal '(:kind conversation-mark
            :conversation-id "G1"
            :ts "2.000001")
          (slackit-normalize-event
           '((type . "mpim_marked")
             (channel . "G1")
             (ts . "2.000001"))))))

(ert-deftest slackit-contract-realtime-requires-authenticated-identity ()
  (slackit-test-with-app (app "realtime-unbound")
    (let (websocket-opened)
      (cl-letf (((symbol-function 'websocket-open)
                 (lambda (&rest _arguments)
                   (setq websocket-opened t))))
        (slackit-realtime-start app))
      (should-not websocket-opened)
      (should
       (eq 'protocol-error
           (slackit-account-state-connection-status
            (slackit-runtime-state app))))
      (should-not
       (slackit-transport-reconnect-timer
        (slackit-runtime-transport app))))))

(ert-deftest slackit-contract-browser-session-realtime-reaches-hello ()
  (slackit-test-with-app (app "browser-realtime")
    (let* ((transport (slackit-runtime-transport app))
           (credential (slackit-transport-credential transport))
           opened-url
           opened-headers)
      (setf (slackit-credential-token credential) "xoxc-REALTIME-CANARY"
            (slackit-credential-cookie credential)
            "d=xoxd-REALTIME-CANARY; d-s=DS; lc=LC"
            (slackit-credential-team-id credential) "T1"
            (slackit-credential-user-id credential) "U1")
      (cl-letf
          (((symbol-function 'websocket-open)
            (lambda (url &rest arguments)
              (setq opened-url url
                    opened-headers
                    (plist-get arguments :custom-header-alist))
              (should-not url-cookie-storage)
              (should-not url-cookie-secure-storage)
              (funcall (plist-get arguments :on-open) 'synthetic-websocket)
              (funcall
               (plist-get arguments :on-message)
               'synthetic-websocket
               (make-websocket-frame
                :opcode 'text
                :payload "{\"type\":\"hello\"}"
                :completep t))
              'synthetic-websocket))
           ((symbol-function 'websocket-close)
            (lambda (&rest _arguments) nil)))
        (slackit-realtime-start app)
        (should (string-prefix-p
                 "wss://wss-primary.slack.com/?" opened-url))
        (should (string-match-p
                 "token=xoxc-REALTIME-CANARY" opened-url))
        (should (string-match-p
                 "agent_version%3D1785403654" opened-url))
        (should (equal "d=xoxd-REALTIME-CANARY"
                       (cdr (assoc "Cookie" opened-headers))))
        (should (equal "https://app.slack.com"
                       (cdr (assoc "Origin" opened-headers))))
        (should (slackit-transport-ready-p transport))
        (should
         (eq 'ready
             (slackit-account-state-connection-status
              (slackit-runtime-state app))))
        (should-not
         (string-match-p
          "REALTIME-CANARY"
          (slackit-runtime-redact app opened-url)))))))

(provide 'slackit-contract-test)

;;; slackit-contract-test.el ends here
