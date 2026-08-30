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

(ert-deftest slackit-contract-upload-capability-is-exact-and-credential-free ()
  (let* ((file (make-temp-file "slackit-upload-contract-" nil ".png"))
         (url "https://files.slack.com/upload/v1/SAFE-CAPABILITY")
         config)
    (unwind-protect
        (progn
          (write-region "png" nil file nil 'silent)
          (should (slackit-upload-url-p url))
          (dolist (rejected
                   '("http://files.slack.com/upload/v1/x"
                     "https://files.slack.com.evil.invalid/upload/v1/x"
                     "https://files.slack.com/files-pri/x"
                     "https://user@files.slack.com/upload/v1/x"
                     "https://files.slack.com/upload/v1/x#fragment"))
            (should-not (slackit-upload-url-p rejected)))
          (setq config (slackit-upload--curl-config url file))
          (should (string-match-p (regexp-quote url) config))
          (should (string-match-p (regexp-quote file) config))
          (should-not (string-match-p "Authorization\\|Cookie\\|xox" config))
          (should (member "--max-redirs" slackit-upload--curl-args))
          (should (member "--retry" slackit-upload--curl-args))
          (should-not (member url slackit-upload--curl-args))
          (should-not (member file slackit-upload--curl-args)))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest slackit-contract-upload-api-encodes-one-threaded-file-share ()
  (let (calls)
    (cl-letf (((symbol-function 'slackit-api-request)
               (lambda (&rest arguments)
                 (push arguments calls)
                 :request)))
      (slackit-api-get-upload-url
       :app "fixture.png" 3 :owner :view)
      (slackit-api-complete-upload
       :app '(((id . "F1") (title . "fixture.png")))
       "D1" :thread-ts "1.000001" :initial-comment "caption"
       :owner :view))
    (pcase-let* ((`(,complete ,negotiate) calls)
                 (complete-params (plist-get (cddr complete) :parameters))
                 (files-json (alist-get 'files complete-params)))
      (should (equal (cadr negotiate) "files.getUploadURLExternal"))
      (should (equal 'post (plist-get (cddr negotiate) :method)))
      (should (equal '((filename . "fixture.png") (length . 3))
                     (plist-get (cddr negotiate) :parameters)))
      (should (equal (cadr complete) "files.completeUploadExternal"))
      (should (equal "D1" (alist-get 'channel_id complete-params)))
      (should (equal "1.000001" (alist-get 'thread_ts complete-params)))
      (should (equal "caption" (alist-get 'initial_comment complete-params)))
      (should
       (equal "F1"
              (alist-get
               'id
               (car
                (json-parse-string
                 files-json :object-type 'alist :array-type 'list))))))))

(ert-deftest slackit-contract-composer-upload-is-one-appkit-owned-share ()
  (slackit-test-with-app (app "composer-upload")
    (let* ((state (slackit-runtime-state app))
           (file (make-temp-file "slackit-composer-contract-" nil ".png"))
           view
           negotiated
           streamed
           completed)
      (unwind-protect
          (progn
            (write-region "png" nil file nil 'silent)
            (slackit-state-put-team-self
             state '((id . "T1")) '((id . "U1") (name . "self")))
            (slackit-state-put-conversation
             state '((id . "D1") (is_im . t) (user . "U1")))
            (cl-letf (((symbol-function 'slackit-history-load-latest)
                       (lambda (&rest _) nil)))
              (setq view (slackit-room-open app "D1" nil)))
            (with-current-buffer (appkit-view-buffer view)
              (appkit-chatbuf-input-set-text "caption")
              (goto-char (point-max))
              (slackit-compose-attach-file file)
              (should (= 1 (length (slackit-compose-attachments))))
              (should
               (equal "caption"
                      (slackit-compose-serialize
                       (appkit-chatbuf-input-state))))
              (cl-letf
                  (((symbol-function 'slackit-api-get-upload-url)
                    (lambda (_app filename length &rest options)
                      (setq negotiated (list filename length))
                      (funcall
                       (plist-get options :on-success)
                       '((ok . t)
                         (upload_url
                          . "https://files.slack.com/upload/v1/TEST")
                         (file_id . "F1")))
                      :negotiate))
                   ((symbol-function 'slackit-upload-file)
                    (lambda (_app owner url path &rest options)
                      (setq streamed (list owner url path))
                      (funcall (plist-get options :on-progress) :transfer 0.5)
                      (funcall (plist-get options :on-success) :transfer)
                      :transfer))
                   ((symbol-function 'slackit-api-complete-upload)
                    (lambda (_app files conversation-id &rest options)
                      (setq completed
                            (list files conversation-id
                                  (plist-get options :thread-ts)
                                  (plist-get options :initial-comment)
                                  (plist-get options :owner)))
                      (funcall (plist-get options :on-success) '((ok . t)))
                      :complete)))
                (slackit-compose-submit))
              (should (equal (list (file-name-nondirectory file) 3)
                             negotiated))
              (should (eq view (car streamed)))
              (should
               (equal "https://files.slack.com/upload/v1/TEST"
                      (cadr streamed)))
              (should (equal file (caddr streamed)))
              (should
               (equal
                (list
                 (list
                  (list (cons 'id "F1")
                        (cons 'title (file-name-nondirectory file))))
                 "D1" nil "caption" view)
                completed))
              (should-not (appkit-compose-operation-active-p))
              (let ((event
                     (car (appkit-view-pending-events-snapshot view))))
                (should (eq 'compose-success (plist-get event :kind)))
                (slackit-compose-apply-settlement event))
              (should (string-empty-p (appkit-chatbuf-input-state)))
              (should-not (slackit-compose-attachments))))
        (when (file-exists-p file) (delete-file file))))))

(ert-deftest slackit-contract-canceling-upload-retains-atomic-draft ()
  (slackit-test-with-app (app "composer-upload-cancel")
    (let* ((state (slackit-runtime-state app))
           (file (make-temp-file "slackit-composer-cancel-" nil ".png"))
           view
           request
           canceled)
      (unwind-protect
          (progn
            (write-region "png" nil file nil 'silent)
            (slackit-state-put-team-self
             state '((id . "T1")) '((id . "U1") (name . "self")))
            (slackit-state-put-conversation
             state '((id . "D1") (is_im . t) (user . "U1")))
            (cl-letf (((symbol-function 'slackit-history-load-latest)
                       (lambda (&rest _) nil)))
              (setq view (slackit-room-open app "D1" nil)))
            (with-current-buffer (appkit-view-buffer view)
              (goto-char (point-max))
              (slackit-compose-attach-file file)
              (cl-letf
                  (((symbol-function 'slackit-api-get-upload-url)
                    (lambda (&rest _)
                      (setq request :negotiating)
                      request))
                   ((symbol-function 'slackit-http-cancel)
                    (lambda (current)
                      (setq canceled current)
                      t)))
                (slackit-compose-submit)
                (should (slackit-compose-upload-active-p))
                (should (string-match-p
                         "Preparing"
                         (slackit-compose-upload-card)))
                (slackit-compose-cancel-upload))
              (should (eq :negotiating canceled))
              (should-not (appkit-compose-operation-active-p))
              (should (= 1 (length (slackit-compose-attachments))))))
        (when (file-exists-p file) (delete-file file))))))

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
                     (preview-key (plist-get private-item :resource-key))
                     (content-key
                      (plist-get private-item :content-resource-key))
                     (preview-file
                      (slackit-media--cached-file preview-key)))
                (should (functionp action))
                (should (functionp (plist-get context :download-action)))
                (should (functionp (plist-get context :save-as-action)))
                (should (file-regular-p preview-file))
                (should-not (equal preview-key content-key))
                (unless (memq system-type '(ms-dos windows-nt cygwin))
                  (should (= #o600
                             (logand #o777 (file-modes preview-file)))))
                (should-not
                 (string-match-p (regexp-quote private-url) context-text))
                (should-not
                 (string-match-p (regexp-quote thumbnail-url) context-text))
                (funcall action)
                (let ((content-file
                       (slackit-media--cached-file content-key)))
                  (should (equal private-url private-source))
                  (should (file-regular-p content-file))
                  (should-not (equal preview-file content-file))
                  (should (equal content-file opened-file))
                  (unless (memq system-type '(ms-dos windows-nt cygwin))
                    (should (= #o600
                               (logand #o777
                                       (file-modes content-file)))))))
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
        (clrhash slackit-media--open-specs)
        (clrhash slackit-media--audio-sessions)
        (when (file-directory-p root) (delete-directory root t))))))

(ert-deftest slackit-contract-video-poster-keeps-preview-file-extension ()
  (slackit-test-with-app (app "video-poster")
    (let* ((root (make-temp-file "slackit-video-poster-" t))
           (slackit-media-cache-directory
            (expand-file-name "media/" root))
           (poster-url
            "https://files.slack.com/files-tmb/T1-FV/poster.jpeg")
           (message
            (slackit-normalize-message
             `((channel . "C1")
               (ts . "1.000002")
               (files
                . (((id . "FV")
                    (name . "movie.mp4")
                    (mimetype . "video/mp4")
                    (thumb_video . ,poster-url)
                    (url_private_download
                     . "https://files.slack.com/files-pri/T1-FV/movie.mp4")))))
             "C1")))
      (unwind-protect
          (progn
            (clrhash slackit-media--fetches)
            (clrhash slackit-media--failures)
            (clrhash slackit-media--image-cache)
            (clrhash slackit-media--open-specs)
            (cl-letf
                (((symbol-function
                   'appkit-media-inline-image-rendering-available-p)
                  (lambda () t))
                 ((symbol-function 'plz)
                  (lambda (_method _url &rest arguments)
                    (let ((file (cadr (plist-get arguments :as))))
                      (with-temp-file file
                        (set-buffer-multibyte nil)
                        (insert "synthetic jpeg"))
                      (funcall (plist-get arguments :then) file))
                    nil))
                 ((symbol-function 'appkit-media-preview-image-from-file)
                  (lambda (_file) 'decoded-video-poster)))
              (slackit-media-ensure-message app message)
              (let* ((item (car (slackit-media--file-items app message)))
                     (key (plist-get item :resource-key))
                     (file (slackit-media--cached-file key)))
                (should (equal "jpeg" (file-name-extension file)))
                (should (eq 'decoded-video-poster
                            (slackit-media--cached-image key))))))
        (clrhash slackit-media--fetches)
        (clrhash slackit-media--failures)
        (clrhash slackit-media--image-cache)
        (clrhash slackit-media--open-specs)
        (when (file-directory-p root) (delete-directory root t))))))

(ert-deftest slackit-contract-media-specs-survive-equal-keys-until-account-stop ()
  (let ((app
         (slackit-runtime-start-account
          "media-spec-owner"
          (list :token "xoxp-CANARY-TOKEN"
                :cookie "xoxd-CANARY-COOKIE")))
        lookup-key)
    (unwind-protect
        (let ((key
               (slackit-media--register-content-spec
                app '(file (id "F-SPEC")) 'audio
                "https://files.slack.com/files-pri/T1-F-SPEC/voice.mp3"
                "voice.mp3" "audio/mpeg" 128 1000)))
          (setq lookup-key (copy-tree key)
                key nil)
          (garbage-collect)
          (should (slackit-media--content-spec-current lookup-key))
          (should
           (appkit-handle-alive-p
            (gethash app slackit-media--spec-handles)))
          (slackit-runtime-stop-account app)
          (should-not (gethash lookup-key slackit-media--open-specs))
          (should-not (gethash app slackit-media--spec-handles)))
      (when (appkit-app-live-p app)
        (slackit-runtime-stop-account app)))))

(ert-deftest slackit-contract-audio-playback-is-appkit-owned ()
  (slackit-test-with-app (app "audio-player")
    (let* ((file (make-temp-file "slackit-audio-" nil ".mp3"))
           (content-key
            (slackit-media--register-content-spec
             app '(file (id "F-AUDIO")) 'audio
             "https://files.slack.com/files-pri/T1-F-AUDIO/voice.mp3"
             "voice.mp3" "audio/mpeg" 128 42000))
           start-arguments
           toggled
           session)
      (unwind-protect
          (cl-letf
              (((symbol-function 'appkit-media-player-available-p)
                (lambda (&rest _arguments) t))
               ((symbol-function 'appkit-media-player-start-file)
                (lambda (path &rest arguments)
                  (should (equal file path))
                  (setq start-arguments arguments
                        session
                        (appkit-media-player-session--create
                         :status 'playing))
                  session))
               ((symbol-function 'appkit-media-player-toggle)
                (lambda (current)
                  (setq toggled current)
                  current)))
            (let ((result
                   (slackit-media--start-audio-file content-key file)))
              (should (eq session result)))
            (should (eq app (plist-get start-arguments :owner)))
            (should (= 42.0
                       (plist-get start-arguments :duration-seconds)))
            (should (functionp
                     (plist-get start-arguments :on-change)))
            (should (eq session
                        (gethash content-key
                                 slackit-media--audio-sessions)))
            (slackit-media--start-audio-file content-key file)
            (should (eq session toggled)))
        (remhash content-key slackit-media--audio-sessions)
        (when (file-exists-p file) (delete-file file))))))

(ert-deftest slackit-contract-media-kinds-download-before-local-dispatch ()
  (slackit-test-with-app (app "media-kinds")
    (let* ((root (make-temp-file "slackit-media-kinds-" t))
           (slackit-media-cache-directory
            (expand-file-name "media/" root))
           (video-url
            "https://files.slack.com/files-pri/T1-F2/movie.mp4")
           (audio-url
            "https://files.slack.com/files-pri/T1-F3/voice.mp3")
           (document-url
            "https://files.slack.com/files-pri/T1-F4/notes.pdf")
           (bad-url
            "https://files.slack.com/files-pri/T1-F5/login.pdf")
           (message
            `((channel . "C1")
              (ts . "2.000001")
              (files
               . (((id . "F2") (name . "movie.mp4")
                   (mimetype . "video/mp4")
                   (url_private_download . ,video-url))
                  ((id . "F3") (name . "voice.mp3")
                   (mimetype . "audio/mpeg")
                   (duration_ms . 42000)
                   (url_private_download . ,audio-url))
                  ((id . "F4") (name . "notes.pdf")
                   (mimetype . "application/pdf")
                   (url_private_download . ,document-url))
                  ((id . "F5") (name . "login.pdf")
                   (mimetype . "application/pdf")
                   (url_private_download . ,bad-url))))))
           fetched
           header-snapshots
           played-video
           played-audio
           opened-document)
      (unwind-protect
          (progn
            (clrhash slackit-media--fetches)
            (clrhash slackit-media--failures)
            (clrhash slackit-media--image-cache)
            (clrhash slackit-media--open-specs)
            (setq message (slackit-normalize-message message "C1"))
            (cl-letf
                (((symbol-function 'plz)
                  (lambda (method url &rest arguments)
                    (should (eq method 'get))
                    (should-not (member "--location" plz-curl-default-args))
                    (push url fetched)
                    (push (plist-get arguments :headers) header-snapshots)
                    (let ((file (cadr (plist-get arguments :as))))
                      (with-temp-file file
                        (set-buffer-multibyte nil)
                        (insert
                         (if (equal url bad-url)
                             "<!doctype html><html><body>login</body></html>"
                           "synthetic local media")))
                      (funcall (plist-get arguments :then) file))
                    nil))
                 ((symbol-function 'appkit-media-play-video-file)
                  (lambda (file label &rest arguments)
                    (should (equal label "slackit"))
                    (should (eq app (plist-get arguments :owner)))
                    (setq played-video file)))
                 ((symbol-function 'slackit-media--start-audio-file)
                  (lambda (_key file) (setq played-audio file)))
                 ((symbol-function 'appkit-media-open-file)
                  (lambda (file) (setq opened-document file))))
              (let ((items (slackit-media--file-items app message)))
                (should (= 4 (length items)))
                (dolist (item items)
                  (let* ((context (slackit-media--item-context item))
                         (context-text (prin1-to-string context)))
                    (should
                     (functionp (plist-get context :open-action)))
                    (dolist (url (list video-url audio-url
                                       document-url bad-url))
                      (should-not
                       (string-match-p
                        (regexp-quote url) context-text)))
                    (funcall (plist-get context :open-action))))
                (should (equal (sort (list video-url audio-url
                                           document-url bad-url)
                                     #'string<)
                               (sort fetched #'string<)))
                (should (equal "mp4" (file-name-extension played-video)))
                (should (equal "mp3" (file-name-extension played-audio)))
                (should
                 (equal "pdf" (file-name-extension opened-document)))
                (should (file-regular-p played-video))
                (should (file-regular-p played-audio))
                (should (file-regular-p opened-document))
                (dolist (item (seq-take items 3))
                  (let* ((content-key
                          (plist-get item :content-resource-key))
                         (fetch-count (length fetched)))
                    (should
                     (eq 'downloaded
                         (plist-get
                          (slackit-media--content-state content-key)
                          :status)))
                    (should (file-regular-p
                             (slackit-media--download-content content-key)))
                    (should (= fetch-count (length fetched)))))
                (let* ((bad-item (nth 3 items))
                       (bad-key
                        (plist-get bad-item :content-resource-key))
                       (bad-state
                        (slackit-media--content-state bad-key)))
                  (should (eq 'error (plist-get bad-state :status)))
                  (should-not (slackit-media--cached-file bad-key)))
                (dolist (headers header-snapshots)
                  (should
                   (equal "Bearer xoxp-CANARY-TOKEN"
                          (cdr (assoc "Authorization" headers))))
                  (should
                   (equal "d=xoxd-CANARY-COOKIE"
                          (cdr (assoc "Cookie" headers))))
                  (should (equal "empty"
                                 (cdr (assoc "Sec-Fetch-Dest" headers))))
                  (should-not (assoc "Origin" headers)))
                (with-temp-buffer
                  (slackit-media-insert-message-cards app message)
                  (should (string-match-p "\\[video\\]" (buffer-string)))
                  (should (string-match-p "\\[audio\\]" (buffer-string)))
                  (should (string-match-p "\\[file\\]" (buffer-string)))
                  (dolist (url (list video-url audio-url
                                     document-url bad-url))
                    (should-not
                     (string-match-p
                      (regexp-quote url) (buffer-string)))))))))
        (clrhash slackit-media--fetches)
        (clrhash slackit-media--failures)
        (clrhash slackit-media--image-cache)
        (clrhash slackit-media--open-specs)
        (clrhash slackit-media--audio-sessions)
        (when (file-directory-p root) (delete-directory root t)))))

(ert-deftest slackit-contract-media-download-cancel-removes-partial-file ()
  (slackit-test-with-app (app "media-cancel")
    (let* ((root (make-temp-file "slackit-media-cancel-" t))
           (slackit-media-cache-directory
            (expand-file-name "media/" root))
           (url "https://files.slack.com/files-pri/T1-F6/archive.zip")
           (message
            (slackit-normalize-message
             `((channel . "C1") (ts . "3.000001")
               (files
                . (((id . "F6") (name . "archive.zip")
                    (mimetype . "application/zip")
                    (url_private_download . ,url)))))
             "C1"))
           partial-file)
      (unwind-protect
          (progn
            (clrhash slackit-media--fetches)
            (clrhash slackit-media--failures)
            (clrhash slackit-media--open-specs)
            (cl-letf
                (((symbol-function 'plz)
                  (lambda (_method _url &rest arguments)
                    (setq partial-file
                          (cadr (plist-get arguments :as)))
                    (with-temp-file partial-file
                      (insert "partial"))
                    nil)))
              (let* ((item (car (slackit-media--file-items app message)))
                     (content-key
                      (plist-get item :content-resource-key))
                     (context (slackit-media--item-context item)))
                (funcall (plist-get context :download-action))
                (should (file-exists-p partial-file))
                (should (gethash content-key slackit-media--fetches))
                (setq context (slackit-media--item-context item))
                (should (functionp (plist-get context :cancel-action)))
                (funcall (plist-get context :cancel-action))
                (should-not (file-exists-p partial-file))
                (should-not
                 (gethash content-key slackit-media--fetches))
                (should
                 (eq 'not-downloaded
                     (plist-get
                      (slackit-media--content-state content-key)
                      :status))))))
        (clrhash slackit-media--fetches)
        (clrhash slackit-media--failures)
        (clrhash slackit-media--open-specs)
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


(ert-deftest slackit-contract-transients-retain-exact-message-scope ()
  (slackit-test-with-app (app "transient")
    (let ((state (slackit-runtime-state app))
          (kill-ring nil)
          media-called)
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
                      (user . "U1") (text . "scoped text")
                      (files
                       . (((id . "F1") (name . "notes.txt")
                           (mimetype . "text/plain")
                           (url_private_download
                            . "https://files.slack.com/files-pri/T1-F1/notes.txt")))))))
                 (response_metadata . ((next_cursor . "")))))))
           ((symbol-function 'appkit-media-card-call-action)
            (lambda (action context)
              (setq media-called (list action context)))))
        (let* ((view (slackit-room-open app "C1" nil))
               scope)
          (with-current-buffer (appkit-view-buffer view)
            (goto-char (point-min))
            (let ((match
                   (text-property-search-forward
                    slackit-message-key-property "1.000001" #'equal)))
              (should match)
              (goto-char (prop-match-beginning match))
              (should (search-forward "notes.txt" nil t)))
            (setq scope (slackit-transient--capture-room-scope t)))
          (with-temp-buffer
            (slackit-transient-actions-copy-text scope))
          (should (equal "scoped text" (current-kill 0 t)))
          (slackit-transient-actions-media-download scope)
          (should (eq 'download (car media-called)))
          (should
           (eq (slackit-transient-scope-media-context scope)
               (cadr media-called)))
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

(ert-deftest slackit-contract-ret-activates-exact-message-semantics ()
  (slackit-test-with-app (app "semantic-ret")
    (let* ((state (slackit-runtime-state app))
           (message
            '((channel . "C1")
              (ts . "1.000001")
              (user . "U1")
              (text . "hello <@U2> in <#C2|random>")
              (reply_count . 3)))
           opened-user opened-room opened-thread)
      (slackit-state-put-user
       state '((id . "U1") (name . "alice")
               (profile . ((display_name . "Alice")))))
      (slackit-state-put-user
       state '((id . "U2") (name . "bob")
               (profile . ((display_name . "Bob")))))
      (slackit-state-put-conversation
       state '((id . "C1") (name . "general") (is_member . t)))
      (slackit-state-put-conversation
       state '((id . "C2") (name . "random") (is_member . t)))
      (with-temp-buffer
        (slackit-room-mode)
        (slackit-room-timeline-mode 1)
        (let ((slackit-show-avatars nil))
          (slackit-render-message-row
           app state message (slackit-room--message-context nil message)))
        (cl-letf (((symbol-function 'slackit-user-open)
                   (lambda (target-app user-id &optional _select)
                     (setq opened-user (list target-app user-id))))
                  ((symbol-function 'slackit-room-open)
                   (lambda (target-app conversation-id &optional _select)
                     (setq opened-room (list target-app conversation-id))))
                  ((symbol-function 'slackit-thread-open)
                   (lambda (target-app conversation-id root-ts
                                       &optional _select)
                     (setq opened-thread
                           (list target-app conversation-id root-ts)))))
          (goto-char (point-min))
          (should (search-forward "Alice" nil t))
          (goto-char (match-beginning 0))
          (should (eq #'appkit-ui-activate
                      (key-binding (kbd "RET"))))
          (appkit-ui-activate-at)
          (should (equal (list app "U1") opened-user))

          (should (search-forward "@Bob" nil t))
          (goto-char (match-beginning 0))
          (appkit-ui-activate-at)
          (should (equal (list app "U2") opened-user))

          (should (search-forward "#random" nil t))
          (goto-char (match-beginning 0))
          (appkit-ui-activate-at)
          (should (equal (list app "C2") opened-room))

          (should (search-forward "[3 replies]" nil t))
          (goto-char (match-beginning 0))
          (appkit-ui-activate-at)
          (should (equal (list app "C1" "1.000001") opened-thread))

          (goto-char (point-min))
          (should (search-forward "hello" nil t))
          (goto-char (match-beginning 0))
          (setq opened-thread nil)
          (should (eq #'slackit-actions-activate
                      (key-binding (kbd "RET"))))
          (slackit-actions-activate)
          (should-not opened-thread)
          (should
           (eq #'slackit-actions-open-thread
               (lookup-key slackit-room-timeline-mode-map (kbd "T")))))))))

(ert-deftest slackit-contract-user-views-own-exact-account-identities ()
  (slackit-test-with-app (app "user-views")
    (let ((state (slackit-runtime-state app))
          callbacks)
      (slackit-state-put-user
       state '((id . "U1") (name . "alice")
               (profile . ((display_name . "Alice")
                           (real_name . "Alice Adams")
                           (title . "Engineer")
                           (pronouns . "she/her")
                           (email . "alice@example.test")
                           (phone . "+1 555 0100")))))
      (slackit-state-put-user
       state '((id . "U2") (name . "bob")
               (profile . ((display_name . "Bob")))))
      (cl-letf (((symbol-function 'slackit-api-user-info)
                 (lambda (_app user-id &rest arguments)
                   (push (cons user-id
                               (plist-get arguments :on-success))
                         callbacks)
                   'synthetic-request)))
        (let ((first (slackit-user-open app "U1" nil))
              (second (slackit-user-open app "U2" nil)))
          (should-not (eq first second))
          (should (equal '(user "U1") (appkit-view-id first)))
          (should (equal '(user "U2") (appkit-view-id second)))
          (should-not
           (equal (buffer-name (appkit-view-buffer first))
                  (buffer-name (appkit-view-buffer second))))
          (with-current-buffer (appkit-view-buffer first)
            (should (equal "U1" slackit-user--user-id))
            (should (string-match-p "Alice Adams" (buffer-string)))
            (should (string-match-p "Engineer" (buffer-string)))
            (should (string-match-p "alice@example.test" (buffer-string))))
          (funcall
           (cdr (assoc "U1" callbacks))
           '((user . ((id . "U1") (name . "alice-new")
                      (profile . ((display_name . "Alice Updated")))))))
          (with-current-buffer (appkit-view-buffer first)
            (appkit-sync-invalidations first)
            (should (string-match-p "Alice Updated" (buffer-string))))
          (funcall
           (cdr (assoc "U2" callbacks))
           '((user . ((id . "WRONG")
                      (profile . ((display_name . "Wrong User")))))))
          (with-current-buffer (appkit-view-buffer second)
            (appkit-sync-invalidations second)
            (should (string-match-p "invalid_response" (buffer-string))))
          (should-not (slackit-state-user state "WRONG")))))))

(ert-deftest slackit-contract-user-profile-callback-dies-with-view-owner ()
  (slackit-test-with-app (app "stale-user")
    (let ((state (slackit-runtime-state app))
          success owner)
      (slackit-state-put-user
       state '((id . "U1") (name . "before")
               (profile . ((display_name . "Before")))))
      (cl-letf (((symbol-function 'slackit-api-user-info)
                 (lambda (_app _user-id &rest arguments)
                   (setq success (plist-get arguments :on-success)
                         owner (plist-get arguments :owner))
                   'synthetic-request)))
        (let ((view (slackit-user-open app "U1" nil)))
          (should (eq view owner))
          (should (slackit-runtime-user-pending-p app "U1"))
          (appkit-kill-view view)
          (should-not (slackit-runtime-user-pending-p app "U1"))
          (funcall
           success
           '((user . ((id . "U1") (name . "after")
                      (profile . ((display_name . "After")))))))
          (should
           (equal "before"
                  (slackit-normalize-get
                   (slackit-state-user state "U1") 'name))))))))

(ert-deftest slackit-contract-user-message-reuses-or-creates-owned-im ()
  (slackit-test-with-app (app "user-dm")
    (let ((state (slackit-runtime-state app))
          opened (api-count 0) dm-success dm-owner)
      (slackit-state-put-user
       state '((id . "U1") (name . "alice")
               (profile . ((display_name . "Alice")))))
      (slackit-state-put-user
       state '((id . "U2") (name . "bob")
               (profile . ((display_name . "Bob")))))
      (slackit-state-put-conversation
       state '((id . "D1") (user . "U1") (is_im . t)
               (is_member . t)))
      (cl-letf (((symbol-function 'slackit-api-user-info)
                 (lambda (&rest _arguments) 'synthetic-request))
                ((symbol-function 'slackit-api-conversations-open)
                 (lambda (_app _user-id &rest arguments)
                   (cl-incf api-count)
                   (setq dm-success (plist-get arguments :on-success)
                         dm-owner (plist-get arguments :owner))
                   'synthetic-request))
                ((symbol-function 'slackit-room-open)
                 (lambda (target-app conversation-id &optional _select)
                   (setq opened (list target-app conversation-id)))))
        (let ((existing-view (slackit-user-open app "U1" nil)))
          (with-current-buffer (appkit-view-buffer existing-view)
            (slackit-user-open-chat))
          (should (equal (list app "D1") opened))
          (should (= 0 api-count)))
        (let ((created-view (slackit-user-open app "U2" nil)))
          (with-current-buffer (appkit-view-buffer created-view)
            (slackit-user-open-chat)
            (should-error (slackit-user-open-chat) :type 'user-error))
          (should (= 1 api-count))
          (should (eq created-view dm-owner))
          (funcall dm-success '((channel . ((id . "D2")))))
          (should (equal (list app "D2") opened))
          (should (equal "D2"
                         (slackit-state-im-conversation-id state "U2")))
          (let ((conversation (slackit-state-conversation state "D2")))
            (should (eq t (slackit-normalize-get conversation 'is_im)))
            (should (equal "U2"
                           (slackit-normalize-get conversation 'user)))))))))

(ert-deftest slackit-contract-stale-user-dm-cannot-publish-or-jump ()
  (slackit-test-with-app (app "stale-user-dm")
    (let ((state (slackit-runtime-state app))
          dm-success opened)
      (slackit-state-put-user
       state '((id . "U1") (name . "alice")
               (profile . ((display_name . "Alice")))))
      (cl-letf (((symbol-function 'slackit-api-user-info)
                 (lambda (&rest _arguments) 'synthetic-request))
                ((symbol-function 'slackit-api-conversations-open)
                 (lambda (_app _user-id &rest arguments)
                   (setq dm-success (plist-get arguments :on-success))
                   'synthetic-request))
                ((symbol-function 'slackit-room-open)
                 (lambda (&rest arguments) (setq opened arguments))))
        (let ((view (slackit-user-open app "U1" nil)))
          (with-current-buffer (appkit-view-buffer view)
            (slackit-user-open-chat))
          (appkit-kill-view view)
          (funcall dm-success '((channel . ((id . "D1")))))
          (should-not opened)
          (should-not (slackit-state-conversation state "D1")))))))

(ert-deftest slackit-contract-user-profile-isolated-across-accounts ()
  (slackit-test-with-app (left "user-left")
    (slackit-test-with-app (right "user-right")
      (slackit-state-put-user
       (slackit-runtime-state left)
       '((id . "U1") (name . "left")
         (profile . ((display_name . "Left Alice")))))
      (slackit-state-put-user
       (slackit-runtime-state right)
       '((id . "U1") (name . "right")
         (profile . ((display_name . "Right Alice")))))
      (cl-letf (((symbol-function 'slackit-api-user-info)
                 (lambda (&rest _arguments) 'synthetic-request)))
        (let ((left-view (slackit-user-open left "U1" nil))
              (right-view (slackit-user-open right "U1" nil)))
          (should-not (eq left-view right-view))
          (should (equal '(user "U1") (appkit-view-id left-view)))
          (should (equal '(user "U1") (appkit-view-id right-view)))
          (should-not
           (equal (buffer-name (appkit-view-buffer left-view))
                  (buffer-name (appkit-view-buffer right-view))))
          (with-current-buffer (appkit-view-buffer left-view)
            (should (string-match-p "Left Alice" (buffer-string)))
            (should-not (string-match-p "Right Alice" (buffer-string))))
          (with-current-buffer (appkit-view-buffer right-view)
            (should (string-match-p "Right Alice" (buffer-string)))
            (should-not (string-match-p "Left Alice" (buffer-string)))))))))

(defun slackit-test--face-includes-p (value face)
  "Return non-nil when face VALUE includes FACE."
  (if (listp value) (memq face value) (eq value face)))

(ert-deftest slackit-contract-code-descriptors-prefer-protocol-language ()
  (let* ((preformatted
          '((type . "rich_text_preformatted")
            (language . "Clojure")
            (border . 1)
            (elements
             . (((type . "text") (text . "(defn f [])"))
                ((type . "link")
                 (text . " docs")
                 (url . "https://example.test"))))))
         (rich-message
          `((blocks
             . (((type . "rich_text")
                 (elements . (,preformatted)))))))
         (markdown-message
          '((blocks
             . (((type . "markdown")
                 (text . "```python\nprint(1)\n```"))))))
         (descriptors
          (append (slackit-code-message-descriptors rich-message)
                  (slackit-code-message-descriptors markdown-message)))
         (rich (nth 0 descriptors))
         (markdown (nth 1 descriptors)))
    (should (= 2 (length descriptors)))
    (should (equal "(defn f []) docs"
                   (slackit-code-descriptor-text rich)))
    (should (equal "clojure"
                   (slackit-code-descriptor-language rich)))
    (should (eq 'rich-text-preformatted
                (slackit-code-descriptor-source-kind rich)))
    (should (= 1 (slackit-code-descriptor-border rich)))
    (should (equal "print(1)" (slackit-code-descriptor-text markdown)))
    (should (equal "python"
                   (slackit-code-descriptor-language markdown)))
    (should
     (eq 'markdown
         (slackit-code-descriptor-source-kind markdown)))))

(ert-deftest slackit-contract-code-descriptor-matching-is-exact-and-ordered ()
  (let* ((first
          (slackit-code-descriptor-create
           :text "(same)" :language "elisp" :source-kind 'rich-text-preformatted))
         (second
          (slackit-code-descriptor-create
           :text "(same)" :language "lisp" :source-kind 'rich-text-preformatted))
         (third
          (slackit-code-descriptor-create
           :text "different" :language "python"
           :source-kind 'rich-text-preformatted))
         (one
          (slackit-code-consume-descriptor
           (list first second third) "(same)"))
         (two
          (slackit-code-consume-descriptor
           (plist-get one :remaining) "(same)"))
         (miss
          (slackit-code-consume-descriptor
           (plist-get two :remaining) "absent")))
    (should (eq first (plist-get one :descriptor)))
    (should (eq second (plist-get two :descriptor)))
    (should-not (plist-get miss :descriptor))
    (should (equal (list third) (plist-get miss :remaining)))))

(ert-deftest slackit-contract-code-fontification-is-hook-free-and-sanitized ()
  (slackit-test-with-app (app "code-font-lock")
    (let ((emacs-lisp-mode-hook-runs 0)
          (emacs-lisp-mode-hook
           (list (lambda () (cl-incf emacs-lisp-mode-hook-runs))))
          first second)
      (setq first
            (slackit-code-block-string
             app "(let ((value 1)) value)" "elisp"))
      (should (= 0 emacs-lisp-mode-hook-runs))
      (should (eq 'block (get-text-property 1 'slackit-code-kind first)))
      (should (eq 'emacs-lisp-mode
                  (get-text-property 1 'slackit-code-mode first)))
      (should (equal "elisp"
                     (get-text-property 1 'slackit-code-language first)))
      (should
       (slackit-test--face-includes-p
        (get-text-property 1 'face first)
        'font-lock-keyword-face))
      (should
       (slackit-test--face-includes-p
        (get-text-property 1 'face first)
        'slackit-code-block))
      (should-not (get-text-property 1 'keymap first))
      (should-not (get-text-property 1 'syntax-table first))
      (put-text-property 0 1 'slackit-test-canary t first)
      (setq second
            (slackit-code-block-string
             app "(let ((value 1)) value)" "elisp"))
      (should-not (get-text-property 0 'slackit-test-canary second))
      (let ((buffer
             (gethash 'emacs-lisp-mode
                      slackit-code--fontification-buffers)))
        (should (buffer-live-p buffer))
        (with-current-buffer buffer
          (should (= 0 (buffer-size)))))
      (let* ((cache (gethash app slackit-code--app-caches))
             (entries (slackit-code-cache-entries cache))
             cache-key)
        (maphash (lambda (key _value) (setq cache-key key)) entries)
        (should (= 1 (hash-table-count entries)))
        (should-not
         (string-match-p "value 1" (prin1-to-string cache-key)))))))

(ert-deftest slackit-contract-unlabelled-and-unknown-code-stay-fixed-pitch ()
  (slackit-test-with-app (app "code-no-detector")
    (let ((detector-called nil)
          unlabelled
          unknown)
      (cl-letf (((symbol-function 'language-detection-string)
                 (lambda (_text)
                   (setq detector-called t)
                   'emacslisp)))
        (setq unlabelled
              (slackit-code-block-string
               app "(let ((value 1)) value)" nil))
        (setq unknown
              (slackit-code-block-string
               app "(let ((value 1)) value)" "unsupported-language")))
      (should-not detector-called)
      (should-not (get-text-property 1 'slackit-code-mode unlabelled))
      (should-not (get-text-property 1 'slackit-code-mode unknown))
      (should (equal "unsupported-language"
                     (get-text-property
                      1 'slackit-code-language unknown)))
      (dolist (payload (list unlabelled unknown))
        (should
         (slackit-test--face-includes-p
          (get-text-property 1 'face payload)
          'slackit-code-block))))))

(ert-deftest slackit-contract-code-rendering-keeps-canonical-wire-text ()
  (slackit-test-with-app (app "code-render")
    (let* ((state (slackit-runtime-state app))
           (wire
            "before `inline` ```(let ((x :wave:)) <@U1>)``` after")
           (preformatted
            '((type . "rich_text_preformatted")
              (language . "elisp")
              (elements
               . (((type . "text")
                   (text . "(let ((x :wave:)) <@U1>)"))))))
           (message
            `((channel . "C1")
              (ts . "1.000001")
              (user . "U1")
              (text . ,wire)
              (blocks
               . (((type . "rich_text")
                   (elements . (,preformatted))))))))
      (with-temp-buffer
        (slackit-render-insert-text app state wire message)
        (let ((rendered (buffer-string)))
          (should (equal
                   "before inline (let ((x :wave:)) <@U1>) after"
                   (substring-no-properties rendered)))
          (goto-char (point-min))
          (should (search-forward "inline" nil t))
          (should (eq 'inline
                      (get-text-property
                       (match-beginning 0) 'slackit-code-kind)))
          (should (search-forward "(let" nil t))
          (should (eq 'block
                      (get-text-property
                       (match-beginning 0) 'slackit-code-kind)))
          (should (eq 'emacs-lisp-mode
                      (get-text-property
                       (match-beginning 0) 'slackit-code-mode)))
          (should (search-forward ":wave:" nil t))
          (should (search-forward "<@U1>" nil t))))
      (should (equal wire (slackit-normalize-get message 'text))))))

(ert-deftest slackit-contract-code-language-requires-exact-block-match ()
  (slackit-test-with-app (app "code-exact")
    (let* ((state (slackit-runtime-state app))
           (wire "```(defun actual ())```")
           (preformatted
            '((type . "rich_text_preformatted")
              (language . "elisp")
              (elements
               . (((type . "text")
                   (text . "(defun different ())"))))))
           (message
            `((text . ,wire)
              (blocks
               . (((type . "rich_text")
                   (elements . (,preformatted))))))))
      (with-temp-buffer
        (slackit-render-insert-text app state wire message)
        (should-not
         (get-text-property (point-min) 'slackit-code-language))
        (should-not
         (get-text-property (point-min) 'slackit-code-mode))))))

(ert-deftest slackit-contract-code-caches-are-account-lifecycle-owned ()
  (slackit-test-with-app (left "code-left")
    (slackit-test-with-app (right "code-right")
      (slackit-code-block-string left "(let ((x 1)) x)" "elisp")
      (slackit-code-block-string right "(let ((x 1)) x)" "elisp")
      (let ((left-cache (gethash left slackit-code--app-caches))
            (right-cache (gethash right slackit-code--app-caches)))
        (should (slackit-code-cache-p left-cache))
        (should (slackit-code-cache-p right-cache))
        (should-not (eq left-cache right-cache))
        (slackit-runtime-stop-account left)
        (should-not (gethash left slackit-code--app-caches))
        (should (eq right-cache
                    (gethash right slackit-code--app-caches)))))))
(ert-deftest slackit-contract-code-mode-resolution-is-emacs-owned ()
  (should (eq 'emacs-lisp-mode
              (slackit-code-mode-for-language "elisp")))
  (let ((major-mode-remap-alist
         '((python-mode . emacs-lisp-mode))))
    (should (eq 'emacs-lisp-mode
                (slackit-code-mode-for-language "python"))))
  (should-not
   (slackit-code-mode-for-language "unsupported-language")))


(provide 'slackit-contract-test)

;;; slackit-contract-test.el ends here
