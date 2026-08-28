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
    (should (slackit-rtm-valid-url-p
             "wss://wss-primary.slack.com/?token=capability"))
    (should-not (slackit-rtm-valid-url-p
                 "wss://slack.com.evil.invalid/?token=capability"))
    (should-not (slackit-rtm-valid-url-p
                 "https://wss-primary.slack.com/"))))

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
                          (members . (((id . "U2"))))
                          (response_metadata . ((next_cursor . ""))))
                      '((ok . t)
                        (members . (((id . "U1"))))
                        (response_metadata . ((next_cursor . "next"))))))))))
      (slackit-api-users-list-all
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

(provide 'slackit-contract-test)

;;; slackit-contract-test.el ends here
