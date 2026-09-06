;;; slackit-http.el --- Origin-bound Slack Web API transport -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Asynchronous plz transport restricted to Slack's fixed Web API origin.
;; Redirect following is disabled so credentials cannot cross an origin.

;;; Code:

(require 'cl-lib)
(require 'plz)
(require 'subr-x)
(require 'url-util)
(require 'appkit-core)
(require 'slackit-customize)
(require 'slackit-decode)
(require 'slackit-runtime)

(defconst slackit-http-api-origin "https://slack.com/api/"
  "Only origin and path prefix that receives Slack account credentials.")

(cl-defstruct (slackit-http-request
               (:constructor slackit-http-request-create))
  app
  owner
  generation
  endpoint
  method
  parameters
  idempotent-p
  attempt
  process
  timer
  handle
  on-success
  on-error
  active-p)

(defun slackit-http--endpoint-url (endpoint)
  "Return fixed Web API URL for validated ENDPOINT."
  (unless (and (stringp endpoint)
               (string-match-p "\\`[[:alnum:]_.]+\\'" endpoint))
    (error "slackit: invalid Web API endpoint"))
  (concat slackit-http-api-origin endpoint))

(defun slackit-http--form-value (value)
  "Return Slack form representation of VALUE."
  (cond
   ((eq value t) "true")
   ((null value) nil)
   ((listp value) (mapconcat (lambda (item) (format "%s" item)) value ","))
   (t (format "%s" value))))

(defun slackit-http--form-parameters (parameters)
  "Return curl-compatible form list for PARAMETERS alist."
  (delq nil
        (mapcar (lambda (entry)
                  (when-let* ((value (slackit-http--form-value (cdr entry))))
                    (list (format "%s" (car entry)) value)))
                parameters)))

(defun slackit-http--url-and-body (endpoint method parameters)
  "Build URL and request body for ENDPOINT METHOD PARAMETERS."
  (let* ((url (slackit-http--endpoint-url endpoint))
         (query (slackit-http--form-parameters parameters))
         (encoded (and query (url-build-query-string query))))
    (if (eq method 'get)
        (cons (concat url (if encoded (concat "?" encoded) "")) nil)
      (cons url encoded))))

(defun slackit-http--cookie-header (cookie)
  "Return account-local Cookie header value for COOKIE."
  (when (and (stringp cookie) (not (string-empty-p cookie)))
    (if (string-match-p "\\`d=" cookie)
        cookie
      (concat "d=" cookie))))

(defun slackit-http--headers (app method)
  "Return credential headers for fixed-origin APP request METHOD."
  (let* ((transport (slackit-runtime-transport app))
         (credential (slackit-transport-credential transport))
         (token (and credential (slackit-credential-token credential)))
         (cookie (and credential (slackit-credential-cookie credential))))
    (unless (and (stringp token) (not (string-empty-p token)))
      (error "slackit: account credential is unavailable"))
    (append
     `(("Accept" . "application/json")
       ("Authorization" . ,(concat "Bearer " token)))
     (unless (eq method 'get)
       '(("Content-Type" . "application/x-www-form-urlencoded")))
     (when-let* ((value (slackit-http--cookie-header cookie)))
       `(("Cookie" . ,value))))))

(defun slackit-http--request-current-p (request)
  "Return non-nil when REQUEST still owns publication."
  (and (slackit-http-request-active-p request)
       (slackit-runtime-current-p
        (slackit-http-request-app request)
        (slackit-http-request-generation request))
       (appkit-owner-live-p (slackit-http-request-owner request))
       (let ((handle (slackit-http-request-handle request)))
         (and (appkit-handle-p handle) (appkit-handle-alive-p handle)))))

(defun slackit-http--cancel-request (request)
  "Cancel process/timer resources held by REQUEST."
  (setf (slackit-http-request-active-p request) nil)
  (when-let* ((timer (slackit-http-request-timer request)))
    (when (timerp timer) (cancel-timer timer)))
  (when-let* ((process (slackit-http-request-process request)))
    (when (and (processp process) (process-live-p process))
      (delete-process process)))
  (setf (slackit-http-request-timer request) nil
        (slackit-http-request-process request) nil))

(defun slackit-http--retire-request (request)
  "Retire REQUEST without cancellation side effects."
  (when (slackit-http-request-active-p request)
    (setf (slackit-http-request-active-p request) nil
          (slackit-http-request-timer request) nil
          (slackit-http-request-process request) nil)
    (when-let* ((handle (slackit-http-request-handle request)))
      (appkit-retire-handle handle))))

(defun slackit-http--decode-body (text)
  "Decode Slack response TEXT, returning nil for invalid JSON."
  (condition-case nil
      (and (stringp text) (not (string-empty-p text))
           (slackit-decode-json text))
    (error nil)))

(defun slackit-http--response-header (response name)
  "Return RESPONSE header NAME case-insensitively."
  (let ((target (downcase name)))
    (cl-loop for (key . value) in (plz-response-headers response)
             when (equal target (downcase (format "%s" key)))
             return value)))

(defun slackit-http--retry-after (response)
  "Return bounded numeric Retry-After from RESPONSE, or nil."
  (when-let* ((value (slackit-http--response-header response "retry-after"))
              (number (string-to-number (format "%s" value))))
    (and (> number 0) (min number 3600))))

(defun slackit-http--error-response (error-object)
  "Return plz response embedded in ERROR-OBJECT, or nil."
  (when (and (fboundp 'plz-error-response) error-object)
    (ignore-errors (plz-error-response error-object))))

(defun slackit-http--emit-error (request status code)
  "Settle REQUEST with redacted STATUS and Slack CODE."
  (when (slackit-http--request-current-p request)
    (let ((callback (slackit-http-request-on-error request))
          (payload (list :status status
                         :code (or code "request_failed"))))
      (slackit-http--retire-request request)
      (when callback (funcall callback payload)))))

(defun slackit-http--schedule-retry (request delay)
  "Schedule REQUEST retry after DELAY seconds under its existing owner."
  (when (slackit-http--request-current-p request)
    (let ((timer
           (run-at-time
            delay nil
            (lambda ()
              (if (slackit-http--request-current-p request)
                  (progn
                    (setf (slackit-http-request-timer request) nil)
                    (slackit-http--dispatch request))
                (slackit-http--retire-request request))))))
      (setf (slackit-http-request-timer request) timer))))

(defun slackit-http--handle-success (request response)
  "Handle successful plz RESPONSE for REQUEST."
  (if (not (slackit-http--request-current-p request))
      (slackit-http--retire-request request)
    (let ((body (slackit-http--decode-body (plz-response-body response)))
          (callback (slackit-http-request-on-success request)))
      (slackit-http--retire-request request)
      (when callback (funcall callback body)))))

(defun slackit-http--handle-failure (request error-object)
  "Handle plz ERROR-OBJECT for REQUEST without exposing response bodies."
  (if (not (slackit-http--request-current-p request))
      (slackit-http--retire-request request)
    (let* ((response (slackit-http--error-response error-object))
           (status (or (and response (plz-response-status response)) 0))
           (body (and response
                      (slackit-http--decode-body
                       (plz-response-body response))))
           (code (and body (alist-get 'error body)))
           (retry-after (and response (slackit-http--retry-after response))))
      (if (and (= status 429)
               retry-after
               (slackit-http-request-idempotent-p request)
               (eq (slackit-http-request-method request) 'get)
               (< (slackit-http-request-attempt request)
                  slackit-read-retry-limit))
          (progn
            (cl-incf (slackit-http-request-attempt request))
            (slackit-http--schedule-retry request retry-after))
        (slackit-http--emit-error request status code)))))

(defun slackit-http--dispatch (request)
  "Dispatch one fixed-origin REQUEST attempt."
  (when (slackit-http--request-current-p request)
    (pcase-let* ((`(,url . ,body)
                  (slackit-http--url-and-body
                   (slackit-http-request-endpoint request)
                   (slackit-http-request-method request)
                   (slackit-http-request-parameters request)))
                 (app (slackit-http-request-app request))
                 ;; `--location' would forward custom headers through redirects.
                 ;; Slackit rejects redirects by removing it for every request.
                 (plz-curl-default-args
                  (remove "--location" plz-curl-default-args))
                 (process
                  (plz (slackit-http-request-method request) url
                    :headers (slackit-http--headers
                              app (slackit-http-request-method request))
                    :body body
                    :as 'response
                    :timeout slackit-http-timeout
                    :connect-timeout slackit-http-timeout
                    :then (lambda (response)
                            (slackit-http--handle-success request response))
                    :else (lambda (error-object)
                            (slackit-http--handle-failure
                             request error-object)))))
      (if (slackit-http--request-current-p request)
          (setf (slackit-http-request-process request) process)
        (when (and (processp process) (process-live-p process))
          (delete-process process))))))

(cl-defun slackit-http-request
    (app endpoint &key (method 'get) parameters idempotent-p owner
         on-success on-error)
  "Issue an account-owned fixed-origin Slack Web API request.

ENDPOINT is a bare Web API method name.  OWNER defaults to APP.  Only
GET requests explicitly marked IDEMPOTENT-P may retry a 429 response."
  (unless (memq method '(get post))
    (error "slackit: unsupported HTTP method"))
  (let* ((effective-owner (or owner app))
         (request
           (slackit-http-request-create
            :app app
            :owner effective-owner
            :generation (slackit-runtime-generation app)
            :endpoint endpoint
            :method method
            :parameters parameters
            :idempotent-p (and idempotent-p t)
            :attempt 0
            :on-success on-success
            :on-error on-error
            :active-p t))
         (handle (appkit-register-handle
                  effective-owner 'slackit-http request
                  #'slackit-http--cancel-request)))
    (setf (slackit-http-request-handle request) handle)
    (condition-case error-data
        (slackit-http--dispatch request)
      (error
       (slackit-http--emit-error request 0 "transport_error")
       (unless (slackit-http-request-on-error request)
         (signal (car error-data) (cdr error-data)))))
    request))

(defun slackit-http-cancel (request)
  "Cancel live Slack HTTP REQUEST without publishing a transport outcome."
  (when (and (slackit-http-request-p request)
             (slackit-http-request-active-p request))
    (if-let* ((handle (slackit-http-request-handle request)))
        (appkit-cancel-handle handle)
      (slackit-http--cancel-request request))
    t))

(provide 'slackit-http)

;;; slackit-http.el ends here
