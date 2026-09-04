;;; slackit-upload.el --- Slack external file upload transport -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Slackit contributors

;;; Commentary:

;; Stream one local file to a validated Slack upload capability without loading
;; its bytes into Emacs.  The capability receives no account credential.  One
;; Appkit owner controls process cancellation and progress publication.

;;; Code:

(require 'cl-lib)
(require 'plz)
(require 'subr-x)
(require 'url-parse)
(require 'appkit-core)
(require 'slackit-runtime)

(defconst slackit-upload--curl-args
  '("--disable" "--max-redirs" "0" "--retry" "0"
    "--show-error" "--progress-meter" "--suppress-connect-headers"
    "--config" "-")
  "Fixed curl arguments for a non-redirecting, non-retrying upload.")

(cl-defstruct (slackit-upload-transfer
               (:constructor slackit-upload-transfer-create))
  app
  owner
  generation
  process
  stderr-process
  response-buffer
  handle
  status
  progress
  settled-p
  on-success
  on-error
  on-progress)

(defun slackit-upload-url-p (value)
  "Return non-nil when VALUE is an exact Slack upload capability URL."
  (when (stringp value)
    (condition-case nil
        (let ((url (url-generic-parse-url value)))
          (and (equal "https" (url-type url))
               (equal "files.slack.com" (downcase (or (url-host url) "")))
               (or (null (url-port url)) (= 443 (url-port url)))
               (null (url-user url))
               (string-prefix-p "/upload/v1/" (or (url-filename url) ""))
               (null (url-target url))))
      (error nil))))

(defun slackit-upload--config-value (value)
  "Return VALUE escaped for one quoted curl config field."
  (unless (and (stringp value)
               (not (string-match-p "[\0\r\n]" value)))
    (error "slackit: upload configuration contains unsupported characters"))
  (setq value (string-replace "\\" "\\\\" value))
  (string-replace "\"" "\\\"" value))

(defun slackit-upload--config-line (name value)
  "Return curl config line NAME carrying quoted VALUE."
  (format "%s = \"%s\"\n" name (slackit-upload--config-value value)))

(defun slackit-upload--form-file (file)
  "Return curl multipart form value for readable local FILE."
  (let ((path (expand-file-name file)))
    (when (file-remote-p path)
      (user-error "slackit: remote attachment paths are unsupported"))
    (unless (and (file-regular-p path) (file-readable-p path))
      (user-error "slackit: attachment is not a readable regular file"))
    (when (string-match-p "[\0\r\n]" path)
      (user-error "slackit: attachment path contains unsupported characters"))
    (format "filename=@\"%s\""
            (slackit-upload--config-value path))))

(defun slackit-upload--curl-config (url file)
  "Return private stdin configuration uploading FILE to capability URL."
  (concat
   (slackit-upload--config-line "url" url)
   (slackit-upload--config-line "request" "POST")
   (slackit-upload--config-line "header" "Accept: application/json")
   (slackit-upload--config-line "header" "Expect:")
   (slackit-upload--config-line "form" (slackit-upload--form-file file))
   (slackit-upload--config-line "output" null-device)
   (slackit-upload--config-line "write-out" "%{http_code}")))

(defun slackit-upload--current-p (transfer)
  "Return non-nil when TRANSFER may still publish callbacks."
  (and (slackit-upload-transfer-p transfer)
       (not (slackit-upload-transfer-settled-p transfer))
       (slackit-runtime-current-p
        (slackit-upload-transfer-app transfer)
        (slackit-upload-transfer-generation transfer))
       (let ((owner (slackit-upload-transfer-owner transfer)))
         (or (appkit-app-live-p owner) (appkit-surface-live-p owner)))
       (let ((handle (slackit-upload-transfer-handle transfer)))
         (or (null handle)
             (and (appkit-handle-p handle)
                  (appkit-handle-alive-p handle))))))

(defun slackit-upload--cleanup (transfer &optional stop-process)
  "Release TRANSFER resources, stopping its process when STOP-PROCESS."
  (when-let* ((process (slackit-upload-transfer-process transfer)))
    (when (and stop-process (process-live-p process))
      (delete-process process)))
  (when-let* ((stderr-process
               (slackit-upload-transfer-stderr-process transfer)))
    (when (process-live-p stderr-process)
      (delete-process stderr-process)))
  (when-let* ((buffer (slackit-upload-transfer-response-buffer transfer)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))
  (setf (slackit-upload-transfer-process transfer) nil
        (slackit-upload-transfer-stderr-process transfer) nil
        (slackit-upload-transfer-response-buffer transfer) nil))

(defun slackit-upload--settle (transfer status &optional error-data)
  "Settle TRANSFER once with STATUS and optional redacted ERROR-DATA."
  (unless (slackit-upload-transfer-settled-p transfer)
    (setf (slackit-upload-transfer-settled-p transfer) t
          (slackit-upload-transfer-status transfer) status)
    (when (eq status 'finished)
      (setf (slackit-upload-transfer-progress transfer) 1.0))
    (slackit-upload--cleanup transfer nil)
    (when-let* ((handle (slackit-upload-transfer-handle transfer)))
      (appkit-retire-handle handle))
    (pcase status
      ('finished
       (when-let* ((callback (slackit-upload-transfer-on-success transfer)))
         (funcall callback transfer)))
      ((or 'failed)
       (when-let* ((callback (slackit-upload-transfer-on-error transfer)))
         (funcall callback
                  transfer
                  (or error-data '(:status 0 :code "upload_failed")))))))
  transfer)

(defun slackit-upload--cancel-owned (transfer)
  "Cancel TRANSFER from its Appkit owner without publishing failure."
  (unless (slackit-upload-transfer-settled-p transfer)
    (setf (slackit-upload-transfer-settled-p transfer) t
          (slackit-upload-transfer-status transfer) 'canceled)
    (slackit-upload--cleanup transfer t))
  transfer)

(defun slackit-upload-transfer-cancel (transfer)
  "Cancel live Slack upload TRANSFER and return non-nil when canceled."
  (when (and (slackit-upload-transfer-p transfer)
             (not (slackit-upload-transfer-settled-p transfer)))
    (if-let* ((handle (slackit-upload-transfer-handle transfer)))
        (appkit-cancel-handle handle)
      (slackit-upload--cancel-owned transfer))
    t))

(defun slackit-upload--curl-upload-ratio (line)
  "Return curl progress LINE's upload percentage as a 0-1 float."
  (when (string-match
         (concat "\\`[ \t]*[0-9]+[ \t]+[^ \t]+[ \t]+[0-9]+[ \t]+"
                 "[^ \t]+[ \t]+\\([0-9]+\\)\\>")
         line)
    (min 1.0 (/ (float (string-to-number (match-string 1 line))) 100.0))))

(defun slackit-upload--stderr-filter (process output)
  "Parse upload progress from curl PROCESS OUTPUT and discard diagnostics."
  (when-let* ((transfer (process-get process 'slackit-upload-transfer))
              ((slackit-upload--current-p transfer)))
    (let* ((pending
            (concat (or (process-get process 'slackit-upload-pending) "")
                    output))
           (terminated (string-match-p "[\r\n]\\'" pending))
           (parts (split-string pending "[\r\n]" t))
           (tail (if (or terminated (null parts)) "" (car (last parts))))
           (lines (if (or terminated (null parts)) parts (butlast parts)))
           progress)
      (process-put process 'slackit-upload-pending tail)
      (dolist (line lines)
        (when-let* ((value (slackit-upload--curl-upload-ratio line)))
          (setq progress value)))
      (when (and progress
                 (> progress (or (slackit-upload-transfer-progress transfer) 0)))
        (setf (slackit-upload-transfer-progress transfer) progress)
        (when-let* ((callback
                     (slackit-upload-transfer-on-progress transfer)))
          (funcall callback transfer progress))))))

(defun slackit-upload--response-status (transfer)
  "Return TRANSFER's three-digit HTTP response status, or nil."
  (when-let* ((buffer (slackit-upload-transfer-response-buffer transfer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (let ((value (string-trim
                    (buffer-substring-no-properties (point-min) (point-max)))))
        (and (string-match-p "\\`[0-9]\\{3\\}\\'" value)
             (string-to-number value))))))

(defun slackit-upload--sentinel (process _event)
  "Settle the Slack upload transfer owned by curl PROCESS."
  (when (memq (process-status process) '(exit signal))
    (when-let* ((transfer (process-get process 'slackit-upload-transfer))
                ((not (slackit-upload-transfer-settled-p transfer))))
      (let ((status (slackit-upload--response-status transfer)))
        (if (and (eq (process-status process) 'exit)
                 (zerop (process-exit-status process))
                 (= (or status 0) 200))
            (slackit-upload--settle transfer 'finished)
          (slackit-upload--settle
           transfer 'failed
           (list :status (or status 0) :code "upload_failed")))))))

(cl-defun slackit-upload-file
    (app owner upload-url file &key on-success on-error on-progress)
  "Stream FILE to Slack UPLOAD-URL under exact Appkit OWNER.

The validated capability receives no APP bearer token or cookie.  ON-SUCCESS
receives the transfer.  ON-ERROR receives the transfer and redacted error data.
ON-PROGRESS receives the transfer and a 0-1 float."
  (unless (slackit-upload-url-p upload-url)
    (error "slackit: rejected non-Slack upload capability"))
  (let* ((program
          (or (executable-find plz-curl-program)
              (error "slackit: curl executable is unavailable")))
         (config (slackit-upload--curl-config upload-url file))
         (response-buffer (generate-new-buffer " *slackit-upload-response*"))
         (transfer
          (slackit-upload-transfer-create
           :app app :owner owner
           :generation (slackit-runtime-generation app)
           :response-buffer response-buffer
           :status 'uploading :progress 0.0
           :on-success on-success :on-error on-error :on-progress on-progress))
         stderr-process process)
    (with-current-buffer response-buffer
      (set-buffer-multibyte nil))
    (condition-case error-data
        (progn
          (setq stderr-process
                (make-pipe-process
                 :name "slackit-upload-stderr" :buffer nil :coding 'binary
                 :noquery t :filter #'slackit-upload--stderr-filter))
          (setq process
                (make-process
                 :name "slackit-upload" :buffer response-buffer
                 :stderr stderr-process
                 :command (cons program slackit-upload--curl-args)
                 :coding 'binary :connection-type 'pipe :noquery t
                 :sentinel #'slackit-upload--sentinel))
          (unless (processp process)
            (error "slackit: curl did not return an upload process"))
          (setf (slackit-upload-transfer-process transfer) process
                (slackit-upload-transfer-stderr-process transfer) stderr-process
                (slackit-upload-transfer-handle transfer)
                (appkit-register-handle
                 owner 'slackit-upload transfer #'slackit-upload--cancel-owned))
          (process-put process 'slackit-upload-transfer transfer)
          (process-put stderr-process 'slackit-upload-transfer transfer)
          (process-send-string process config)
          (process-send-eof process)
          transfer)
      ((error quit)
       (slackit-upload--cleanup transfer t)
       (setf (slackit-upload-transfer-settled-p transfer) t
             (slackit-upload-transfer-status transfer) 'failed)
       (when on-error
         (funcall on-error transfer '(:status 0 :code "transport_error")))
       (when (eq (car error-data) 'quit)
         (signal 'quit nil))
       transfer))))

(provide 'slackit-upload)

;;; slackit-upload.el ends here
