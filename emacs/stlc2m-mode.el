;;; stlc2m-mode.el --- Minimal dialog client for stlc2m --server  -*- lexical-binding: t; -*-

(require 'json)
(require 'cl-lib)
(require 'flycheck)

(defgroup stlc2m nil
  "Dialog client for the stlc2m compiler server."
  :group 'tools)

(defcustom stlc2m-server-command (list (expand-file-name "../_build/default/bin/main.exe") "--server")
  "Command to start the stlc2m server."
  :type '(repeat string))

(defvar stlc2m--proc nil)
(defvar stlc2m--next-id 1)
(defvar stlc2m--pending (make-hash-table :test 'eql))
(defvar-local stlc2m--overlays nil)

(defun stlc2m--ensure-server ()
  "Ensure the stlc2m server process is running and return it."
  (when (or (null stlc2m--proc) (not (process-live-p stlc2m--proc)))
    (let* ((buf (get-buffer-create "*stlc2m-server*"))
           (proc (make-process
                  :name "stlc2m-server"
                  :buffer buf
                  :command stlc2m-server-command
                  :connection-type 'pipe
                  :noquery t
                  :filter #'stlc2m--process-filter
                  :sentinel #'stlc2m--process-sentinel)))
      (setq stlc2m--proc proc)
      ;; We accumulate partial lines per process:
      (process-put proc :stlc2m-partial "")
      (message "stlc2m server started")))
  stlc2m--proc)

(defun stlc2m--process-sentinel (proc event)
  (message "stlc2m server: %s" (string-trim event))
  ;; clear pending callbacks on exit to avoid leaks
  (unless (process-live-p proc)
    (clrhash stlc2m--pending)))

(defun stlc2m--process-filter (proc chunk)
  "Accumulate CHUNK into complete lines and handle JSON responses."
  (let* ((partial (or (process-get proc :stlc2m-partial) ""))
         (data (concat partial chunk))
         (lines (split-string data "\n"))
         (complete (butlast lines))
         (rest (car (last lines))))
    (process-put proc :stlc2m-partial rest)
    (dolist (line complete)
      (unless (string-empty-p (string-trim line))
        (stlc2m--handle-response-line line)))))

(defun stlc2m--handle-response-line (line)
  (condition-case err
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (json-key-type 'symbol)
             ;; Use built-in fast parser when available
             (obj (if (fboundp 'json-parse-string)
                      (json-parse-string line :object-type 'alist :array-type 'list)
                    (json-read-from-string line)))
             (id (alist-get 'id obj))
             (cb (and (integerp id) (gethash id stlc2m--pending))))
        (when cb
          (remhash id stlc2m--pending)
          (funcall cb obj)))
    (error
     (message "stlc2m: failed to parse response: %S (line=%s)" err line))))

(defun stlc2m--diag->flycheck-error (diag)
  "Convert one server DIAG (alist) to a `flycheck-error`."
  (let* ((code (alist-get 'code diag))
	 (msg (alist-get 'message diag))
	 (sev (alist-get 'severity diag))
	 (range (alist-get 'range diag))
	 (start (alist-get 'start range))
	 ;; Server: line is 1-based, col is 0-based
	 (line (alist-get 'line start))
	 (col0 (alist-get 'col start))
	 ;; Flycheck expects 1-based columns (or nil)
	 (col (and (numberp col0) (1+ col0)))
	 (level (pcase sev
		  ("error" 'error)
		  ("warning" 'warning)
		  (_ 'info))))
    (flycheck-error-new-at
     line col level
     (format "[%s] %s" code msg)
     :checker 'stlc2m)))

(defun stlc2m--push-response-to-flycheck (resp)
  "Push server RESP (alist) into Flycheck for the current buffer."
  (let ((ok (alist-get 'ok resp)))
    (cond
     ((not ok)
      (let* ((err (alist-get 'error resp))
	     (ecode (alist-get 'code err))
	     (emsg (alist-get 'message err))
	     (fe (flycheck-error-new-at
		  1 1 'error
		  (format "[%s] %s" ecode emsg)
		  :checker 'stlc2m)))
	;; Replace Flycheck errors in this buffer
	(flycheck-report-current-errors (list fe))))
     (t
      (let* ((diags (alist-get 'diagnostics resp))
	     (errs (mapcar #'stlc2m--diag->flycheck-error diags)))
	(flycheck-report-current-errors errs))))))

(defun stlc2m--buffer-uri ()
  "Return a file:// URI for current buffer, or a synthetic one."
  (if buffer-file-name
      (concat "file://" (expand-file-name buffer-file-name))
    (format "buffer://%s" (buffer-name))))

(defun stlc2m--send-check (buffer)
  "Send a check request for BUFFER. Updates overlays when response arrives."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((proc (stlc2m--ensure-server))
             (id stlc2m--next-id)
             (uri (stlc2m--buffer-uri))
             (text (buffer-substring-no-properties (point-min) (point-max)))
             (req `((id . ,id)
                    (method . "check")
                    (uri . ,uri)
                    (text . ,text)))
             (json-str (if (fboundp 'json-serialize)
                           (json-serialize req)
                         (json-encode req))))
        (setq stlc2m--next-id (1+ stlc2m--next-id))
        (puthash id
                 (lambda (resp)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (stlc2m--push-response-to-flycheck resp))))
                 stlc2m--pending)
        (process-send-string proc (concat json-str "\n"))))))

(defun stlc2m--clear-overlays ()
  (when stlc2m--overlays
    (mapc #'delete-overlay stlc2m--overlays)
    (setq stlc2m--overlays nil)))

(defun stlc2m--pos-to-point (pos)
  "Convert server position POS (alist with 'line and 'col) to buffer point.
Assumes line is 1-based; col is 0-based."
  (let* ((line (alist-get 'line pos))
         (col (alist-get 'col pos)))
    (save-excursion
      (goto-char (point-min))
      (forward-line (max 0 (1- (or line 1))))
      (forward-char (max 0 (or col 0)))
      (point))))

(defun stlc2m-jump-to-related ()
  "Jump to the first related location of the diagnostic overlay at point."
  (interactive)
  (let* ((ovs (overlays-at (point)))
         (ov (cl-find-if (lambda (o) (overlay-get o 'stlc2m)) ovs)))
    (unless ov
      (user-error "No stlc2m diagnostic at point"))
    (let* ((related (overlay-get ov 'stlc2m-related)))
      (unless (and related (listp related))
        (user-error "No related locations for this diagnostic"))
      (let* ((r0 (car related))
             (rrange (alist-get 'range r0))
             (start (stlc2m--pos-to-point (alist-get 'start rrange))))
        (goto-char start)
	(message "%s" (alist-get 'message r0))))))

(defun stlc2m--apply-diagnostics (resp)
  "Render diagnostics from RESP into overlays in current buffer."
  (stlc2m--clear-overlays)
  (let* ((ok (alist-get 'ok resp))
         (diags (alist-get 'diagnostics resp)))
    (cond
     ((not ok)
      ;; Protocol error, show in minibuffer and keep buffer clean.
      (let* ((err (alist-get 'error resp))
             (code (alist-get 'code err))
             (msg (alist-get 'message err)))
        (message "stlc2m protocol error [%s]: %s" code msg)))
     ((null diags) nil)
     (t
      (dolist (d diags)
        (let* ((code (alist-get 'code d))
               (msg (alist-get 'message d))
               (sev (alist-get 'severity d))
               (range (alist-get 'range d))
               (start (stlc2m--pos-to-point (alist-get 'start range)))
               (end (stlc2m--pos-to-point (alist-get 'end range)))
               (ov (make-overlay start (max start end))))
          (overlay-put ov 'stlc2m t)
	  (overlay-put ov 'stlc2m-diag d)
	  (overlay-put ov 'stlc2m-related (alist-get 'related d))
          (overlay-put ov 'help-echo (format "[%s/%s] %s" code sev msg))
          ;; underline + face to make it visible
          (overlay-put ov 'face '(:underline (:style wave)))
          (push ov stlc2m--overlays)))
      ;; Optional: summary in minibuffer
      (message "stlc2m: %d diagnostic(s)" (length diags))))))

;;;###autoload
(define-minor-mode stlc2m-mode
  "Minor mode to check STLC2M buffers via stlc2m --server."
  :lighter " stlc2m"
  (if stlc2m-mode
      (progn
        (add-hook 'after-save-hook #'stlc2m-check-buffer nil t)
        ;; Optional: check on enable
        (stlc2m-check-buffer))
    (remove-hook 'after-save-hook #'stlc2m-check-buffer t)
    (stlc2m--clear-overlays)))

(defun stlc2m-check-buffer ()
  "Check current buffer with stlc2m server."
  (interactive)
  (stlc2m--send-check (current-buffer)))

(provide 'stlc2m-mode)
;;; stlc2m-mode.el ends here
