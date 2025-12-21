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

(defvar-local stlc2m--flycheck-last-id 0)
;; For "Problems" buffer
(defvar stlc2m--problems-buffer-name "*stlc2m-problems*")
(defvar-local stlc2m--last-response nil)
(defvar-local stlc2m--last-uri nil)

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

(defun stlc2m--flycheck-start (_checker callback)
  "Flycheck start function. Sends buffer to stlc2m server and calls CALLBACK."
  (let* ((buf (current-buffer))
         (proc (stlc2m--ensure-server)))
    (with-current-buffer buf
      (let* ((id stlc2m--next-id)
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
        (setq stlc2m--flycheck-last-id id)

        (puthash id
                 (lambda (resp)
                   (when (buffer-live-p buf)
                     (with-current-buffer buf
		       ;; Store for "Problems" buffer
		       (setq stlc2m--last-response resp)
		       (setq stlc2m--last-uri (stlc2m--buffer-uri))

                       ;; Ignore stale responses from older requests.
                       (when (= id stlc2m--flycheck-last-id)
                         (let ((ok (alist-get 'ok resp)))
                           (cond
                            ((not ok)
                             (let* ((err (alist-get 'error resp))
                                    (ecode (alist-get 'code err))
                                    (emsg (alist-get 'message err))
                                    (fe (flycheck-error-new-at
                                         1 1 'error
                                         (format "[%s] %s" ecode emsg)
                                         :checker 'stlc2m
                                         :data resp)))
                               (funcall callback 'finished (list fe))))
                            (t
                             (let* ((diags (alist-get 'diagnostics resp))
                                    (errs (mapcar #'stlc2m--diag->flycheck-error diags)))
                               (funcall callback 'finished errs)))))))))
                 stlc2m--pending)

        (process-send-string proc (concat json-str "\n"))
        ;; Flycheck expects a cleanup object; nil is fine here.
        nil))))

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

(defun stlc2m--render-problems (srcbuf resp)
  "Render RESP (alist) for SRCBUF into the problems buffer."
  (let ((buf (get-buffer-create stlc2m--problems-buffer-name)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)

      (insert (format "STLC2m Problems\nSource: %s\nURI: %s\n\n"
                      (buffer-name srcbuf)
                      (or (buffer-local-value 'stlc2m--last-uri srcbuf) "<unknown>")))

      (let ((ok (alist-get 'ok resp)))
        (if (not ok)
            (let* ((err (alist-get 'error resp))
                   (code (alist-get 'code err))
                   (msg (alist-get 'message err)))
              (insert (format "Protocol/Server error: [%s] %s\n" code msg)))
          (let ((diags (alist-get 'diagnostics resp)))
            (if (null diags)
                (insert "No diagnostics.\n")
              (dolist (d diags)
                (let* ((code (alist-get 'code d))
                       (sev  (alist-get 'severity d))
                       (msg  (alist-get 'message d))
                       (range (alist-get 'range d))
                       (start (alist-get 'start range))
                       (line (alist-get 'line start))
                       (col0 (alist-get 'col start))
                       (related (alist-get 'related d)))
                  (insert (format "[%s/%s] %s\n" code sev msg))
                  (insert (format "  at %d:%d\n" line col0))
                  (when (and (listp related) related)
                    (dolist (r related)
                      (let* ((rmsg (alist-get 'message r))
                             (rrange (alist-get 'range r))
                             (rstart (alist-get 'start rrange))
                             (rline (alist-get 'line rstart))
                             (rcol0 (alist-get 'col rstart)))
                        (insert (format "    related: %s\n" rmsg))
                        (insert (format "      -> %d:%d\n" rline rcol0)))))
                  (insert "\n"))))))))

      (goto-char (point-min))
      (view-mode 1)
      (display-buffer buf)))

(defun stlc2m-problems ()
  "Show the STLC2m Problems buffer for the current source buffer.
Uses the last stored server response (no re-check)."
  (interactive)
  (unless stlc2m--last-response
    (user-error "No diagnostics available yet.  Run Flycheck (e.g. save buffer) first"))
  (stlc2m--render-problems (current-buffer) stlc2m--last-response))

(define-derived-mode stlc2m-source-mode prog-mode "STLC2M"
  "Major mode for STLC2M files.")
(add-to-list 'auto-mode-alist '("\\.lang\\'" . stlc2m-source-mode))

(flycheck-define-generic-checker 'stlc2m
  "A Flycheck checker using the stlc2m --server."
  :start #'stlc2m--flycheck-start
  :modes '(stlc2m-source-mode))
(add-to-list 'flycheck-checkers 'stlc2m)

;;;###autoload
(define-minor-mode stlc2m-mode
  "Minor mode to check STLC2M buffers via stlc2m --server and display diagnostics via Flycheck."
  :lighter " stlc2m"
  (if stlc2m-mode
      (progn
	(flycheck-mode 1)
	;; Make stlc2m the active checker in this buffer:
	(setq-local flycheck-checker 'stlc2m)
        ;; Optional: check on enable
        (stlc2m-check-buffer))
    (progn
      (kill-local-variable 'flycheck-checker)
      (remove-hook 'after-save-hook #'stlc2m-check-buffer t))))

(defun stlc2m-check-buffer ()
  "Check current buffer with stlc2m server."
  (interactive)
  (stlc2m--send-check (current-buffer)))

(provide 'stlc2m-mode)
;;; stlc2m-mode.el ends here
