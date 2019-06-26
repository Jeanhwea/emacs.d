(defun ct/start-point ()
  "Get the start point."
  (save-excursion
    (and (re-search-backward "codetta: start" nil t)
      (progn (beginning-of-line) (point)))))

(defun ct/output-point ()
  "Get the output point."
  (save-excursion
    (and (re-search-forward "codetta: output" nil t)
      (progn (beginning-of-line) (point)))))

(defun ct/end-point ()
  "Get the end point."
  (save-excursion
    (and (re-search-forward "codetta: end" nil t)
      (progn (beginning-of-line) (point)))))

(defun ct/command-lines ()
  "Get the command lines."
  (let ((sp (ct/start-point))
         (op (ct/output-point)))
    (and sp op
      (cdr (split-string
             (buffer-substring-no-properties sp op) "\n")))))

(defun ct/execute-command ()
  "Execute shell command and capture outputs."
  (let* ((default-directory (or (jh/git-project-root-dir (buffer-file-name)) default-directory))
          (rawcmds (mapconcat
                     (lambda (line) (replace-regexp-in-string "^[ \t]*\\(#\\|//\\|;;\\|--\\|rem\\)[ \t]*" "" line))
                     (ct/command-lines) "\n"))
          (commands (if (jh/windows?) (jh/trim rawcmds) rawcmds)))
    (shell-command-to-string commands)))

(defun ct/insert-output (str)
  "Insert STR to output."
  (let ((op (ct/output-point))
         (ep (ct/end-point)))
    (and op ep
      (progn
        (goto-char op)
        (forward-line)
        (kill-region (point) ep)
        (insert str)))))

(defun ct/expand-command ()
  "Expand command, which means execute shell command and insert stdout to buffer."
  (interactive)
  (let ((sp (ct/start-point))
         (op (ct/output-point))
         (ep (ct/end-point)))
    (and sp op ep
      (save-excursion (ct/insert-output (ct/execute-command))))))


(provide 'init-codetta)
