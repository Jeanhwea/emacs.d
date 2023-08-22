(setq rust-format-on-save t)

(defun jh/run-rust-scratch (&optional file)
  "Run rust scratch file."
  (interactive)
  (let*
    ((file (or file (buffer-file-name)))
      (dir (file-name-directory file))
      (name (file-name-nondirectory file)))
    (save-buffer)
    (if (not window-system)
      (jh/run-rust-scratch-in-tmux dir name)
      (jh/run-rust-scratch-gui dir name))))

(defun jh/run-rust-scratch-gui (dir name)
  (let
    ((sbufname "*rust-scratch-buffer*") (default-directory dir))

    (setq cmd1 (format "rustc %s -o debug.run" name))
    (setq cmd2 "./debug.run")
    (setq cmd (concat cmd1 " && " cmd2))

    (progn
      (if (get-buffer sbufname)
        (setq sbuf (get-buffer sbufname))
        (setq sbuf (generate-new-buffer sbufname)))
      (shell-command cmd sbuf sbuf)
      (display-buffer sbuf)
      (message (format "Run %s" name)))))

(defun jh/run-rust-scratch-in-tmux (dir name)
  "Run rust scratch in tmux."
  (emamux:run-command (format "cd %s" dir))
  (emamux:run-command "clear")
  (setq cmd1 (format "rustc %s -o debug.run" name))
  (setq cmd2 "./debug.run")
  (setq cmd (concat cmd1 " && " cmd2))
  (emamux:run-command cmd)
  (message (format "Run %s" name)))

(provide 'init-rust)
