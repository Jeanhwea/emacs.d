(setq rust-format-on-save t)

(defun jh/run-rust-scratch (&optional file)
  "Run rust scratch file."
  (interactive)
  (let*
    ((file (or file (buffer-file-name)))
      (dir (file-name-directory file))
      (name (file-name-nondirectory file)))

    (emamux:run-command (format "cd %s" dir))
    (emamux:run-command "clear")

    (setq cmd1 (format "rustc %s -o debug.run" name))
    (setq cmd2 "./debug.run")
    (setq cmd (concat cmd1 " && " cmd2))
    (emamux:run-command cmd)))

(provide 'init-rust)
