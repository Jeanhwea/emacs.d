(setq rust-format-on-save t)

(defun jh/run-rust-scratch (&optional file)
  "Run rust scratch file."
  (interactive)
  (let*
    ((file (or file (buffer-file-name)))
      (dir (file-name-directory file))
      (name (file-name-nondirectory file)))
    (setq cmd0 (format "cd %s" dir))
    (setq cmd1 (format "rustc %s -o debug.run" name))
    (setq cmd2 "./debug.run")
    (setq cmd (concat cmd0 " && " cmd1 " && " cmd2))
    (emamux:run-command cmd)))

(provide 'init-rust)
