(setq rust-format-on-save t)

(add-hook 'rustic-mode-hook
  #'(lambda()

      (evil-define-key '(normal visual) 'local (kbd "<tab>") 'rust-tab-action)
      (evil-define-key '(normal visual) 'local (kbd "TAB") 'rust-tab-action)
      (evil-define-key '(normal visual) 'local (kbd "<S-tab>") 'workflow-inflect-string)
      (evil-define-key '(normal visual) 'local (kbd "<backtab>") 'workflow-inflect-string)

      (highlight-current-line)
      (rainbow-delimiters-mode 1)))

(defun rust-tab-action ()
  "Default <tab> key action for golang."
  (interactive)
  (jh/tab-dwim))

(defun jh/run-rust-scratch (&optional file)
  "Run rust scratch file."
  (interactive)
  (let*
    ((file (or file (buffer-file-name)))
      (dir (file-name-directory file))
      (name (file-name-nondirectory file)))
    (save-buffer)
    (if (not window-system)
      (jh/run-rust-scratch-in-tmux file dir name)
      (jh/run-rust-scratch-gui file dir name))))

(defun jh/run-rust-scratch-gui (file dir name)
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
      (message (format "Run %s" file)))))

(defun jh/run-rust-scratch-in-tmux (file dir name)
  "Run rust scratch in tmux."
  (emamux:run-command (format "cd %s" dir))
  (emamux:run-command "clear")

  ;; (setq cmd1 (format "rustc %s -o debug.run" name))
  ;; (setq cmd2 "./debug.run")
  ;; (setq cmd (concat cmd1 " && " cmd2))
  (setq cmd "cargo run -q")

  (emamux:run-command cmd)

  (message (format "Run %s" file)))

(when (require 'rustic)
  (setq rustic-lsp-client 'eglot)
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1))))

(when (require 'cargo)
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(provide 'init-rust)
