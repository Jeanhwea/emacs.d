(add-hook 'sh-mode-hook
  #'(lambda()
      (setq
        show-trailing-whitespace t
        indent-tabs-mode nil
        sh-basic-offset 4
        tab-width 4)
      ;; npm i -g bash-language-server
      ;; (eglot-ensure)
      (evil-define-key '(normal visual) 'local (kbd "<tab>") 'shell-tab-action)
      (evil-define-key '(normal visual) 'local (kbd "TAB") 'shell-tab-action)))

(defun shell-tab-action ()
  "Default <tab> key action for shell."
  (interactive)
  (jh/tab-dwim))

(defun jh/run-shell-scratch (&optional file)
  "Run shell scratch source code."
  (let*
    ((sbufname "*shell-scratch-buffer*")
      (file (or file (buffer-file-name)))
      (filename file))
    (setq cmd (format "%s" file))
    (if (string-match-p ".*\\.sh$" filename)
      (progn
        (save-buffer)
        (if (get-buffer sbufname)
          (setq sbuf (get-buffer sbufname))
          (setq sbuf (generate-new-buffer sbufname)))
        (async-shell-command cmd sbuf sbuf)
        (display-buffer sbuf)
        (message (format "Run %s" file)))
      (user-error (format "Not a valid shell sratch file: %s" file)))))

(provide 'init-sh)
