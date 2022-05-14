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

(provide 'init-sh)
