(add-hook 'sh-mode-hook
  #'(lambda()
      (setq
        show-trailing-whitespace t
        indent-tabs-mode nil
        sh-basic-offset 2
        tab-width 2)
      ;; npm i -g bash-language-server
      ;; (eglot-ensure)
      ))

(provide 'init-sh)
