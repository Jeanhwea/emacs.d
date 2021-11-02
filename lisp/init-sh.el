(add-hook 'sh-mode-hook
  #'(lambda()
      (setq
        show-trailing-whitespace t
        indent-tabs-mode nil
        sh-basic-offset 4
        tab-width 4)
      ;; npm i -g bash-language-server
      ;; (eglot-ensure)
      ))

(provide 'init-sh)
