(add-hook 'groovy-mode-hook
  #'(lambda ()
      (setq
        show-trailing-whitespace t
        indent-tabs-mode nil
        groovy-indent-offset 2
        tab-width 4)
      (highlight-current-line)
      (rainbow-delimiters-mode 1)))

(provide 'init-groovy)
