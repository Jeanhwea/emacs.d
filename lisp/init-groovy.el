(add-hook 'groovy-mode-hook
  #'(lambda ()
      (setq
        show-trailing-whitespace t
        indent-tabs-mode nil
        groovy-indent-offset 2
        tab-width 4)
      (hl-line-mode 1)
      (rainbow-delimiters-mode 1)))

(provide 'init-groovy)
