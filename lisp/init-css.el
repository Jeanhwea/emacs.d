(add-hook 'css-mode-hook
  (lambda()
    (setq
      show-trailing-whitespace t
      indent-tabs-mode nil
      css-indent-offset 2
      tab-width 2)))

(provide 'init-css)
