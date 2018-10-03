(add-hook 'css-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq css-indent-offset 2)
    (setq tab-width 2)))

(provide 'init-css)
