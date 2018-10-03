(add-hook 'sgml-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq html-indent-level 2)
    (setq tab-width 2)))

(provide 'init-html)
