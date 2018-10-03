(add-hook 'sh-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq sh-basic-offset 2)
    (setq tab-width 2)))

(provide 'init-sh)
