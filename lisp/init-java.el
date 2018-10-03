(add-hook 'java-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq c-basic-offset 2)
    (setq tab-width 2)))

(provide 'init-java)
