(add-hook 'java-mode-hook
  (lambda()
    (setq
      show-trailing-whitespace t
      indent-tabs-mode
      c-basic-offset 2
      tab-width 2)))

(provide 'init-java)
