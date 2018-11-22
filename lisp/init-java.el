(add-hook 'java-mode-hook
  (lambda()
    (setq
      show-trailing-whitespace t
      indent-tabs-mode nil
      c-basic-offset 2
      tab-width 2)
    ;; string-inflection
    (local-set-key (kbd "M-u") 'string-inflection-java-style-cycle)
    (hl-line-mode 1)))

(provide 'init-java)
