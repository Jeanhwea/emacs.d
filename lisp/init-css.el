(add-hook 'css-mode-hook
  (lambda()
    (setq
      show-trailing-whitespace t
      indent-tabs-mode nil
      sgml-basic-offset 2
      sgml-attribute-offset 2
      css-indent-offset 2
      tab-width 2)
    (hs-minor-mode 1)
    (local-set-key (kbd "M-h") 'hs-toggle-hiding)))

(provide 'init-css)
