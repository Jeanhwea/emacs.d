(add-hook 'css-mode-hook
  #'(lambda()
      (setq
        show-trailing-whitespace t
        indent-tabs-mode nil
        sgml-basic-offset 2
        sgml-attribute-offset 2
        css-indent-offset 2
        tab-width 4)
      (rainbow-delimiters-mode 1)))

(provide 'init-css)
