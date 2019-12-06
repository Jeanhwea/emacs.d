(add-hook 'graphviz-dot-mode-hook
  (lambda ()
    (setq
      show-trailing-whitespace t
      indent-tabs-mode nil
      tab-width 2
      graphviz-dot-auto-indent-on-semi nil
      graphviz-dot-indent-width 2)
    (hl-line-mode 1)
    (rainbow-delimiters-mode 1)))

(provide 'init-dot)
