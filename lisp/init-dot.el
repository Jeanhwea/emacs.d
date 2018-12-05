(add-hook 'graphviz-dot-mode-hook
  (lambda ()
    (setq
      show-trailing-whitespace t
      indent-tabs-mode nil
      tab-width 2
      graphviz-dot-indent-width 2)))

(provide 'init-dot)
