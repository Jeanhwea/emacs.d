(add-hook 'graphviz-dot-mode-hook
  (lambda ()
    (setq
      show-trailing-whitespace t
      indent-tabs-mode nil
      tab-width 2
      graphviz-dot-indent-width 2)
    (hs-minor-mode 1)
    (local-set-key (kbd "M-h") 'hs-toggle-hiding)))

(provide 'init-dot)
