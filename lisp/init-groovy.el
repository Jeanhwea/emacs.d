(add-hook 'groovy-mode-hook
  (lambda ()
    (setq
      show-trailing-whitespace t
      indent-tabs-mode nil
      groovy-indent-offset 2
      tab-width 2)
    (hl-line-mode 1)
    (rainbow-delimiters-mode 1)
    (hs-minor-mode 1)
    (local-set-key (kbd "C-c h") 'hs-toggle-hiding)))

(provide 'init-groovy)
