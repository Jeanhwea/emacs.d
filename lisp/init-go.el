(add-hook 'go-mode-hook
  #'(lambda()
      (setq
        show-trailing-whitespace t
        indent-tabs-mode nil)
      ;; (flyspell-mode 1)
      (hl-line-mode 1)
      (rainbow-delimiters-mode 1)))


(provide 'init-go)
