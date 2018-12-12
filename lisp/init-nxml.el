(add-hook 'nxml-mode-hook
  (lambda ()
    (hl-line-mode 1)
    (hs-minor-mode 1)
    (local-set-key (kbd "C-c h") 'hs-toggle-hiding)
    (add-to-list 'hs-special-modes-alist
      (list 'nxml-mode
        "<!--\\|<[^/>]*[^/]>"
        "-->\\|</[^/>]*[^/]>"
        "<!--"
        'nxml-forward-element nil))))

(provide 'init-nxml)
