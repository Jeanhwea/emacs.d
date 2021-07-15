(add-hook 'nxml-mode-hook
  #'(lambda ()
      (sgml-mode)
      (hl-line-mode 1)
      (add-to-list 'hs-special-modes-alist
        (list 'nxml-mode
          "<!--\\|<[^/>]*[^/]>"
          "-->\\|</[^/>]*[^/]>"
          "<!--"
          'nxml-forward-element nil))
      (hl-line-mode 1)))

(provide 'init-nxml)
