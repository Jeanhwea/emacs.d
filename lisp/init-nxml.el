(add-hook 'nxml-mode-hook
  #'(lambda ()
      (sgml-mode)
      (highlight-current-line)
      (add-to-list 'hs-special-modes-alist
        (list 'nxml-mode
          "<!--\\|<[^/>]*[^/]>"
          "-->\\|</[^/>]*[^/]>"
          "<!--"
          'nxml-forward-element nil))
      (highlight-current-line)))

(provide 'init-nxml)
