(add-hook 'sgml-mode-hook
  (lambda()
    (setq
      show-trailing-whitespace t
      indent-tabs-mode nil
      html-indent-level 2
      css-indent-offset 2
      sgml-basic-offset 2
      sgml-attribute-offset 2
      js-indent-level 2
      tab-width 2)
    (dolist (mode '(sgml-mode mhtml-mode))
      (add-to-list 'hs-special-modes-alist
        (list mode
          "<!--\\|<[^/>]*[^/]>"
          "-->\\|</[^/>]*[^/]>"
          "<!--"
          'sgml-skip-tag-forward nil)))
    (hl-line-mode 1)
    (hs-minor-mode 1)
    (local-set-key (kbd "C-c h") 'hs-toggle-hiding)))


;; -------------------------------------------------------------------------
;; emmet-mode
;; -------------------------------------------------------------------------
(when (require 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))


(provide 'init-html)
