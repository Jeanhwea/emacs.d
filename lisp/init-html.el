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
    (hl-line-mode 1)))

(add-hook 'mhtml-mode-hook
  (lambda ()
    (add-to-list 'hs-special-modes-alist
      (list 'mhtml-mode
        "<!--\\|<[^/>]*[^/]>"
        "-->\\|</[^/>]*[^/]>"
        "<!--"
        'sgml-skip-tag-forward nil))))


;; -------------------------------------------------------------------------
;; emmet-mode
;; -------------------------------------------------------------------------
(when (require 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))


(provide 'init-html)
