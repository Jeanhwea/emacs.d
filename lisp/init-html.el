(add-hook 'sgml-mode-hook
  #'(lambda()
      (setq
        show-trailing-whitespace t
        indent-tabs-mode nil
        html-indent-level 2
        css-indent-offset 2
        sgml-basic-offset 2
        sgml-attribute-offset 2
        js-indent-level 2
        tab-width 4)
      (highlight-current-line)))

(add-hook 'mhtml-mode-hook
  #'(lambda ()
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
  ;; only " /", "/" and "" are valid. eg. <meta />, <meta/>, <meta>
  (setq emmet-self-closing-tag-style " /")
  ;; jsx use className="..." instead of class=""
  (setq emmet-expand-jsx-className? t)
  ;; indent 2 spaces.
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))

  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))

(provide 'init-html)
