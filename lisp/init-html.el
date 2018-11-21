(add-hook 'sgml-mode-hook
  (lambda()
    (setq
      show-trailing-whitespace t
      indent-tabs-mode nil
      sgml-basic-offset 2
      html-indent-level 2
      tab-width 2)
    (hl-line-mode 1)))


;; -------------------------------------------------------------------------
;; emmet-mode
;; -------------------------------------------------------------------------
(when (require 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))


(provide 'init-html)
