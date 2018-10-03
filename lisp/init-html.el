(add-hook 'sgml-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq html-indent-level 2)
    (setq tab-width 2)))


;; -------------------------------------------------------------------------
;; emmet-mode
;; -------------------------------------------------------------------------
(when (require 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))


(provide 'init-html)
