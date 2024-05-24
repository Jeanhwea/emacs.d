;; -----------------------------------------------------------------------------
;; plantuml-mode
;; -----------------------------------------------------------------------------

(add-hook 'plantuml-mode-hook
  #'(lambda()
      (setq
        show-trailing-whitespace t
        indent-tabs-mode nil
        c-basic-offset 2
        tab-width 3
        comment-start "' "
        comment-end "")))

(when (require 'plantuml-mode)
  (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode)))


(provide 'init-puml)
