(add-hook 'js-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq js-indent-level 2)
    (setq tab-width 2)))

(when (require 'js-comint)
  (setq inferior-js-program-command "node"))

(add-hook 'typescript-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq typescript-indent-level 2)
    (setq tab-width 2)))

(when (require 'tide)
  (add-hook 'typescript-mode-hook
     (lambda()
       (tide-setup)
       (flycheck-mode 1)
       (setq flycheck-check-syntax-automatically '(save mode-enabled))
       (eldoc-mode 1)
       (tide-hl-identifier-mode 1)))

  ;; format options
  ;; (setq tide-format-options
  ;;   '( :baseIndentSize 2
  ;;      :indentSize 2
  ;;      :tabSize 2
  ;;      :convertTabsToSpaces t))

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save))


(provide 'init-javascript)
