;; -------------------------------------------------------------------------
;; company
;; -------------------------------------------------------------------------
(when (require 'company)
  (add-hook 'after-init-hook 'global-company-mode)
  ;; add normal keybindings
  (define-key company-mode-map (kbd "M-/") 'company-complete)
  (define-key company-active-map (kbd "M-/") 'company-other-backend)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

;; -------------------------------------------------------------------------
;; undo-tree
;; -------------------------------------------------------------------------
(when (require 'undo-tree)
  (global-undo-tree-mode))

;; -------------------------------------------------------------------------
;; expand-region
;; -------------------------------------------------------------------------
(when (require 'expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region))


;; -------------------------------------------------------------------------
;; magit
;; -------------------------------------------------------------------------
(when (require 'magit)
  (global-set-key (kbd "C-x g") 'magit-status))


;; -------------------------------------------------------------------------
;; yasnippet
;; -------------------------------------------------------------------------
(when (require 'yasnippet)
  (yas-global-mode 1))
;; (setq yas/indent-line nil)


;; -------------------------------------------------------------------------
;; nyan-mode
;; -------------------------------------------------------------------------
(when (require 'nyan-mode)
  (nyan-mode 1))


;; -------------------------------------------------------------------------
;; exec-path-from-shell
;; -------------------------------------------------------------------------
(when (require 'exec-path-from-shell)
  (when (string-equal "darwin" system-type)
    (exec-path-from-shell-initialize)))

;; -------------------------------------------------------------------------
;; browse-at-remote
;; -------------------------------------------------------------------------
;; (when (require 'browse-at-remote)
;;   (defalias 'open 'browse-url))

;; -------------------------------------------------------------------------
;; js-comint
;; -------------------------------------------------------------------------
(when (require 'js-comint)
  (setq inferior-js-program-command "node"))

;; -------------------------------------------------------------------------
;; tide
;; -------------------------------------------------------------------------
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

;; -------------------------------------------------------------------------
;; emmet-mode
;; -------------------------------------------------------------------------
(when (require 'emmet-mode)
  (add-hook 'mhtml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))


;; -------------------------------------------------------------------------
(provide 'init-plugin)
