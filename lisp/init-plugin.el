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
;; expand-region
;; -------------------------------------------------------------------------
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;; -------------------------------------------------------------------------
;; magit
;; -------------------------------------------------------------------------
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)


;; -------------------------------------------------------------------------
;; yasnippet
;; -------------------------------------------------------------------------
(require 'yasnippet)
(yas-global-mode 1)
;; (setq yas/indent-line nil)


;; -------------------------------------------------------------------------
(provide 'init-plugin)
