(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[jt]s\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[jt]sx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  ;; (flycheck-mode +1)
  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

;; if you use treesitter based typescript-ts-mode (emacs 29+)
(add-hook 'typescript-ts-mode-hook #'setup-tide-mode)

(when (require 'web-mode)

  (add-hook 'web-mode-hook
    (lambda ()
      (setq
        comment-start "// "
        comment-end ""
        web-mode-comment-style 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2)

      (when (string-equal "tsx" (file-name-extension buffer-file-name))
        (setup-tide-mode))

      (eglot-ensure))))

;; (when (require 'vue3-mode)
;;     (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-html-mode)))

(provide 'init-web)
