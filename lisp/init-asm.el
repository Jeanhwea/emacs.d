(defun asm-hook-func ()
  (setq
    show-trailing-whitespace t
    comment-start "#"
    asm-comment-char "#"
    indent-tabs-mode t))

(add-hook 'asm-mode-hook 'asm-hook-func)
(add-hook 'nasm-mode-hook 'asm-hook-func)

;; -----------------------------------------------------------------------------
;; nasm
;; -----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))
(add-to-list 'auto-mode-alist '("\\.S\\'" . asm-mode))

;; -----------------------------------------------------------------------------
;; bochs
;; -----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.bxrc\\'" . conf-mode))

(provide 'init-asm)
