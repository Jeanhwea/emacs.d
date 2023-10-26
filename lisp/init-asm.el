(defun asm-hook-func ()
  (setq
    show-trailing-whitespace t
    comment-start "#"
    comment-end ""
    indent-tabs-mode t))

;; 注释 gas 风格汇编, # 表示注释, 注意这里字符的写法
(setq asm-comment-char ?\#)

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

(add-to-list 'auto-mode-alist '("\\.img\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.bin\\'" . hexl-mode))


(eval-after-load 'hexl-mode
  (evil-define-key 'normal hexl-mode-map (kbd "x") 'hexl-insert-hex-char))

(provide 'init-asm)
