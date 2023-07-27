(defun asm-hook-func ()
  (setq
    show-trailing-whitespace t
    indent-tabs-mode t))

(add-hook 'asm-mode-hook 'asm-hook-func)
(add-hook 'nasm-mode-hook 'asm-hook-func)

(provide 'init-asm)
