(defun asm-hook-func ()
  (setq
    show-trailing-whitespace t
    indent-tabs-mode t))

(add-hook 'asm-mode-common-hook 'asm-hook-func)
(add-hook 'nasm-mode-common-hook 'asm-hook-func)

(provide 'init-asm)
