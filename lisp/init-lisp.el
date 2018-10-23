(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (setq
      show-trailing-whitespace t
      indent-tabs-mode nil
      lisp-indent-offset 2
      tab-width 2
      prettify-symbols-alist
        (append prettify-symbols-alist
          '(
             ("lambda" . 955)           ; λ
             ("defun" . 402)            ; ƒ
             )))

    (rainbow-delimiters-mode)))

(provide 'init-lisp)
