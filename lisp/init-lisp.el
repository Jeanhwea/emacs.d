(add-hook 'emacs-lisp-mode-hook
  #'(lambda ()
      (setq
        show-trailing-whitespace t
        indent-tabs-mode nil
        lisp-indent-offset 2
        tab-width 2
        prettify-symbols-alist
        (append prettify-symbols-alist
          '(
             ;; ----------------------------------------------------------------
             ("defun" . 402)            ; ƒ
             ("defvar" . 957)           ; ν
             ("defconst" . 1089)        ; с
             ;; ----------------------------------------------------------------
             ("lambda" . 955)           ; λ
             ("interactive" . 967)      ; χ
             ;; ----------------------------------------------------------------
             ("nil" . 1488)             ; א
             ("not" . 172)              ; ¬
             ("and" . 8743)             ; ∧
             ("or" . 8744)              ; ∨
             ;; ----------------------------------------------------------------
             )))
      (hl-line-mode 1)
      (rainbow-delimiters-mode)
      (highlight-indent-guides-mode 1)))

(provide 'init-lisp)
