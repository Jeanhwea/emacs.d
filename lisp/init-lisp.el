(add-hook 'emacs-lisp-mode-hook
  #'(lambda ()
      (setq
        show-trailing-whitespace t
        indent-tabs-mode nil
        lisp-indent-offset 2
        tab-width 4
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
      (highlight-current-line)
      (company-fuzzy-mode 1)
      (rainbow-delimiters-mode)))

(add-to-list 'auto-mode-alist '(".dir-locals.el\\'" . emacs-lisp-mode))

(provide 'init-lisp)
