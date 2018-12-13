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
             ;; ----------------------------------------------------------------
             ("lambda" . 955)           ; λ
             ("defun" . 402)            ; ƒ
             ;; ----------------------------------------------------------------
             ("nil" . 1488)             ; א
             ("not" . 172)              ; ¬
             ("and" . 8743)             ; ∧
             ("or" . 8744)              ; ∨
             ;; ----------------------------------------------------------------
             )))
    (hl-line-mode 1)
    (rainbow-delimiters-mode)
    (hs-minor-mode 1)
    (local-set-key (kbd "C-c h") 'hs-toggle-hiding)))

(provide 'init-lisp)
