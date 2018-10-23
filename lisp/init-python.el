(add-hook 'python-mode-hook
  (lambda()
    (when (and (when (require 'ggtags)) (derived-mode-p 'python-mode))
      (ggtags-mode 1))
    (setq
      show-trailing-whitespace t
      indent-tabs-mode nil
      tab-width 2
      python-indent-offset 2
      prettify-symbols-alist
        (append prettify-symbols-alist
          '(
             ;; -----------------------------------------------------------------
             ("self" . 949)                 ; ε
             ("def" . 402)                  ; ƒ
             ("not" . 172)                  ; ¬
             ("and" . 8743)                 ; ∧
             ("or" . 8744)                  ; ∨
             ;; -----------------------------------------------------------------
             ("<=" . 8804)                  ; ≤
             (">=" . 8805)                  ; ≥
             ("!=" . 8800)                  ; ≠
             ;; -----------------------------------------------------------------
             )))))

(provide 'init-python)
