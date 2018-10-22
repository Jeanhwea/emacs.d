(add-hook 'python-mode-hook
  (lambda()
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
             ;; -----------------------------------------------------------------
             ("<=" . 8804)                  ; ≤
             (">=" . 8805)                  ; ≥
             ("!=" . 8800)                  ; ≠
             ;; -----------------------------------------------------------------
             )))))

(provide 'init-python)
