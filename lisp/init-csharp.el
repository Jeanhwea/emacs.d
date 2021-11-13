(add-hook 'csharp-mode-hook
  #'(lambda()
      (setq
        show-trailing-whitespace t
        ;; indent-tabs-mode nil
        ;; c-basic-offset 4
        ;; tab-width 4
        prettify-symbols-alist
        (append prettify-symbols-alist
          '(
             ;; ----------------------------------------------------------------
             ("public" . 9794)          ; ♂
             ("private" . 9792)         ; ♀
             ("this" . 964)             ; τ
             ("return" . 1103)          ; я
             ("new" . 1081)             ; й
             ;; ----------------------------------------------------------------
             ("alpha" . 945)            ; α
             ("beta" . 946)             ; β
             ("delta" . 948)            ; δ
             ("epsilon" . 949)          ; ε
             ("theta" . 952)            ; θ
             ("lambda" . 955)           ; λ
             ("mu" . 956)               ; μ
             ("omega" . 969)            ; ω
             ;; ----------------------------------------------------------------
             ("null" . 1488)            ; א
             ("true" . 8730)            ; √
             ("false" . 215)            ; ×
             ;; ----------------------------------------------------------------
             ("&&" . 8743)              ; ∧
             ("||" . 8744)              ; ∨
             ("!" . 172)                ; ¬
             ("=>" . 8658)              ; ⇒
             ("<=" . 8804)              ; ≤
             ("!=" . 8800)              ; ≠
             ;; ----------------------------------------------------------------
             )))
      (highlight-current-line)
      (rainbow-delimiters-mode 1)))

(provide 'init-csharp)
