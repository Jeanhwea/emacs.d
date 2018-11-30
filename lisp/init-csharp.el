(add-hook 'csharp-mode-hook
  (lambda()
    (setq
      show-trailing-whitespace t
      indent-tabs-mode nil
      prettify-symbols-alist
        (append prettify-symbols-alist
          '(
             ;; ----------------------------------------------------------------
             ("public" . 9794)          ; ♂
             ("private" . 9792)         ; ♀
             ("this" . 964)             ; τ
             ("return" . 1103)          ; я
             ("new" . 1080)             ; и
             ;; ----------------------------------------------------------------
             ("null" . 951)             ; η
             ("true" . 8857)            ; ⊙
             ("false" . 8855)           ; ⊗
             ;; ----------------------------------------------------------------
             ("&&" . 8743)              ; ∧
             ("||" . 8744)              ; ∨
             ("!" . 172)                ; ¬
             ("=>" . 8658)              ; ⇒
             ("<=" . 8804)              ; ≤
             ("!=" . 8800)              ; ≠
             ;; ----------------------------------------------------------------
             )))
    ;; string-inflection
    (local-set-key (kbd "M-i") 'string-inflection-java-style-cycle)
    (hl-line-mode 1)))

(provide 'init-csharp)
