(add-hook 'typescript-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq js-indent-level 2)
    (setq tab-width 2)
    (setq prettify-symbols-alist
      '(
         ("lambda" . 955)               ; λ
         ("import" . 10940)             ; ⪼
         ("export" . 10939)             ; ⪻
         ("from" . 8712)                ; ∈
         ("=>" . 8658)                  ; ⇒
         ("<=" . 8804)                  ; ≤
         (">=" . 8805)                  ; ≥
         ("==" . 8876)                  ; ≈
         ("!=" . 8777)                  ; ≉
         ("===" . 8801)                 ; ≡
         ("!==" . 8802)                 ; ≢
         ("constructor" . 8364)         ; €
         ("function" . 8753)            ; ∱
         ("this" . 8706)                ; ∂
         ("any" . 120120)               ; 𝔸
         ("boolean" . 120121)           ; 𝔹
         ("number" . 8469)              ; ℕ
         ("string" . 120138)            ; 𝕊
         ("void" . 120141)              ; 𝕍
         ))))

(when (require 'js-comint)
  (setq inferior-js-program-command "node"))

(add-hook 'typescript-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq typescript-indent-level 2)
    (setq tab-width 2)))

(when (require 'tide)
  (add-hook 'typescript-mode-hook
     (lambda()
       (tide-setup)
       (flycheck-mode 1)
       (setq flycheck-check-syntax-automatically '(save mode-enabled))
       (eldoc-mode 1)
       (tide-hl-identifier-mode 1)))

  ;; format options
  ;; (setq tide-format-options
  ;;   '( :baseIndentSize 2
  ;;      :indentSize 2
  ;;      :tabSize 2
  ;;      :convertTabsToSpaces t))

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save))

;; (add-to-list 'auto-mode-alist '("\\.ts?\\'" . javascript-mode))

(provide 'init-javascript)
