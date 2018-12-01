(add-hook 'typescript-mode-hook
  (lambda ()
    (setq
      show-trailing-whitespace t
      indent-tabs-mode nil
      tab-width 2
      js-indent-level 2
      typescript-indent-level 2
      ;; tide-format-options
      ;;  '(:baseIndentSize 0
      ;;    :indentSize 2
      ;;    :tabSize 2
      ;;    :convertTabsToSpaces t)
      prettify-symbols-alist
        (append prettify-symbols-alist
          '(
             ;; ----------------------------------------------------------------
             ("public" . 9794)          ; ♂
             ("private" . 9792)         ; ♀
             ("function" . 402)         ; ƒ
             ("self" . 949)             ; ε
             ("this" . 964)             ; τ
             ("return" . 1103)          ; я
             ("var" . 957)              ; ν
             ("const" . 1089)           ; с
             ("let" . 8467)             ; ℓ
             ("new" . 1080)             ; и
             ("Infinity" . 8734)        ; ∞
             ("from" . 8810)            ; ≪
             ("as" . 8776)              ; ≈
             ("implements" . 8715)      ; ∋
             ("extends" . 8834)         ; ⊂
             ("Math" . 8499)            ; ℳ
             ("PI" . 960)               ; π
             ("console" . 231)          ; ç
             ;; ----------------------------------------------------------------
             ("undefined" . 956)        ; μ
             ("null" . 951)             ; η
             ("true" . 8857)            ; ⊙
             ("false" . 8855)           ; ⊗
             ;; ----------------------------------------------------------------
             ("&&" . 8743)              ; ∧
             ("||" . 8744)              ; ∨
             ("!" . 172)                ; ¬
             ("=>" . 8658)              ; ⇒
             ("<=" . 8804)              ; ≤
             (">=" . 8805)              ; ≥
             ("!=" . 8800)              ; ≠
             ;; ("===" . 8801)             ; ≡
             ;; ("!==" . 8802)             ; ≢
             ;; ----------------------------------------------------------------
             )))
    ;; highlight current line
    (hl-line-mode 1)
    ;; setup js-comint keybindings
    (local-set-key (kbd "C-c C-e") 'js-comint-send-region)
    (local-set-key (kbd "C-c C-c") 'js-comint-send-last-sexp)
    (local-set-key (kbd "C-c C-r") 'js-comint-send-region)
    (local-set-key (kbd "C-c C-b") 'js-comint-send-buffer)
    ;; string-inflection
    (local-set-key (kbd "M-i") 'string-inflection-java-style-cycle)
    (hs-minor-mode 1)
    (local-set-key (kbd "M-h") 'hs-toggle-hiding)))


(when (require 'js-comint)
  (setq js-comint-program-command "node"))


(when (require 'tide)
  ;; add hook for tide-mode
  (add-hook 'typescript-mode-hook
    (lambda ()
      (tide-setup)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      (flycheck-mode 1)
      (eldoc-mode 1)
      (tide-hl-identifier-mode 1)))
  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save))

;; (add-to-list 'auto-mode-alist '("\\.ts?\\'" . javascript-mode))
(provide 'init-javascript)
