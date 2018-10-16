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
             ;; -----------------------------------------------------------------
             ;; ("import" . 10940)          ; ⪼
             ;; ("export" . 10939)          ; ⪻
             ;; ("function" . 119917)       ; 𝑭
             ;; ("from" . 8647)             ; ⇇
             ;; ("any" . 119912)            ; 𝑨
             ;; ("boolean" . 119913)        ; 𝑩
             ;; ("number" . 119925)         ; 𝑵
             ;; ("string" . 119930)         ; 𝑺
             ;; ("void" . 119933)           ; 𝑽
             ;; -----------------------------------------------------------------
             ("=" . 8612)                   ; ↤
             ("=>" . 8658)                  ; ⇒
             ("<=" . 8804)                  ; ≤
             (">=" . 8805)                  ; ≥
             ("==" . 61)                    ; =
             ("!=" . 8800)                  ; ≠
             ("===" . 8801)                 ; ≡
             ("!==" . 8802)                 ; ≢
             ;; -----------------------------------------------------------------
             )))))

(when (require 'js-comint)
  (setq inferior-js-program-command "node"))

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
