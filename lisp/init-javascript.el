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
             ("this" . 964)             ; τ
             ("return" . 961)           ; ρ
             ;; ----------------------------------------------------------------
             ("undefined" . 956)        ; μ
             ("null" . 8709)            ; ∅
             ("true" . 30495)           ; 真
             ("false" . 20551)          ; 假
             ;; ----------------------------------------------------------------
             ("&&" . 8743)              ; ∧
             ("||" . 8744)              ; ∨
             ("!" . 172)                ; ¬
             ("=>" . 8658)              ; ⇒
             ("<=" . 8804)              ; ≤
             (">=" . 8805)              ; ≥
             ("!=" . 8800)              ; ≠
             ("===" . 8801)             ; ≡
             ("!==" . 8802)             ; ≢
             ;; ----------------------------------------------------------------
             )))
    ;; setup js-comint keybindings
    (local-set-key (kbd "C-c C-e") 'js-comint-send-region)
    (local-set-key (kbd "C-c C-c") 'js-comint-send-last-sexp)
    (local-set-key (kbd "C-c C-r") 'js-comint-send-region)
    (local-set-key (kbd "C-c C-b") 'js-comint-send-buffer)
    ;; string-inflection
    (local-set-key (kbd "M-u") 'string-inflection-java-style-cycle)
    (hl-line-mode 1)
    (hs-minor-mode 1)))


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
