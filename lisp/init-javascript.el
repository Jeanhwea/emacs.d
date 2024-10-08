(add-hook 'typescript-mode-hook
  #'(lambda ()
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
        compilation-scroll-output t
        compilation-ask-about-save nil
        compilation-error-regexp-alist '(typescript-tslint typescript-tsc-pretty typescript-tsc)
        prettify-symbols-alist
        (append prettify-symbols-alist
          '(
             ;; ----------------------------------------------------------------
             ("public" . 9794)          ; ♂
             ("private" . 9792)         ; ♀
             ("function" . 402)         ; ƒ
             ("this" . 964)             ; τ
             ("return" . 1103)          ; я
             ("var" . 957)              ; ν
             ("const" . 1089)           ; с
             ("let" . 8467)             ; ℓ
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
             ("undefined" . 250)        ; ú
             ("null" . 1488)            ; א
             ("true" . 8730)            ; √
             ("false" . 215)            ; ×
             ;; ----------------------------------------------------------------
             ("&&" . 8743)              ; ∧
             ("||" . 8744)              ; ∨
             ("!" . 172)                ; ¬
             ("=>" . 8658)              ; ⇒
             ("<=" . 8804)              ; ≤
             (">=" . 8805)              ; ≥
             ;; ("!=" . 8800)              ; ≠
             ;; ("===" . 8801)             ; ≡
             ;; ("each" . 8704)            ; ∀
             ;; ("in" . 8712)              ; ∈
             ("Math" . 1084)            ; м
             ("Math.PI" . 960)          ; π
             ("Infinity" . 8734)        ; ∞
             ;; ----------------------------------------------------------------
             )))
      ;; highlight current line
      (highlight-current-line)
      (rainbow-delimiters-mode 1)
      ;; setup js-comint keybindings

      (local-set-key (kbd "C-c C-e") 'js-comint-send-region)
      (local-set-key (kbd "C-c C-c") 'js-comint-send-last-sexp)
      (local-set-key (kbd "C-c C-r") 'js-comint-send-region)
      (local-set-key (kbd "C-c C-b") 'js-comint-send-buffer)))

(defun jh/format-js-source (&optional file)
  "Format js source code."
  (let
    ((file (or file (buffer-file-name))))
    (progn
      (save-buffer)
      ;; format buffer
      (shell-command (format "prettier --single-quote --write \"%s\"" file))
      ;; reload buffer
      (revert-buffer nil t)
      ;; leave a messge
      (message (format "Formatted %s" file)))))

(when (require 'js-comint)
  (setq js-comint-program-command "node"))

(when (require 'tide)
  ;; formats the buffer before saving
  ;; (add-hook 'before-save-hook 'tide-format-before-save)

  ;; add hook for tide-mode
  (add-hook 'typescript-mode-hook
    #'(lambda ()
        (tide-setup)
        (setq flycheck-check-syntax-automatically '(save mode-enabled))
        (flycheck-mode 1)
        (eldoc-mode 1)
        (tide-hl-identifier-mode 1)))

  ;; add hook for web-mode
  (add-hook 'web-mode-hook
    #'(lambda()
        (setq
          show-trailing-whitespace t
          indent-tabs-mode nil
          tab-width 2
          js-indent-level 2
          typescript-indent-level 2)
        (setq tide-format-options '(:indentSize 2 :tabSize 2))

        (when
          (or
            (string-equal "ts" (file-name-extension buffer-file-name))
            (string-equal "tsx" (file-name-extension buffer-file-name)))
          (tide-setup))))
  ;; END of tide
  )

(add-to-list 'auto-mode-alist '("\\.mjs?\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.cjs?\\'" . javascript-mode))
;; (add-to-list 'auto-mode-alist '("\\.ts?\\'" . javascript-mode))
(provide 'init-javascript)
