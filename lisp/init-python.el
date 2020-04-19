(add-hook 'python-mode-hook
  #'(lambda ()
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
             ;; ----------------------------------------------------------------
             ("def" . 402)              ; ƒ
             ("self" . 964)             ; τ
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
             ("ehat" . 234)             ; ê
             ("ybar" . 563)             ; ȳ
             ("yhat" . 375)             ; ŷ
             ;; ----------------------------------------------------------------
             ("None" . 1488)            ; א
             ("True" . 8730)            ; √
             ("False" . 215)            ; ×
             ;; ----------------------------------------------------------------
             ("and" . 8743)             ; ∧
             ("or" . 8744)              ; ∨
             ("not" . 172)              ; ¬
             ("<=" . 8804)              ; ≤
             (">=" . 8805)              ; ≥
             ("!=" . 8800)              ; ≠
             ("in" . 8712)              ; ∈
             ("not in" . 8713)          ; ∉
             ("intersection" . 8745)    ; ∩
             ("union" . 8746)           ; ∪
             ("issuperset" . 8835)      ; ⊃
             ("issubset" . 8834)        ; ⊂
             ;; ----------------------------------------------------------------
             )))
      (hl-line-mode 1)
      (rainbow-delimiters-mode 1)
      (highlight-indent-guides-mode 1)
      (when (require 'elpy)
        ;; pip install jedi flake8 autopep8 rope
        (delete 'elpy-module-highlight-indentation elpy-modules)
        (elpy-mode 1)
        (elpy-enable)
        (when (executable-find "python3")
          (setq elpy-rpc-python-command "python3"))
        (local-set-key (kbd "M-.") 'elpy-goto-definition))))

(when (executable-find "python3")
  (setq
    python-shell-interpreter "python3"
    python-shell-interpreter-args "-i"))

(provide 'init-python)
