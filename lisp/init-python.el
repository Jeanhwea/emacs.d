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
             ;; ----------------------------------------------------------------
             ("def" . 402)              ; ƒ
             ("cls" . 952)              ; θ
             ("self" . 949)             ; ε
             ("return" . 1103)          ; я
             ("new" . 1081)             ; й
             ("lambda" . 955)           ; λ
             ("__init__" . 9836)        ; ♬
             ;; ----------------------------------------------------------------
             ("None" . 951)             ; η
             ("True" . 8730)            ; √
             ("False" . 215)            ; ×
             ;; ----------------------------------------------------------------
             ("and" . 8743)             ; ∧
             ("or" . 8744)              ; ∨
             ("not" . 172)              ; ¬
             ("<=" . 8804)              ; ≤
             (">=" . 8805)              ; ≥
             ("!=" . 8800)              ; ≠
             ;; ----------------------------------------------------------------
             )))
    (hl-line-mode 1)
    (rainbow-delimiters-mode 1)
    ;; string-inflection
    (local-set-key (kbd "M-i") 'string-inflection-python-style-cycle)
    (hs-minor-mode 1)
    (local-set-key (kbd "C-c h") 'hs-toggle-hiding)))

(when (jh/mac?)
  (setq
    python-shell-interpreter "python3"
    python-shell-interpreter-args "-i"))

(provide 'init-python)
