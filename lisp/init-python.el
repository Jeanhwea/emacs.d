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
             ("return" . 961)           ; ρ
             ("lambda" . 955)           ; λ
             ;; ----------------------------------------------------------------
             ("None" . 951)             ; η
             ("True" . 30495)           ; 真
             ("False" . 20551)          ; 假
             ;; ----------------------------------------------------------------
             ("and" . 8743)             ; ∧
             ("or" . 8744)              ; ∨
             ("not" . 172)              ; ¬
             ("<=" . 8804)              ; ≤
             (">=" . 8805)              ; ≥
             ("!=" . 8800)              ; ≠
             ;; ----------------------------------------------------------------
             )))
    ;; string-inflection
    (local-set-key (kbd "M-u") 'string-inflection-python-style-cycle)))

(when (jh/mac?)
  (setq
    python-shell-interpreter "python3"
    python-shell-interpreter-args "-i"))

(provide 'init-python)
