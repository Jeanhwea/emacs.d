(add-hook 'java-mode-hook
  (lambda()
    (setq
      show-trailing-whitespace t
      indent-tabs-mode nil
      c-basic-offset 2
      tab-width 2
      compilation-scroll-output t
      compilation-ask-about-save nil
      compilation-error-regexp-alist '(java maven)
      prettify-symbols-alist
        (append prettify-symbols-alist
          '(
             ;; ----------------------------------------------------------------
             ("package" . 167)          ; §
             ("public" . 9794)          ; ♂
             ("private" . 9792)         ; ♀
             ("implements" . 8715)      ; ∋
             ("extends" . 8834)         ; ⊂
             ("instanceof" . 8712)      ; ∈
             ("this" . 964)             ; τ
             ("return" . 1103)          ; я
             ("new" . 1081)             ; й
             ("static" . 351)           ; ş
             ("logger" . 287)           ; ğ
             ("final" . 8750)           ; ∮
             ("import" . 9834)          ; ♪
             ;; ----------------------------------------------------------------
             ("null" . 1488)            ; א
             ("true" . 8730)            ; √
             ("false" . 215)            ; ×
             ;; ----------------------------------------------------------------
             ("&&" . 8743)              ; ∧
             ("||" . 8744)              ; ∨
             ("!" . 172)                ; ¬
             ("->" . 8594)              ; →
             ("<=" . 8804)              ; ≤
             ("!=" . 8800)              ; ≠
             ;; ----------------------------------------------------------------
             )))
    ;; string-inflection
    (local-set-key (kbd "M-i") 'string-inflection-java-style-cycle)
    (hl-line-mode 1)
    (rainbow-delimiters-mode 1)
    (hs-minor-mode 1)
    (local-set-key (kbd "C-c h") 'hs-toggle-hiding)))

;; -----------------------------------------------------------------------------
;; meghanada
;;
;;   git clone https://github.com/mopemope/meghanada-emacs.git
;;
;; -----------------------------------------------------------------------------
(when (require 'meghanada)
  (add-hook 'java-mode-hook
    (lambda ()
      ;; meghanada-mode on
      (meghanada-mode t)
      (flycheck-mode +1)
      (setq c-basic-offset 2)
      ;; use code format
      ;; (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
      ))

    (if (jh/windows?)
      (setq
        meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME"))
        meghanada-maven-path "mvn.cmd")
      ;; unix-like system
      (setq
        meghanada-java-path "java"
        meghanada-maven-path "mvn")))

(provide 'init-java)
