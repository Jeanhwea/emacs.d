(add-hook 'java-mode-hook
  (lambda()
    (setq
      show-trailing-whitespace t
      indent-tabs-mode nil
      c-basic-offset 2
      tab-width 2
      prettify-symbols-alist
        (append prettify-symbols-alist
          '(
             ;; ----------------------------------------------------------------
             ("public" . 9794)          ; ♂
             ("private" . 9792)         ; ♀
             ("this" . 964)             ; τ
             ("return" . 1103)          ; я
             ("new" . 1081)             ; й
             ;; ----------------------------------------------------------------
             ("null" . 951)             ; η
             ("true" . 8730)            ; √
             ("false" . 215)            ; ×
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
    (hl-line-mode 1)
    (local-set-key (kbd "M-h") 'hs-toggle-hiding)))

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
      (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
  (cond
    ((eq system-type 'windows-nt)
      (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
      (setq meghanada-maven-path "mvn.cmd"))
    (t
      (setq meghanada-java-path "java")
      (setq meghanada-maven-path "mvn"))))

(provide 'init-java)
