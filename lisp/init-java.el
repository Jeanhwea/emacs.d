(add-hook 'java-mode-hook
  #'(lambda()
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
             ("public" . 9794)          ; ♂
             ("private" . 9792)         ; ♀
             ("this" . 964)             ; τ
             ("return" . 1103)          ; я
             ("new" . 1081)             ; й
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
             ;; ("forEach" . 8704)         ; ∀
             ;; ----------------------------------------------------------------
             )))
      (hl-line-mode 1)
      (rainbow-delimiters-mode 1)))

(when
  (and (require 'lsp-mode) (require 'company-lsp)
    ;; (require 'lsp-ui)
    (require 'lsp-java))
  (setq
    lsp-ui-sideline-show-code-actions nil
    lsp-ui-doc-enable nil
    lsp-java-completion-import-order ["org.springframework" "com.avic" "java" "javax" "com" "org"])

  (add-hook 'java-mode-hook 'lsp))

(defconst jh/gjf-dir (expand-file-name "resource" user-emacs-directory))
(defconst jh/gjf-file "google-java-format-1.7-all-deps.jar")
;; (defconst jh/gjf-file "google-java-format-1.7-120-all-deps.jar")
(defun jh/format-java-source (&optional file)
  "Format java source code."
  (let
    ((file (or file (buffer-file-name)))
      (jarfile (expand-file-name jh/gjf-file jh/gjf-dir)))
    (progn
      (save-buffer)
      ;; format buffer
      (shell-command (format "java -jar %s --replace %s" jarfile file))
      ;; reload buffer
      (revert-buffer nil t)
      (message (format "Done format file: %s" file)))))

;; -----------------------------------------------------------------------------
;; meghanada
;;
;;   git clone https://github.com/mopemope/meghanada-emacs.git
;;
;; -----------------------------------------------------------------------------
;; (when (require 'meghanada)
;;   (add-hook 'java-mode-hook
;;     #'(lambda ()
;;         ;; meghanada-mode on
;;         (meghanada-mode t)
;;         (flycheck-mode +1)
;;         (setq c-basic-offset 2)
;;         ;; use code format
;;         ;; (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
;;         ))
;;   (if (jh/windows?)
;;     (setq
;;       meghanada-java-path
;;       (expand-file-name "bin/java.exe" (getenv "JAVA_HOME"))
;;       meghanada-maven-path "mvn.cmd")
;;     ;; unix-like system
;;     (setq
;;       meghanada-java-path "java"
;;       meghanada-maven-path "mvn")))

;; (when (require 'lsp-java)
;;   (add-hook 'java-mode-hook #'lsp))

(provide 'init-java)
