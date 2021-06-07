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
             ;; ("import" . 8656)          ; ⇐
             ;; ("import" . 304)          ; İ
             ;; ("this" . 964)             ; τ
             ;; ("return" . 1103)          ; я
             ;; ("new" . 1081)             ; й
             ;; ----------------------------------------------------------------
             ;; ("null" . 1488)            ; א
             ;; ("true" . 8730)            ; √
             ;; ("false" . 215)            ; ×
             ;; ----------------------------------------------------------------
             ;; ("&&" . 8743)              ; ∧
             ;; ("||" . 8744)              ; ∨
             ;; ("!" . 172)                ; ¬
             ;; ("->" . 8594)              ; →
             ;; ("<=" . 8804)              ; ≤
             ;; ("!=" . 8800)              ; ≠
             ;; ("for" . 8704)             ; ∀
             ;; ----------------------------------------------------------------
             )))
      ;; (flyspell-mode 1)
      (hl-line-mode 1)
      (rainbow-delimiters-mode 1)))

;; (when
;;   (and (require 'lsp-mode) (require 'lsp-ui) (require 'lsp-java))
;;   (setq
;;     lsp-server-install-dir (expand-file-name "lsp-cache" user-emacs-directory)
;;     lsp-java-server-install-dir (expand-file-name "jdtls" lsp-server-install-dir)
;;     lsp-ui-sideline-show-code-actions nil
;;     lsp-ui-doc-enable nil
;;     lsp-java-completion-import-order
;;     ["com.avic" "org.springframework" "java" "javax" "com" "org"])

;;   (add-hook 'java-mode-hook 'lsp))

(defconst jh/gjf-dir (expand-file-name "resource" user-emacs-directory))
(defconst jh/gjf-file "google-java-format-1.7-all-deps.jar")
;; (defconst jh/gjf-file "google-java-format-1.7-120-all-deps.jar")

(defconst gjf-prog (format "java -jar \"%s\"" (expand-file-name jh/gjf-file jh/gjf-dir))
  "Google Java Formatter Program.")

(defun jh/format-java-source (&optional file)
  "Format java source code."
  (let
    ((file (or file (buffer-file-name))))
    (progn
      (save-buffer)
      ;; format buffer
      (shell-command (format "%s --replace \"%s\"" gjf-prog file))
      ;; reload buffer
      (revert-buffer nil t)
      ;; leave a messge
      (message (format "Formatted %s" file)))))

(defun jh/run-java-scratch (&optional file)
  "Run java scratch source code."
  (let*
    ((sbufname "*java-scratch-buffer*")
      (file (or file (buffer-file-name)))
      (filename (file-name-nondirectory file))
      (clsname (file-name-sans-extension filename))
      (default-directory (file-name-directory file)))
    (setq cmd (format "javac -encoding UTF-8 %s && java %s && rm %s*.class" filename clsname clsname))
    (if (string-match-p "Solution.*\\.java$" filename)
      (progn
        (save-buffer)
        (if (get-buffer sbufname)
          (setq sbuf (get-buffer sbufname))
          (setq sbuf (generate-new-buffer sbufname)))
        (shell-command cmd sbuf sbuf)
        (display-buffer sbuf)
        (message (format "Run %s" file)))
      (user-error (format "Not a valid java sratch file: %s" file)))))

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

(provide 'init-java)
