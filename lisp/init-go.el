;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go get -u -v github.com/rogpeppe/godef
;; go get -u -v golang.org/x/tools/cmd/goimports
;; go get -u -v github.com/mdempsky/gocode
;; go get -u -v golang.org/x/tools/gopls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md

(when (require 'lsp-mode)
  ;; Set up before-save hooks to format buffer and add/delete imports.
  ;; Make sure you don't have other gofmt/goimports hooks enabled.
  ;; (defun lsp-go-install-save-hooks ()
  ;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
  ;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))
  ;; (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  )

(add-hook 'go-mode-hook
  #'(lambda()
      (setq
        gofmt-command "goimports"
        ;; indent-tabs-mode nil
        tab-width 4
        show-trailing-whitespace t)

      ;; lsp-mode for golang
      (lsp-deferred)

      ;; (flyspell-mode 1)
      (hl-line-mode 1)
      (rainbow-delimiters-mode 1)))

(defun jh/run-go-scratch (&optional file)
  "Run go scratch source code."
  (let*
    ((sbufname "*go-scratch-buffer*")
      (file (or file (buffer-file-name)))
      (filename file))
    (setq cmd (format "go run %s" file))
    (if (string-match-p ".*\\.go$" filename)
      (progn
        (save-buffer)
        (if (get-buffer sbufname)
          (setq sbuf (get-buffer sbufname))
          (setq sbuf (generate-new-buffer sbufname)))
        (shell-command cmd sbuf sbuf)
        (display-buffer sbuf)
        (message (format "Run %s" file)))
      (user-error (format "Not a valid go sratch file: %s" file)))))

(provide 'init-go)
