;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go install github.com/mdempsky/gocode@latest
;; go install github.com/rogpeppe/godef@latest
;; go install golang.org/x/tools/cmd/godoc@latest
;; go install golang.org/x/tools/cmd/goimports@latest
;; go install golang.org/x/tools/gopls@latest
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md

;; (when (require 'lsp-mode)
;;   (setq lsp-headerline-breadcrumb-enable nil))

(add-hook 'go-mode-hook
  #'(lambda()
      (setq
        gofmt-command "goimports"
        ;; indent-tabs-mode nil
        tab-width 4
        show-trailing-whitespace t)

      (eglot-ensure)
      ;; (define-key evil-normal-state-local-map (kbd "gr") 'eglot-rename)

      ;; ;; lsp-mode for golang
      ;; (lsp-deferred)
      ;; (define-key evil-normal-state-local-map (kbd "gd") 'lsp-find-definition)
      ;; (define-key evil-normal-state-local-map (kbd "gr") 'lsp-rename)

      ;; (flyspell-mode 1)
      (highlight-current-line)
      (rainbow-delimiters-mode 1)))

(add-hook 'thrift-mode-hook
  #'(lambda() (setq thrift-indent-level 4)))

(defun jh/format-thrift-source (&optional file)
  "Format thrift source code."
  (let
    ((file (or file (buffer-file-name))))
    (progn
      (save-buffer)
      ;; format buffer
      (shell-command (format "sed -i 's/  */ /g;s/  *,/,/g' \"%s\"" file))
      ;; reload buffer
      (revert-buffer nil t)
      (jh/indent-current-buffer)
      ;; leave a messge
      (message (format "Formatted t %s" file)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; snippet utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jh/go-current-func ()
  "Search function name backward."
  (setq func "function")
  (save-excursion
    (progn
      (re-search-backward "^func [[:alnum:]]+" nil t)
      (forward-word)
      (forward-word)
      (setq func (thing-at-point 'word t))))
  func)

(defun jh/go-class-name ()
  "Read golang class name."
  (jh/pascalcase (jh/file-base-name (buffer-file-name))))

(defun jh/go-inter-name ()
  "Read golang inter name."
  (jh/pascalcase (jh/file-base-name (buffer-file-name))))

(defun jh/go-receiver-name ()
  "Get receiver short name."
  (let ((clzname (jh/go-class-name)))
    (jh/camelcase (substring clzname 0 1))))

(provide 'init-go)
