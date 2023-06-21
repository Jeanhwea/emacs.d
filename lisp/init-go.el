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
        compilation-scroll-output t
        compilation-ask-about-save nil
        tab-width 4
        show-trailing-whitespace t)

      (eglot-ensure)
      ;; (evil-define-key '(normal visual) 'local (kbd "gr") 'eglot-rename)

      ;; ;; lsp-mode for golang
      ;; (lsp-deferred)
      ;; (evil-define-key '(normal visual) 'local (kbd "gd") 'lsp-find-definition)
      ;; (evil-define-key '(normal visual) 'local (kbd "gr") 'lsp-rename)
      (evil-define-key '(normal visual) 'local (kbd "<tab>") 'go-tab-action)
      (evil-define-key '(normal visual) 'local (kbd "TAB") 'go-tab-action)
      (evil-define-key '(normal visual) 'local (kbd "<S-tab>") 'workflow-inflect-string)
      (evil-define-key '(normal visual) 'local (kbd "<backtab>") 'workflow-inflect-string)

      ;; (flyspell-mode 1)
      (highlight-current-line)
      (rainbow-delimiters-mode 1)))

(defun go-tab-action ()
  "Default <tab> key action for golang."
  (interactive)
  (jh/tab-dwim))

(defun go-fold-import ()
  "Automatic fold import(...)"
  (save-excursion
    (beginning-of-buffer)
    (when (re-search-forward "^import (" nil t)
      (end-of-line) (hs-toggle-hiding))))

;; setup for go-tags
;; (setq go-tag-args '("-transform" "snakecase"))
;; (setq go-tag-args '("-transform" "pascalcase"))
(setq go-tag-args '("-transform" "camelcase"))

(defun gen-struct-from-sql ()
  "Generate gorm struct from sql."
  (interactive)
  (insert (shell-command-to-string "dm golang-gen-struct-from-create-table")))

(defun jh/format-golang-source (&optional file)
  "Format golang source code."
  (let
    ((file (or file (buffer-file-name))))
    (progn
      (save-buffer)
      ;; format buffer
      (shell-command (format "sed -i '/^import (/,/^\s*)/ { /^\s*$/ d; }' \"%s\"" file))
      ;; reload buffer
      (revert-buffer nil t)
      (gofmt)
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

(defun go-implement-method ()
  "Implement a method."
  (interactive)
  (let
    ((srvfile (buffer-file-name))
      (line (string-trim (thing-at-point 'line t)))
      (impl-name (jh/go-impl-name))
      (recv-name (jh/go-receiver-name)))
    (setq sign
      (format "func (%s *%s) %s {\n\tpanic(\"not implement yet\")\n}"
        recv-name impl-name line))
    (progn
      (find-file (jh/re-replace ".go$" "_impl.go" srvfile))
      (end-of-buffer)
      (newline)
      (insert sign))))

(defun go-get-master ()
  "Run go upgrade to master branch."
  (interactive)
  (let*
    ((pkg (car (split-string (string-trim (thing-at-point 'line t)) " ")))
      (default-directory (jh/git-root (buffer-file-name)))
      (cmd (format "go get %s@master && go mod tidy" pkg)))
    (async-shell-command cmd)
    (message (format "Run %s" cmd))))

(defun go-get-current ()
  "Run go get to upgrade package under line."
  (interactive)
  (let*
    ((pkg (car (split-string (string-trim (thing-at-point 'line t)) " ")))
      (default-directory (jh/git-root (buffer-file-name)))
      (cmd (format "go get %s && go mod tidy" pkg)))
    (async-shell-command cmd)
    (message (format "Run %s" cmd))))

(defun go-mod-tidy ()
  "Run go mod tidy."
  (interactive)
  (let
    ((file (buffer-name))
      (default-directory (jh/git-root (buffer-file-name)))
      (cmd "go mod tidy"))
    ;; (unless (string= file "go.mod")
    ;;   (user-error "Please open go.mod and try again!"))
    (async-shell-command cmd)
    (message (format "Run %s" cmd))))

(defun go-show-cover-report()
  "Open cover test file"
  (interactive)
  (let ((report (expand-file-name "cover.out" (jh/git-root (buffer-file-name)))))
    (unless (file-exists-p report)
      (user-error "report file is not exist: %s" report))
    (go-coverage report)))

(defun go-prettify-json ()
  "Prettify JSON string inside back slash quote."
  (interactive)
  (let ((beg) (end))
    (save-excursion
      (when (re-search-backward "`{")
        (setq beg (+ (point) 1)))
      (when (re-search-forward "}`")
        (setq end (- (point) 1)))
      (when (and beg end)
        (json-pretty-print beg end)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; project management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun go/swap-test-and-subject ()
  "Do swap test file and subject file."
  (interactive)
  (let ((file (jh/re-replace "_impl.go$" ".go" (buffer-file-name))))
    (if (string-match-p "_test.go$" file)
      ;; test -> subject
      (find-file (jh/re-replace "_test.go$" ".go" file))
      ;; subject -> test
      (find-file (jh/re-replace ".go$" "_test.go" file)))))

(defun go/swap-impl-and-subject ()
  "Do swap impl file and subject file."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (string-match-p "_impl.go$" file)
      ;; impl -> subject
      (find-file (jh/re-replace "_impl.go$" ".go" file))
      ;; subject -> impl
      (find-file (jh/re-replace ".go$" "_impl.go" file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; snippet utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jh/go-current-func ()
  "Search function name backward."
  (setq func "function")
  (save-excursion
    (progn
      (re-search-backward "^func" nil t)
      (setq line (thing-at-point 'line t))
      (setq method-flag (substring line 5 6))
      (when (string= method-flag "(")
        (progn
          (forward-word)
          (forward-word)))
      (forward-word)
      (forward-word)
      (setq func (thing-at-point 'word t))))
  func)

(defun jh/go-package-name (&optional file)
  "Return the package name for golang."
  (let*
    ((file (or file (buffer-file-name))))
    (jh/file-base-name (directory-file-name (jh/parent-dir file)))))

(defun jh/go-class-name ()
  "Read golang class name."
  (let ((basename (jh/file-base-name (buffer-file-name))))
    (if (string-match-p ".*_impl" basename)
      (jh/camelcase basename)
      (jh/pascalcase basename))))

(defun jh/go-impl-name ()
  "Read golang implement name."
  (jh/camelcase (format "%sImpl" (jh/go-class-name))))

(defun jh/go-test-name ()
  "Read golang test class name."
  (let ((fn (buffer-file-name)))
    (if (string-match-p "lc.*_test.go$" fn)
      (jh/re-replace "^" "_"
        (jh/re-replace "Test$" "" (jh/upcase (jh/file-base-name fn))))
      (jh/re-replace "Test$" "" (jh/pascalcase (jh/file-base-name fn))))))

(defun jh/go-inter-name ()
  "Read golang inter name."
  (jh/pascalcase (jh/file-base-name (buffer-file-name))))

(defun jh/go-receiver-name ()
  "Get receiver short name."
  (let ((clzname (jh/go-class-name)))
    (jh/camelcase (substring clzname 0 1))))

(provide 'init-go)
