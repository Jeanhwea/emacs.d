;; -----------------------------------------------------------------------------
;; My Package List
;; -----------------------------------------------------------------------------
(setq my-packages
  '(
     ;; elpy
     ;; lsp-java
     ;; lsp-mode
     ;; lsp-ui
     ;; multiple-cursors
     ;; ace-jump-mode
     adoc-mode
     ag
     auctex
     bison-mode
     browse-at-remote
     browse-kill-ring
     cargo
     color-theme-sanityinc-solarized
     color-theme-sanityinc-tomorrow
     company
     ;; company-fuzzy
     counsel
     ;; csharp-mode
     csv-mode
     cmake-mode
     cuda-mode
     dashboard
     dired-k
     ;; docker
     ;; docker-tramp
     dockerfile-mode
     doom-themes
     edit-indirect
     editorconfig
     eglot
     elfeed
     elm-mode
     emamux
     emmet-mode
     engine-mode
     evil
     evil-collection
     evil-commentary
     evil-leader
     evil-numbers
     evil-pinyin-mode
     exec-path-from-shell
     expand-region
     figlet
     fill-column-indicator
     flyspell-correct
     ggtags
     git-auto-commit-mode
     git-msg-prefix
     go-dlv
     go-mode
     go-projectile
     go-tag
     gotest
     graphviz-dot-mode
     groovy-mode
     haskell-mode
     htmlize
     plantuml-mode
     javadoc-lookup
     js-comint
     json-mode
     keycast
     keyfreq
     leetcode
     ;; llvm-ts-mode
     lua-mode
     magit
     magit-lfs
     magit-todos
     markdown-mode
     matlab-mode
     nasm-mode
     nyan-mode
     ob-go
     ob-tmux
     org-bullets
     org-ref
     org-table-comment
     pangu-spacing
     posframe
     projectile
     protobuf-mode
     pyim
     rainbow-delimiters
     realgud
     realgud-lldb
     request
     restclient
     rg
     rust-mode
     rustic
     sicp
     smex
     spacemacs-theme
     sql-indent
     sqlformat
     string-inflection
     sudo-edit
     swiper
     thrift
     tide
     tldr
     verb
     vterm
     web-mode
     wgrep
     which-key
     xclip
     xcscope
     yaml-mode
     yasnippet))

;; -----------------------------------------------------------------------------
;; initialize emacs package system
;; -----------------------------------------------------------------------------
(setq byte-compile-warnings '(cl-functions))
(require 'cl-lib)

(when (require 'package)
  ;; Add MELPA
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

  ;; Use TUNA mirror
  ;(setq package-archives
  ;  '(("melpa-tuna" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
  ;     ("gnu-tuna" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
  ;     ("nongnu-tuna" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))


  ;; must initialize package first
  (when (version< emacs-version "27.0")
    (package-initialize)))

;; (eval-when-compile (require 'cl))

;; load all the path in ~/.emacs.d/site-lisp
(defun jh/add-subdirs-to-load-path (parent-dir)
  "Adds every non-hidden subdir of PARENT-DIR to `load-path'."
  (let* ((default-directory parent-dir))
    (progn
      (setq load-path
        (append
          (cl-remove-if-not
            (lambda (dir) (file-directory-p dir))
            (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
          load-path)))))

(jh/add-subdirs-to-load-path (expand-file-name "site-lisp/" user-emacs-directory))

(defun jh/install-packages-from-elpa ()
  "Install all packages from ELPA."
  (interactive)
  (package-refresh-contents)
  (package-install-selected-packages)
  (byte-force-recompile "~/.emacs.d/"))

(provide 'init-package)
