;; -----------------------------------------------------------------------------
;; A reasonable Emacs configuration files.
;; -----------------------------------------------------------------------------
(setq-default
  user-full-name "Jinghui Hu"
  user-mail-address "hujinghui@buaa.edu.cn"
  debug-on-error t)

(let ((mini-version "26.1"))
  (when (version< emacs-version mini-version)
    (error
      "This config supported mini version is v%s, upgrade your Emacs first"
      mini-version)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; -----------------------------------------------------------------------------
;; bootstrap
;; -----------------------------------------------------------------------------
(require 'init-util)
(require 'init-package) ; call "package-initialize" here

;; -----------------------------------------------------------------------------
;; behavior
;; -----------------------------------------------------------------------------
(require 'init-abbrev)
(require 'init-basic)
(require 'init-desktop)
(require 'init-edit)

;; -----------------------------------------------------------------------------
;; plugin
;; -----------------------------------------------------------------------------
(require 'init-completion)
(require 'init-codetta)
(require 'init-dired)
(require 'init-elfeed)
(require 'init-git)
(require 'init-org)
(require 'init-projectile)
(require 'init-markdown)
(require 'init-query)
(require 'init-springboot)
(require 'init-angular)
(require 'init-yasnippet)
(require 'init-workflow)

;; -----------------------------------------------------------------------------
;; programming language
;; -----------------------------------------------------------------------------
(require 'init-cc)
(require 'init-css)
(require 'init-csv)
(require 'init-csharp)
(require 'init-dot)
(require 'init-groovy)
(require 'init-html)
(require 'init-nxml)
(require 'init-java)
(require 'init-javascript)
(require 'init-lisp)
(require 'init-python)
(require 'init-sh)
(require 'init-sql)

;; -----------------------------------------------------------------------------
;; misc
;; -----------------------------------------------------------------------------
(require 'init-ffmpeg)
(require 'init-experimental)
(require 'init-keymate)

;; -----------------------------------------------------------------------------
;; local settings
;; -----------------------------------------------------------------------------
(let ((local-settings "~/.emacs.local.el"))
  (when (file-exists-p local-settings) (load-file local-settings)))

(provide 'init)

;; -----------------------------------------------------------------------------
;; the bottom part is auto generated by Emacs, DO NOT EDIT IT DIRECTLY!!!
;; -----------------------------------------------------------------------------
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(package-selected-packages
     (quote
       (
         ;; --------------------------------------------------------------------
         ace-jump-mode
         adoc-mode
         ag
         auctex
         browse-at-remote
         browse-kill-ring
         color-theme-sanityinc-solarized
         color-theme-sanityinc-tomorrow
         company
         company-fuzzy
         counsel
         csharp-mode
         csv-mode
         cuda-mode
         dashboard
         docker
         dockerfile-mode
         edit-indirect
         editorconfig
         elfeed
         elm-mode
         elpy
         emmet-mode
         engine-mode
         evil
         evil-leader
         exec-path-from-shell
         expand-region
         figlet
         fill-column-indicator
         flycheck
         flyspell-correct
         ggtags
         graphviz-dot-mode
         groovy-mode
         haskell-mode
         htmlize
         js-comint
         keyfreq
         leetcode
         lsp-java
         lsp-mode
         lsp-ui
         lua-mode
         magit
         markdown-mode
         monokai-theme
         multiple-cursors
         nyan-mode
         org-bullets
         pangu-spacing
         posframe
         projectile
         protobuf-mode
         pyim
         rainbow-delimiters
         restclient
         rg
         sicp
         smex
         spacemacs-theme
         sql-indent
         sqlformat
         string-inflection
         swiper
         tide
         verb
         web-mode
         yaml-mode
         zenburn-theme
         yasnippet
         ;; --------------------------------------------------------------------
         ))))
