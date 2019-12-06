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
 (when (file-exists-p local-settings)
   (load-file local-settings)))

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
         ag
         auctex
         browse-at-remote
         color-theme-sanityinc-solarized
         color-theme-sanityinc-tomorrow
         company
         counsel
         csharp-mode
         csv-mode
         cuda-mode
         dashboard
         docker
         dockerfile-mode
         edit-indirect
         elfeed
         elm-mode
         elpy
         emmet-mode
         engine-mode
         evil
         exec-path-from-shell
         expand-region
         fancy-narrow
         figlet
         fill-column-indicator
         flycheck
         ggtags
         graphviz-dot-mode
         groovy-mode
         haskell-mode
         highlight-indent-guides
         htmlize
         js-comint
         keyfreq
         lua-mode
         magit
         markdown-mode
         meghanada
         multiple-cursors
         nyan-mode
         org-bullets
         pangu-spacing
         projectile
         protobuf-mode
         rainbow-delimiters
         restclient
         sicp
         smex
         sql-indent
         string-inflection
         swiper
         tide
         web-mode
         yaml-mode
         yasnippet
         ;; --------------------------------------------------------------------
         ))))
