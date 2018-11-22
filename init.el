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
(require 'init-dired)
(require 'init-elfeed)
(require 'init-git)
(require 'init-org)
(require 'init-yasnippet)

;; -----------------------------------------------------------------------------
;; programming language
;; -----------------------------------------------------------------------------
(require 'init-css)
(require 'init-csv)
(require 'init-html)
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
  '(custom-safe-themes
     (quote
       ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
  '(package-selected-packages
     (quote
       (ag
        browse-at-remote
        color-theme-sanityinc-solarized
        company
        counsel
        csv-mode
        edit-indirect
        elfeed
        emmet-mode
        engine-mode
        evil
        exec-path-from-shell
        expand-region
        fill-column-indicator
        ggtags
        htmlize
        hungry-delete
        js-comint
        magit
        markdown-mode
        multiple-cursors
        nyan-mode
        org-bullets
        pangu-spacing
        projectile
        rainbow-delimiters
        smex
        sql-indent
        string-inflection
        swiper
        tide
        yasnippet))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
