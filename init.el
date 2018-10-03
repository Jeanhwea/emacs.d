;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; -----------------------------------------------------------------------------
;; BEGIN of confiugration
;; -----------------------------------------------------------------------------

;; bootstrap
(require 'init-site-lisp)
(require 'init-elpa)
(require 'init-startup)
(require 'init-basic)
(require 'init-apperence)
(require 'init-abbrev)

;; plugin
(require 'init-plugin)
(require 'init-git)
(require 'init-completion)
(require 'init-yasnippet)
(require 'init-helm)

;; programming language
(require 'init-typescript)

;; misc and experimental feature
(require 'init-misc)
(require 'init-experimental)

;; -----------------------------------------------------------------------------
;; END of confiugration
;; -----------------------------------------------------------------------------

(provide 'init)

;; -----------------------------------------------------------------------------
;; end of this file
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
       (emmet-mode tide markdown-mode js-comint expand-region nyan-mode company exec-path-from-shell helm evil browse-at-remote yasnippet smex magit color-theme-solarized ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


