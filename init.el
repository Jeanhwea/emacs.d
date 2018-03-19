;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; -----------------------------------------------------------------------------
;; BEGIN: Bootstrap confiugration
;; -----------------------------------------------------------------------------

;; normal setup
(require 'init-site-lisp)
(require 'init-elpa)
(require 'init-startup)
(require 'init-gui)
(require 'init-themes)

;; plugin
(require 'init-magit)
(require 'init-smex)
(require 'init-yasnippet)
(require 'init-helm)

;; -----------------------------------------------------------------------------
;; END: Bootstrap confiugration
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
 '(package-selected-packages
   (quote
    (helm evil browse-at-remote yasnippet smex magit color-theme-solarized ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
