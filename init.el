;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; -----------------------------------------------------------------------------
;; BEGIN: Bootstrap confiugration
;; -----------------------------------------------------------------------------

(require 'init-package)
(require 'init-startup)
(require 'init-gui)

;; plugin
(require 'init-magit)
(require 'init-smex)
(require 'init-yasnippet)

;; -----------------------------------------------------------------------------
;; END: Bootstrap confiugration
;; -----------------------------------------------------------------------------

(provide 'init)

;; -----------------------------------------------------------------------------
;; end of this file
;; -----------------------------------------------------------------------------

