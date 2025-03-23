;; -----------------------------------------------------------------------------
;; A reasonable Emacs configuration files.
;; -----------------------------------------------------------------------------
(setq-default
  user-full-name "Jinghui Hu"
  user-mail-address "hujinghui@buaa.edu.cn"
  debug-on-error t)

;; -----------------------------------------------------------------------------
;; local settings
;; -----------------------------------------------------------------------------
(let ((pre-local-settings "~/.emacs.pre.el"))
  (when (file-exists-p pre-local-settings) (load-file pre-local-settings)))

;; setup local file
(defconst jh/init-file
  (expand-file-name "user-init.el" user-emacs-directory))
(defconst jh/cust-file
  (expand-file-name "user-custom.el" user-emacs-directory))

(when (file-exists-p jh/init-file)
  (setq-default user-init-file jh/init-file))
(when (file-exists-p jh/cust-file)
  (setq-default custom-file jh/cust-file))


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
(require 'init-wsl)

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
(require 'init-asm)
(require 'init-cc)
(require 'init-csharp)
(require 'init-css)
(require 'init-csv)
(require 'init-dot)
(require 'init-file-creator)
(require 'init-go)
(require 'init-goal)
(require 'init-groovy)
(require 'init-html)
(require 'init-java)
(require 'init-json)
(require 'init-nxml)
(require 'init-thrift)
(require 'init-lisp)
(require 'init-pascal)
(require 'init-python)
(require 'init-puml)
(require 'init-rust)
(require 'init-sh)
(require 'init-sql)
(require 'init-matlab)
(require 'init-web)

;; -----------------------------------------------------------------------------
;; misc
;; -----------------------------------------------------------------------------
(require 'init-ffmpeg)
(require 'init-leetcode)
(require 'init-apidocs)
(require 'init-experimental)
(require 'init-keymate)
(require 'init-debug)

;; -----------------------------------------------------------------------------
;; local settings
;; -----------------------------------------------------------------------------
(let ((local-settings "~/.emacs.local.el"))
  (when (file-exists-p local-settings) (load-file local-settings)))

(setq package-selected-packages my-packages)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

(provide 'init)
