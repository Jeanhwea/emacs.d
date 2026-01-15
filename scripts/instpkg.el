(message (emacs-version))

(setq mini-version "29.1")
(when (version< emacs-version mini-version)
  (error "This config supported mini version is v%s" mini-version))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-package)
(setq package-selected-packages my-packages)
(package-refresh-contents)
(package-install-selected-packages t)
