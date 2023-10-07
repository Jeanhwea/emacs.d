;; -----------------------------------------------------------------------------
;; initialize emacs package system
;; -----------------------------------------------------------------------------
(setq byte-compile-warnings '(cl-functions))
(require 'cl-lib)

(when (require 'package)
  ;; Add MELPA
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

  ;; Use TUNA mirror
  ;;(setq package-archives
  ;;  '(("gnu-tuna"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
  ;;     ("nongnu-tuna" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
  ;;     ("melpa-tuna"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))


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
