;; -----------------------------------------------------------------------------
;; initialize emacs package system
;; -----------------------------------------------------------------------------
(when (require 'package)
  ;; using tsinghua mirror as default source
  (setq package-archives
    '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
       ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

  ;; must initialize package first
  (package-initialize))

(eval-when-compile (require 'cl))

;; load all the path in ~/.emacs.d/site-lisp
(defun jh/add-subdirs-to-load-path (parent-dir)
  "Adds every non-hidden subdir of PARENT-DIR to `load-path'."
  (let* ((default-directory parent-dir))
    (progn
      (setq load-path
        (append
          (remove-if-not
            (lambda (dir) (file-directory-p dir))
            (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
          load-path)))))

(jh/add-subdirs-to-load-path (expand-file-name "site-lisp/" user-emacs-directory))

(defun jh/install-packages-from-elpa ()
  "Install all packages from ELPA."
  (interactive)
  (package-refresh-contents)
  (package-install-selected-packages))

(provide 'init-package)
