;; load all the path in ~/.emacs.d/site-path

(eval-when-compile (require 'cl))

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

(provide 'init-site-lisp)
