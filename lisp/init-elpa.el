;; -----------------------------------------------------------------------------
;; initialize emacs package system
;; -----------------------------------------------------------------------------
(defun jh/install-packages-from-elpa ()
  "Install all packages from ELPA."
  (interactive)
  (package-refresh-contents)
  (package-install-selected-packages))


(when (require 'package)
  ;; using tsinghua mirror as default source
  (setq package-archives
    '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
       ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

  ;; must initialize package first
  (package-initialize))


(provide 'init-elpa)
