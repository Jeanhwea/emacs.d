(when (require 'package)
  ;; using tsinghua mirror as default source
  (setq package-archives
    '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
	     ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
  ;; must initialize package first
  (package-initialize))

(provide 'init-elpa)
