(when (require 'cuda-mode)
  (add-to-list 'auto-mode-alist '("\\.cu.cc\\'" . cuda-mode)))

(provide 'init-cc)
