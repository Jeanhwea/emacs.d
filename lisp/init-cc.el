(when (require 'cuda-mode)
  (add-to-list 'auto-mode-alist '("\\.cu?\\'" . cuda-mode)))

(provide 'init-cc)
