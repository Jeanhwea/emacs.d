(when (require 'yasnippet)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (setq yas-indent-line 'auto)
  (yas-global-mode 1))

(provide 'init-yasnippet)
