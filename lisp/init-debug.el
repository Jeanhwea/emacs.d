(setq-default
  compilation-scroll-output t
  compilation-ask-about-save nil)

;; compilation line wrap by default
(add-hook 'compilation-mode-hook (lambda () (visual-line-mode 1)))
(add-hook 'compilation-minor-mode-hook (lambda () (visual-line-mode 1)))

;; setup lldb on mac
(when (jh/mac?) (require 'gud-lldb))

(provide 'init-debug)
