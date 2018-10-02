(when (require 'company)
  (add-hook 'after-init-hook 'global-company-mode)

  ;; https://emacs.stackexchange.com/questions/10837/how-to-make-company-mode-be-case-sensitive-on-plain-text/10838
  ;; https://github.com/company-mode/company-mode/issues/14
  (setq company-dabbrev-downcase nil)

  ;; add normal keybindings
  (define-key company-mode-map (kbd "M-/") 'company-complete)
  (define-key company-active-map (kbd "M-/") 'company-other-backend)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)

  ;; set a global keybindings for more help
  (global-set-key (kbd "M-C-/") 'company-complete)

  (setq-default company-dabbrev-other-buffers 'all
    company-tooltip-align-annotations t))

(provide 'init-company)
