(setq-default dired-dwim-target t)

(add-hook 'after-init-hook
  #'(lambda ()
      (setq dired-recursive-deletes 'top)
      (define-key dired-mode-map (kbd "C-c w") 'wdired-change-to-wdired-mode)))

(provide 'init-dired)
