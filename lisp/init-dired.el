(setq-default dired-dwim-target t)

(add-hook 'after-init-hook
  #'(lambda ()
      (setq dired-recursive-deletes 'top)

      ;; 忽略一些不需要关注的文件
      (dired-omit-mode 1)

      (define-key dired-mode-map (kbd "C-c w") 'wdired-change-to-wdired-mode)))

;; (when (require 'dired-k)
;;   (setq dired-k-style 'git)
;;   (add-hook 'dired-initial-position-hook 'dired-k)
;;   (add-hook 'dired-after-readin-hook #'dired-k-no-revert))

(provide 'init-dired)
