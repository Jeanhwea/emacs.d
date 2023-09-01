(setq-default
  dired-listing-switches "-al -X -v --group-directories-first"
  dired-dwim-target t)

(add-hook 'after-init-hook
  #'(lambda ()
      (setq dired-recursive-deletes 'top)


      (define-key dired-mode-map (kbd "C-c w") 'wdired-change-to-wdired-mode)))

;; 忽略一些不需要关注的文件
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))


;; (when (require 'dired-k)
;;   (setq dired-k-style 'git)
;;   (add-hook 'dired-initial-position-hook 'dired-k)
;;   (add-hook 'dired-after-readin-hook #'dired-k-no-revert))

(provide 'init-dired)
