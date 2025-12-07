(setq-default
  dired-sort-defaults (list 'name)
  dired-listing-switches "-l -a -v --group-directories-first"
  dired-dwim-target t)

;; (when (jh/bsd?)
;;   (setq dired-listing-switches "-l -a -v --group-directories-first")
;;   (setenv "LC_COLLATE" "C"))

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
