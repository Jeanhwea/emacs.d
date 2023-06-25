(when (require 'magit)
  ;; repositories for magit-list-repositories
  ;; (setq
  ;;   magit-repository-directories `((,user-emacs-directory . 0)))
  (add-to-list 'magit-repository-directories '("~/work" . 2))
  (cond
    ((jh/mac?) (add-to-list 'magit-repository-directories '("~/code" . 3)))
    ((jh/linux?) (add-to-list 'magit-repository-directories '("~/code" . 3)))
    ((jh/windows?) (add-to-list 'magit-repository-directories '("e:/Code" . 3))))
  ;; (require 'magit-lfs)
  (defalias 'list-repositories 'magit-list-repositories)
  (global-set-key (kbd "C-x g") 'magit-status))

(when (require 'magit-todos)
  (magit-todos-mode))


;; -----------------------------------------------------------------------------
;; browse-at-remote
;; -----------------------------------------------------------------------------
(when (require 'browse-at-remote)
  (add-to-list 'browse-at-remote-remote-type-regexps '(:host "^192\\.168\\.0\\.202$" :type "gitlab"))
  (add-to-list 'browse-at-remote-remote-type-regexps '(:host "^mtiisl\\.cn$" :type "gitlab"))
  (add-to-list 'browse-at-remote-remote-type-regexps '(:host "^gitana\\.jeanhwea\\.io$" :type "gitlab"))

  ;; 增强 gitana 远端调用
  (defadvice browse-at-remote--format-region-url-as-gitlab
    (around browse-at-remote--format-region-url-as-gitlab-around activate)

    ;; 调用函数
    ad-do-it
    ;; 修改返回值
    (setq ad-return-value
      (jh/re-replace "^https://mtiisl.cn" "http://mtiisl.cn/gitlab" ad-return-value))
    (setq ad-return-value
      (jh/re-replace "^https://192.168.0.202" "http://192.168.0.202" ad-return-value))
    (setq ad-return-value
      (jh/re-replace "^https://gitana.jeanhwea.io" "http://gitana.jeanhwea.io" ad-return-value)))

  ;; END
  )

;; -----------------------------------------------------------------------------
;; git-auto-commit
;; -----------------------------------------------------------------------------
(when (require 'git-auto-commit-mode)
  (setq-default gac-automatically-push-p t))

;; (when (require 'git-msg-prefix)
;;   (add-hook 'git-commit-mode-hook 'git-msg-prefix))

;; -----------------------------------------------------------------------------
;; some helper function with git repository
;; -----------------------------------------------------------------------------
(defun jh/git-root-p (dir)
  "Return ture if DIR is contains `.git'."
  (file-directory-p
    (directory-file-name
      (expand-file-name ".git" dir))))

(defun jh/git-root (dir)
  "Return the root directory of a git repository."
  (let ((dirs
          (remove-if-not #'jh/git-root-p
            (jh/directory-sequence dir))))
    (unless (null dirs) (car dirs))))

(defun jh/git-relative-filename (file)
  "Return a git file name relative to git root directory."
  (let ((git-root (jh/git-root (jh/parent-dir file))))
    (jh/relative-path file git-root)))

(provide 'init-git)
