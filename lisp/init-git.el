(when (and (require 'magit) (require 'magit-todos))
  ;; repositories for magit-list-repositories
  ;; (setq
  ;;   magit-repository-directories `((,user-emacs-directory . 0)))
  (add-to-list 'magit-repository-directories '("~/work" . 2))
  (cond
    ((jh/mac?) (add-to-list 'magit-repository-directories '("~/Code" . 3)))
    ((jh/linux?) (add-to-list 'magit-repository-directories '("~/code" . 3)))
    ((jh/windows?) (add-to-list 'magit-repository-directories '("e:/Code" . 3))))
  ;; (require 'magit-lfs)
  (defalias 'list-repositories 'magit-list-repositories)
  (magit-todos-mode)
  (global-set-key (kbd "C-x g") 'magit-status))


;; -----------------------------------------------------------------------------
;; browse-at-remote
;; -----------------------------------------------------------------------------
(when (require 'browse-at-remote)

  (add-to-list 'browse-at-remote-remote-type-regexps '("^mtiisl\\.cn/gitlab$" . "gitlab"))
  (add-to-list 'browse-at-remote-remote-type-regexps '("^192\\.168\\.0\\.202$" . "gitlab"))

  (defadvice browse-at-remote--get-url-from-remote
    (after mtiisl-gitlab activate)
    (let ((domain (car ad-return-value)) (url (cdr ad-return-value)))
      ;; force use http for 202
      (when (string-match-p ".*192.168.0.202.*" url)
        (setq url (jh/re-replace "^https" "http" url)))
      ;; force add '/gitlab' sub-path for dev58
      (setq ad-return-value
        (cons
          (jh/re-replace "mtiisl.cn" "mtiisl.cn/gitlab" domain)
          (jh/re-replace "mtiisl.cn" "mtiisl.cn/gitlab" url)))))
  )

;; -----------------------------------------------------------------------------
;; git-auto-commit
;; -----------------------------------------------------------------------------
(when (require 'git-auto-commit-mode)
  (setq-default gac-automatically-push-p t))

(when (require 'git-msg-prefix)
  (add-hook 'git-commit-mode-hook 'git-msg-prefix))

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
