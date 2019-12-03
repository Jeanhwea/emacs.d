(when (require 'magit)

  ;; repositories for magit-list-repositories
  (setq
    magit-repository-directories `((,user-emacs-directory . 0)))

  (when (jh/mac?)
    (add-to-list 'magit-repository-directories '("~/Code" . 3)))
  (when (jh/windows?)
    (add-to-list 'magit-repository-directories '("e:/Code" . 3)))

  ;; keybinding
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x f") 'magit-blame))


;; -----------------------------------------------------------------------------
;; browse-at-remote
;; -----------------------------------------------------------------------------
(when (require 'browse-at-remote)
  (global-set-key (kbd "C-c b") 'bar-browse))

;; -----------------------------------------------------------------------------
;; some helper function with git repository
;; -----------------------------------------------------------------------------
(defun jh/git-project-root-dir-p (dir)
  "Return ture if DIR is contains `.git'."
  (file-directory-p
    (directory-file-name
      (expand-file-name ".git" dir))))

(defun jh/git-project-root-dir (dir)
  "Return the root directory of a git repository."
  (let ((dirs
          (remove-if-not #'jh/git-project-root-dir-p
            (jh/directory-sequence dir))))
    (unless (null dirs) (car dirs))))

(defun jh/git-relative-filename (file)
  "Return a git file name relative to git root directory."
  (let ((git-root (jh/git-project-root-dir (jh/parent-dir file))))
    (jh/relative-path file git-root)))

(provide 'init-git)
