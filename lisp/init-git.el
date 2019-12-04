(when (require 'magit)
  ;; repositories for magit-list-repositories
  (setq
    magit-repository-directories `((,user-emacs-directory . 0)))
  (when (jh/mac?)
    (add-to-list 'magit-repository-directories '("~/Code" . 3)))
  (when (jh/windows?)
    (add-to-list 'magit-repository-directories '("e:/Code" . 3)))
  (global-set-key (kbd "C-x g") 'magit-status))


;; -----------------------------------------------------------------------------
;; browse-at-remote
;; -----------------------------------------------------------------------------
(when (require 'browse-at-remote)
  (add-to-list 'browse-at-remote-remote-type-domains '("192.168.0.202" . "avic"))

  (defun browse-at-remote--format-region-url-as-avic (repo-url location filename &optional linestart lineend)
    "url formatted for avic."
    (let
      ((avic-repo-url
         (replace-regexp-in-string "^https://" "http://" repo-url)))
      (cond
        ((and linestart lineend)
          (format "%s/blob/%s/%s#L%d-%d" avic-repo-url location filename linestart lineend))
        (linestart (format "%s/blob/%s/%s#L%d" avic-repo-url location filename linestart))
        (t (format "%s/tree/%s/%s" avic-repo-url location filename)))))

  (defun browse-at-remote--format-commit-url-as-avic (repo-url commithash)
    "commit url formatted for avic."
    (message (format "%s/commit/%s" repo-url commithash))
    (let
      ((avic-repo-url
         (replace-regexp-in-string "^https://" "http://" repo-url)))
      (format "%s/commit/%s" avic-repo-url commithash))))

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
