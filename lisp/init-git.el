(when (require 'magit)
  ;; repositories for magit-list-repositories
  (setq
    magit-repository-directories `((,user-emacs-directory . 0)))
  (if (jh/windows?)
    (add-to-list 'magit-repository-directories '("e:/Code" . 3))
    (add-to-list 'magit-repository-directories '("~/Code" . 3)))
  (global-set-key (kbd "C-x g") 'magit-status))

;; -----------------------------------------------------------------------------
;; browse-at-remote
;; -----------------------------------------------------------------------------
(when (require 'browse-at-remote)
  (add-to-list
    'browse-at-remote-remote-type-domains '("192.168.0.202" . "gitlab2"))
  (add-to-list
    'browse-at-remote-remote-type-domains '("dev58.mti.avic.com" . "gitlab3"))
  (add-to-list
    'browse-at-remote-remote-type-domains '("minix.jeanhwea.io" . "gitlab3"))

  (defun browse-at-remote--format-commit-url-as-gitlab2 (repo-url commithash)
    "commit url formatted for gitlab2."
    (let
      ((repo-url
         (jh/re-replace "^https://" "http://" repo-url)))
      (browse-at-remote--format-commit-url-as-gitlab repo-url commithash)))

  (defun browse-at-remote--format-region-url-as-gitlab2
    (repo-url location filename &optional linestart lineend)
    "url formatted for gitlab2."
    (let
      ((repo-url
         (jh/re-replace "^https://" "http://" repo-url)))
      (browse-at-remote--format-region-url-as-gitlab
        repo-url location filename linestart lineend)))

  (defun browse-at-remote--format-commit-url-as-gitlab3 (repo-url commithash)
    "commit url formatted for gitlab3."
    (let
      ((repo-url
         (jh/re-replace
           "^\\(https://\\)\\([^/]+\\)\\(.+\\)$"
           "http://\\2/gitlab\\3" repo-url)))
      (browse-at-remote--format-commit-url-as-gitlab repo-url commithash)))

  (defun browse-at-remote--format-region-url-as-gitlab3
    (repo-url location filename &optional linestart lineend)
    "url formatted for gitlab3."
    (let
      ((repo-url
         (jh/re-replace
           "^\\(https://\\)\\([^/]+\\)\\(.+\\)$"
           "http://\\2/gitlab\\3" repo-url)))
      (browse-at-remote--format-region-url-as-gitlab
        repo-url location filename linestart lineend))))

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
