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

  ;; (add-to-list 'browse-at-remote-remote-type-domains '("192.168.0.202" . "gitlab2"))
  ;; (add-to-list 'browse-at-remote-remote-type-domains '("mtiisl.cn" . "gitlab3"))
  ;; (add-to-list 'browse-at-remote-remote-type-domains '("minix.jeanhwea.io" . "gitlab3"))

  ;; (defun browse-at-remote--format-commit-url-as-gitlab2 (repo-url commithash)
  ;;   "commit url formatted for gitlab2."
  ;;   (let
  ;;     ((repo-url
  ;;        (jh/re-replace "^https://" "http://" repo-url)))
  ;;     (browse-at-remote--format-commit-url-as-gitlab repo-url commithash)))

  ;; (defun browse-at-remote--format-region-url-as-gitlab2
  ;;   (repo-url location filename &optional linestart lineend)
  ;;   "url formatted for gitlab2."
  ;;   (let
  ;;     ((repo-url
  ;;        (jh/re-replace "^https://" "http://" repo-url)))
  ;;     (browse-at-remote--format-region-url-as-gitlab
  ;;       repo-url location filename linestart lineend)))

  ;; (defun browse-at-remote--format-commit-url-as-gitlab3 (repo-url commithash)
  ;;   "commit url formatted for gitlab3."
  ;;   (let
  ;;     ((repo-url
  ;;        (jh/re-replace
  ;;          "^\\(https://\\)\\([^/]+\\)\\(.+\\)$"
  ;;          "http://\\2/gitlab\\3" repo-url)))
  ;;     (browse-at-remote--format-commit-url-as-gitlab repo-url commithash)))

  ;; (defun browse-at-remote--format-region-url-as-gitlab3
  ;;   (repo-url location filename &optional linestart lineend)
  ;;   "url formatted for gitlab3."
  ;;   (let
  ;;     ((repo-url
  ;;        (jh/re-replace
  ;;          "^\\(https://\\)\\([^/]+\\)\\(.+\\)$"
  ;;          "http://\\2/gitlab\\3" repo-url)))
  ;;     (if (string-match-p "\\.\\(md\\|org\\)$" filename)
  ;;       (browse-at-remote--format-region-url-as-gitlab
  ;;         repo-url location filename)
  ;;       (browse-at-remote--format-region-url-as-gitlab
  ;;         repo-url location filename linestart lineend))))
  )

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
