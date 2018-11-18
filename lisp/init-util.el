;; -----------------------------------------------------------------------------
;; system predictor
;; -----------------------------------------------------------------------------
(defun jh/windows? ()
  "test if system-type is windows?"
  (string-equal "windows-nt" system-type))
(defun jh/mac? ()
  "test if system-type is mac?"
  (string-equal "darwin" system-type))


;; -----------------------------------------------------------------------------
;; git repository related helper
;; -----------------------------------------------------------------------------
(defun jh/git-root-dir ()
  "Get the root directory of a Git repository."
  (replace-regexp-in-string "\n" ""
    (expand-file-name
      (shell-command-to-string
        "git rev-parse --show-cdup"))))

(defun jh/git-file-name ()
  "Get the relative filename of a file in Git repository"
  (if buffer-file-name
    (replace-regexp-in-string (jh/git-root-dir) ""
      (expand-file-name buffer-file-name)) nil))

(provide 'init-util)
