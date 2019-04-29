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

;; -----------------------------------------------------------------------------
;; setup timer
;; -----------------------------------------------------------------------------
(defun jh/run-at-time-if-possible (timestr func)
  "If timestr is not out of date, then setup a callback function"
  (let ((now (current-time))
         (start (date-to-time (format-time-string (concat "%Y-%m-%d " timestr)))))
    (when (time-less-p now start)
      (run-at-time timestr nil func))))

(provide 'init-util)
