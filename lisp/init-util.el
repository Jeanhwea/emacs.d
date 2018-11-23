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
;; helper function for setup timer. use as following example
;;
;;   (jh/set-timer-if-possible "10:50:00"
;;     (lambda () (load-theme 'kill-emacs t)))
;; -----------------------------------------------------------------------------
(defun jh/current-seconds-to (timestr)
  "Current seconds to specific timestr is `%H:%M:%S' like string"
  (setq from (cadr (current-time)))
  (setq to
    (cadr (date-to-time
            (format-time-string (concat "%Y-%m-%d " timestr)))))
  (- to from))

(defun jh/set-timer-if-possible (timestr func)
  "If timestr is not out of date, then setup a callback function"
  (when (> (jh/current-seconds-to timestr) 0)
    (run-at-time (jh/current-seconds-to timestr) nil func)))



(provide 'init-util)
