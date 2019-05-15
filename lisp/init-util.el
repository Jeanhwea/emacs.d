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
;; file and directory helper
;; -----------------------------------------------------------------------------
(defun jh/absolute-path (dir)
  "Return absolute path of DIR."
  (directory-file-name (expand-file-name dir)))

(defun jh/relative-path (file dir)
  "Return a relative path of FILE to DIR."
  (let ((absolute-file (jh/absolute-path file))
         (absolute-dir (concat (jh/absolute-path dir) "/")))
    (message absolute-dir)
    (replace-regexp-in-string absolute-dir "" absolute-file)))

(defun jh/parent-dir (dir)
  "Return the parent directory of DIR."
  (directory-file-name
    (file-name-directory
      (directory-file-name (expand-file-name dir)))))

(defun jh/root-dir-p (dir)
  "Return ture if DIR is a root directory"
  (let ((path (jh/absolute-path dir)))
    (string-equal path (jh/parent-dir path))))

(defun jh/filename-without-extension (&optional file)
  "Return the file name without extension."
  (file-name-nondirectory
    (file-name-sans-extension (or file (buffer-file-name)))))

;; -----------------------------------------------------------------------------
;; git repository related helper
;; -----------------------------------------------------------------------------
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
