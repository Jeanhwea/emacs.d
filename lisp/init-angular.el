;; -----------------------------------------------------------------------------
;;     _                      _
;;    / \   _ __   __ _ _   _| | __ _ _ __
;;   / △ \ | '_ \ / _` | | | | |/ _` | '__|
;;  / ___ \| | | | (_| | |_| | | (_| | |
;; /_/   \_\_| |_|\__, |\__,_|_|\__,_|_|
;;                |___/
;; -----------------------------------------------------------------------------
;; Angular and Typescript related files
(defconst ng/files
  '((test . "{}.spec.ts")
     (model . "{}.ts")
     (view . "{}.html")
     (style . "{}.less"))
  "A Angular component files structure.")

(defun ng/files-match (file pattern)
  "Test if the file matches pattern."
  (let*
    ((idgrp "\\\\([_a-zA-Z0-9.]+\\\\)" )
      (regexp (concat "^.*/" (jh/re-replace "{}" idgrp pattern) "$"))
      (topic))
    (or file (user-error "Ops: Test file is nil."))
    (save-match-data
      (and (string-match regexp file) (setq topic (match-string 1 file))))
    topic))

(defun ng/files-get (&optional file)
  "Get the first element that matches ng/files."
  (let
    ((file (or file (buffer-file-name)))
      (pred #'(lambda (e) (ng/files-match file (cdr e)))))
    (car (remove-if-not pred ng/files))))

(defun ng/goto-related-topic-file (file from to)
  "Goto related topic file."
  (let*
    ((pred #'(lambda (e) (ng/files-match file (cdr e))))
      (topic (car (remove-if #'null (mapcar pred ng/files))))
      (suffix (jh/re-replace "{}" topic (cdr to)))
      (prefix
        (jh/re-replace
          (concat (jh/re-replace "{}" topic (cdr from)) "$") "" file)))
    (concat prefix suffix)))

(provide 'init-angular)
