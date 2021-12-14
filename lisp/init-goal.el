;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; golang project manager should given a proper name, let's call it GOAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst goal/files
  '((class . "{}.go")
     (helper . "{}_helper.go")
     (implement . "{}_impl.go")
     (test . "{}_test.go"))
  "A golang related file alist, use `{}' represent TOPIC.")

(defun goal/get-file-topic (file pattern)
  "Test if the file matches pattern, and return topic if found."
  (let*
    ((topic)
      (topic-regexp "\\\\([_a-zA-Z0-9]+\\\\)")
      (regexp (concat "^.*/" (jh/re-replace "{}" topic-regexp pattern) "$")))
    (or file (user-error "Ops: Test file is nil."))
    (save-match-data
      (and (string-match regexp file)
        (setq topic (match-string 1 file))))
    topic))

(defun goal/files-get (&optional file)
  "Get the first element that matches goal/files."
  (let
    ((file (or file (buffer-file-name)))
      (pred #'(lambda (e) (goal/get-file-topic file (cdr e)))))
    (car (remove-if-not pred goal/files))))

(defun goal/get-related-topic-file (file from to)
  "Goto related topic file."
  (let*
    ((pred #'(lambda (e) (goal/get-file-topic file (cdr e))))
      (topic (car (remove-if #'null (mapcar pred goal/files))))
      (suffix (jh/re-replace "{}" topic (cdr to)))
      (prefix (jh/re-replace (concat (jh/re-replace "{}" topic (cdr from)) "$") "" file)))
    (concat prefix suffix)))

(defun goal/find-the-new-place (where &optional file)
  "Return the destination filename."
  (let*
    ((file (or file (buffer-file-name)))
      (from (goal/files-get file))
      (to (assoc where goal/files)))
    (or (string-match-p ".go$" file)
      from (user-error "Ops: Cannot get any information about this file."))
    (or to (user-error "Ops: Missing place to go."))
    ;; do the find work
    (goal/get-related-topic-file file from to)))

(defun goal/switch-to (&optional where file)
  "Switch to a new type file based on file."
  (interactive)
  (let*
    ((file (or file (buffer-file-name)))
      (where (completing-read "Switch to >> " goal/files nil t "^"))
      (dest (goal/find-the-new-place (intern where) file)))
    (progn
      (find-file dest)
      (message "Switched to `%s'" dest))))

(provide 'init-goal)
