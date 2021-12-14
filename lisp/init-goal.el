;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; golang project manager should given a proper name, let's call it GOAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst goal/topics
  '((class . "{}.go")
     (helper . "{}_helper.go")
     (implement . "{}_impl.go")
     (test . "{}_test.go"))
  "A golang related file alist, use `{}' represent TOPIC.")

(defun goal/get-file-topic (file pattern)
  "Test if the file matches pattern, and return topic if found.
       mysql_client.go      => mysql_client
       mysql_client_impl.go => mysql_client
       mysql_client_test.go => mysql_client
"
  (let*
    ((topic)
      (name-regexp "\\\\([_a-zA-Z0-9]+\\\\)")
      (regexp (concat "^.*/" (jh/re-replace "{}" name-regexp pattern) "$")))
    (or file (user-error "Ops: Test file is nil."))
    (save-match-data
      (and (string-match regexp file) (setq topic (match-string 1 file))))
    topic))

(defun goal/get-file-entry (&optional file)
  "Get the first entry that matches goal/topics. => (test . \"{}_test.go\") "
  (let
    ((file (or file (buffer-file-name)))
      (entries #'(lambda (e) (goal/get-file-topic file (cdr e)))))
    (car (remove-if-not entries goal/topics))))

(defun goal/find-the-new-place (where &optional file)
  "Return the destination filename."
  (let*
    ((file (or file (buffer-file-name)))
      (from (goal/get-file-entry file))
      (to (assoc (intern where) goal/topics))
      (topic (car (remove-if #'null (mapcar entry goal/topics))))
      (dir (file-name-directory file))
      (dest (jh/re-replace "{}" topic (cdr to))))
    (or (string-match-p ".go$" file)
      from (user-error "Ops: Cannot get any information about this file."))
    (or to (user-error "Ops: Missing place to go."))
    (concat dir dest)))

(defun goal/switch-to (&optional where file)
  "Switch to a new type file based on file."
  (interactive)
  (let*
    ((file (or file (buffer-file-name)))
      (where (completing-read "Switch to >> " goal/topics nil t "^"))
      (dest (goal/find-the-new-place where file)))
    (progn
      (find-file dest)
      (message "Switched to `%s'" dest))))

(provide 'init-goal)
