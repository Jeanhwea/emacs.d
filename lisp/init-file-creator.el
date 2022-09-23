(defconst project/golang
  '((repository . "app/repository/{}_repo/{}_repo.go")
     (helper . "app/helper/{}_helper/{}_helper.go")
     (service . "app/service/{}_service/{}_service.go")
     (controller . "app/controller/{}_controller/{}_controller.go")))

(defun create-new-component ()
  "Create new project component."
  (interactive)
  (let*
    ((default-directory (jh/git-root (buffer-file-name)))
      (comp (completing-read "Component Category >> " project/golang nil t "^"))
      (to (assoc (intern comp) project/golang))
      (name (read-string "Component Name >> "))
      (path (jh/re-replace "{}" name (cdr to))))
    (progn
      (find-file path)
      (message "Create `%s'" path))))

(provide 'init-file-creator)
