(defconst project/golang
  '((repository . "biz/repository/{}_repo/{}_repo.go")
     (helper . "biz/helper/{}_helper/{}_helper.go")
     (service . "biz/service/{}_service/{}_service.go")
     (controller . "biz/controller/{}_controller/{}_controller.go")))

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
