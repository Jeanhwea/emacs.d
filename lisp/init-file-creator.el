(defconst project/golang
  '((repository . "app/repository/{}_repo/{}_repo.go")
     (helper . "app/helper/{}_helper/{}_helper.go")
     (infrastructure . "app/infrastructure/{}_infra/{}_infra.go")
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

(defun jh/find-alternate-file()
  "Switch between source file and header file (e.g., .cpp <-> .h)."
  (interactive)
  (let ((ff-always-try-to-create nil))
    (ff-find-other-file nil)))

(provide 'init-file-creator)
