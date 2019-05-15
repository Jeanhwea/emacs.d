(defun spt/find-file (file)
  "Open a FILE."
  (find-file file))

(defun spt/project-root (&optional file)
  "Return current project root dir."
  (jh/git-project-root-dir-from-file file))

(defun spt/flatten-dirs-list (dirs-list)
  "flatten a nested list."
  (let (value)
    (dolist (elt dirs-list value)
      (setq value (concatenate 'list value elt)))))

(defun spt/java-files (dir)
  "Return a list of `*.java' files."
  (directory-files-recursively dir "^.*\.java$"))

(defun spt/directory-files (dirs)
  "Return a list of interested files."
  (spt/flatten-dirs-list (mapcar #'spt/java-files dirs)))

(defun spt/file-name-to-class-name (&optional file)
  "Return the class name from a file name."
  (jh/filename-without-extension file))

(defun spt/file-name-to-entity-name (file)
  "Return the entity name from a file name."
  (when (string-match-p ".*\\(controller\\|entity\\|repo\\|service\\)/.*\\.java" file)
    (jh/pascalcase
      (replace-regexp-in-string
        "\\(RepositoryImpl\\|ServiceImpl\\|Repository\\|Service\\|Controller\\)$"
        "" (spt/file-name-to-class-name file)))))

(defun spt/file-name-to-implement-name (file)
  "Return the implement name from a file name."
  (let ((class (spt/file-name-to-class-name file)))
    (when (string-match-p "^.*\\(Repository\\|Service\\)$" class)
      (concat class "Impl"))))

(defvar spt/all-components-cache (make-hash-table :test 'equal)
  "Cached all components in this project.")

(defvar spt/all-implements-cache (make-hash-table :test 'equal)
  "Cached all implements in this project.")

(defun spt/scan-all-components (dirs)
  "Return a list that contains all components in the project."
  (progn
    (clrhash spt/all-components-cache)
    (dolist (file (spt/directory-files dirs))
      (let ((class (spt/file-name-to-class-name file)))
        (unless (null (spt/file-name-to-entity-name file))
          (puthash class file spt/all-components-cache))))))

(defun spt/scan-all-implements (dirs)
  "Return a list that contains all implements in the project."
  (progn
    (clrhash spt/all-implements-cache)
    (dolist (file (spt/directory-files dirs))
      (let ((class (spt/file-name-to-class-name file)))
        (when (string-match-p "^.*Impl$" class)
          (puthash class file spt/all-implements-cache))))))

(defun spt/trans-file-name (file path formula)
  "Transfer file to given relative path, with a formula, like `%sRepository.java'."
  (let ((entity (spt/file-name-to-entity-name file))
         (folder (expand-file-name path (jh/parent-dir file))))
    (expand-file-name (replace-regexp-in-string "%s" entity formula) folder)))

(defun spt/switch-to-component-file (path formula)
  "Switch to a component file in the project."
  (let ((entity (spt/file-name-to-entity-name (buffer-file-name))))
    (progn
      (spt/scan-all-components (spt/maven-project-source-dirs))
      (unless (null entity)
        (spt/find-file
          (spt/trans-file-name
            (gethash entity spt/all-components-cache) path formula))))))

(defun spt/switch-to-entity-file ()
  "Switch to entity file."
  (interactive)
  (let ((entity (spt/file-name-to-entity-name (buffer-file-name))))
    (progn
      (spt/scan-all-components (spt/maven-project-source-dirs))
      (unless (null entity)
        (spt/find-file (gethash entity spt/all-components-cache))))))

(defun spt/switch-to-repository-file ()
  "Switch to repository file."
  (interactive)
  (spt/switch-to-component-file "../repo" "%sRepository.java"))

(defun spt/switch-to-service-file ()
  "Switch to service file."
  (interactive)
  (spt/switch-to-component-file "../../service" "%sService.java"))

(defun spt/switch-to-controller-file ()
  "Switch to controller file."
  (interactive)
  (spt/switch-to-component-file "../../controller" "%sController.java"))

(defun spt/swap-interface-and-implemention (file)
  "Swap interface and implemention name."
  (let ((class (spt/file-name-to-class-name file)))
    (cond
      ((string-match-p "^.*Impl$" class)
        (expand-file-name
          (concat (replace-regexp-in-string "Impl$" "" class) ".java")
          (jh/parent-dir (jh/parent-dir file))))
      ((string-match-p "^.*\\(Repository\\|Service\\)$" class)
        (expand-file-name
          (concat class "Impl.java")
          (expand-file-name "impl" (jh/parent-dir file))))
      (t nil))))

(defun spt/toggle-interface-and-implemention ()
  "Toggle interface file and implemention."
  (interactive)
  (spt/find-file (spt/swap-interface-and-implemention (buffer-file-name))))

;; (mapcar #'spt/file-name-to-class-name (spt/directory-files '("~/Code/work/avic/skree/src")))
;; (mapcar #'spt/file-name-to-entity-name (spt/directory-files '("~/Code/work/avic/skree/src")))
;; (spt/file-name-to-entity-name (car (spt/directory-files '("~/Code/work/avic/skree/src"))))
;; (spt/scan-all-components '("~/Code/work/avic/skree/src"))
;; (spt/scan-all-implements '("~/Code/work/avic/skree/src"))
;; (spt/scan-all-components nil)
;; (gethash "Employee" spt/all-components-cache)
;; (find-file (gethash "Employee" spt/all-components-cache))
;; (spt/trans-file-name (gethash "Employee" spt/all-components-cache) "../repo" "%sRepository.java")
;; (print spt/all-components-cache)
;; (print spt/all-implements-cache)

(defun spt/maven-project-source-dirs ()
  "Return the maven project source folder."
  (list (expand-file-name "src" (spt/project-root))))

;; (spt/maven-project-source-dirs)

;; -----------------------------------------------------------------------------
;; key bindings
;; -----------------------------------------------------------------------------
(progn
  (define-prefix-command 'spt/leader-key-map)
  (define-key spt/leader-key-map (kbd "c") 'spt/switch-to-controller-file)
  (define-key spt/leader-key-map (kbd "i") 'spt/toggle-interface-and-implemention)
  (define-key spt/leader-key-map (kbd "r") 'spt/switch-to-repository-file)
  (define-key spt/leader-key-map (kbd "s") 'spt/switch-to-service-file)
  (define-key spt/leader-key-map (kbd "t") 'projectile-toggle-between-implementation-and-test)
  (define-key spt/leader-key-map (kbd "RET") 'jh/java-try-import-class)
  (define-key spt/leader-key-map (kbd "e") 'spt/switch-to-entity-file))
(global-set-key (kbd "M-RET") 'spt/leader-key-map)

(provide 'init-springboot)
