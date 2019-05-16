(defun spt/find-file (file)
  "Open a FILE."
  (find-file file))

(defun spt/project-root (&optional file)
  "Return current project root dir."
  (jh/git-project-root-dir-from-file file))

(defun spt/source-files (&optional file)
  "Return a list of `*.java' files in the project."
  (let ((dir (expand-file-name "src" (spt/project-root file))))
    (unless (null dir) (directory-files-recursively dir "^.*\.java$"))))

;; (spt/source-files "e:/Code/work/avic/skree/src/main/java/com/avic/mti/skree/user/service/EmployeeService.java")

(defun spt/flatten-dirs-list (dirs-list)
  "flatten a nested list."
  (let (value)
    (dolist (elt dirs-list value)
      (setq value (concatenate 'list value elt)))))

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

;; -----------------------------------------------------------------------------
;; modify file contents
;; -----------------------------------------------------------------------------
(defun spt/insert-import-package-statement (package class)
  "Insert `import com.package.ClassName;'"
  (save-excursion
    (progn
      (beginning-of-buffer)
      (next-line)
      (newline)
      (insert (format "import %s.%s;" package class)))))

;; -----------------------------------------------------------------------------
;; extractor
;; -----------------------------------------------------------------------------
(defun spt/extract-java-package-class (line)
  "Extract package name and class name from line."
  (save-match-data
    (and (string-match "^import \\([^;]*\\)\\.\\([a-zA-Z0-9]*\\);$" line)
      (setq package (match-string 1 line)
        class (match-string 2 line))
      (list class package))))

(defun spt/extract-java-entity-field (line)
  "Extract java entity file from line."
  (save-match-data
    (and (string-match "^\\([\t ]+\\)private \\([0-9A-Za-z]+\\) \\([0-9A-Za-z]+\\);" line)
      (setq type (match-string 2 line)
        name (match-string 3 line))
      (list type name))))

(defun spt/extract-java-component (file)
  "Extract package and entity name."
  (save-match-data
    (and (string-match ".*src/\\(main\\|test\\)/java/.*\\(entity\\|repo\\|service\\|controller\\)/\\([0-9a-zA-Z]*\\).java$" file)
      (setq folder (match-string 1 file)
        type (match-string 2 file)
        class (match-string 3 file))
      (list folder type class))))

;; -----------------------------------------------------------------------------
;; cache builder
;; -----------------------------------------------------------------------------
(defun spt/read-imported-class-from-file (file)
  "Read imported class in the FILE, then put them into a cache."
  (let ((cache (make-hash-table :test 'equal)))
    (puthash (jh/java-class-name file) (jh/java-package-name file) cache)
    (dolist (ele (mapcar #'spt/extract-java-package-class (jh/read-file-content-as-lines file)))
      (unless (null ele) (puthash (car ele) (cadr ele) cache)))
    cache))

;; (spt/read-imported-class-from-file "e:/Code/work/avic/skree/src/main/java/com/avic/mti/skree/user/service/EmployeeService.java")

(defun spt/read-imported-class-in-project ()
  "Read imported class in the whole project, then put them into a cache."
  (let ((cache (make-hash-table :test 'equal)))
    (dolist (file (spt/source-files))
      (puthash (jh/java-class-name file) (jh/java-package-name file) cache)
      (dolist (ele (mapcar #'spt/extract-java-package-class (jh/read-file-content-as-lines file)))
        (unless (null ele) (puthash (car ele) (cadr ele) cache))))
    cache))

(defun spt/read-components-in-project (&optional buffile)
  "Return a list that contains all component in the project."
  (let ((cache (make-hash-table :test 'equal)))
    (dolist (file (spt/source-files buffile))
      (let ((cpnt (spt/extract-java-component file)))
        (unless (null cpnt)
          (puthash (caddr cpnt) file cache))))
    cache))

;; (spt/read-components-in-project "e:/Code/work/avic/skree/src/main/java/com/avic/mti/skree/user/service/EmployeeService.java")

(defun spt/read-field-in-entity-class (file)
  "Read all field and type in a entity class."
  (let ((cache (make-hash-table :test 'equal)))
    (dolist (fld (mapcar #'spt/extract-java-entity-field (jh/read-file-content-as-lines file)))
      (unless (null fld) (puthash (cadr fld) (car fld) cache)))
    cache))

;; (spt/read-field-in-entity-class "e:/Code/work/avic/skree/src/main/java/com/avic/mti/skree/user/domain/entity/Employee.java")

;; -----------------------------------------------------------------------------
;; keybind interactive function
;; -----------------------------------------------------------------------------
(defun spt/try-import-class (&optional class)
  "Try to import CLASS."
  (interactive)
  (save-buffer)
  (let* ((clz (or class (word-at-point)))
          (project-class-cache (spt/read-imported-class-in-project))
          (file-class-cache (spt/read-imported-class-from-file (buffer-file-name)))
          (pkg (gethash clz project-class-cache))
          (file-pkg (gethash clz file-class-cache)))
    (when (and (null file-pkg) (not (null pkg)) (not (string-equal pkg (jh/java-package-name))))
      (spt/insert-import-package-statement pkg clz))))

(defun spt/trans-file-name (file path formula)
  "Transfer file to given relative path, with a formula, like `%sRepository.java'."
  (let ((entity (spt/file-name-to-entity-name file))
         (folder (expand-file-name path (jh/parent-dir file))))
    (expand-file-name (replace-regexp-in-string "%s" entity formula) folder)))

(defun spt/switch-to-component-file (path formula)
  "Switch to a component file in the project."
  (let ((entity (spt/file-name-to-entity-name (buffer-file-name)))
         (cache (spt/read-components-in-project)))
    (unless (null entity)
      (spt/find-file
        (spt/trans-file-name
          (gethash entity cache) path formula)))))

(defun spt/switch-to-entity-file ()
  "Switch to entity file."
  (interactive)
  (let ((entity (spt/file-name-to-entity-name (buffer-file-name)))
         (cache (spt/read-components-in-project)))
    (unless (null entity)
      (spt/find-file (gethash entity cache)))))

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

(defun spt/format-java-source-code ()
  "format java source file code."
  (interactive)
  (save-excursion
    (meghanada-code-beautify)))

(defun spt/toggle-test-and-source ()
  "toggle between implementation and test."
  (interactive)
  (projectile-toggle-between-implementation-and-test))

(defun spt/toggle-interface-and-implement (&optional file)
  "Toggle interface and implement file."
  (interactive)
  (let* ((the-file (or file (buffer-file-name)))
          (dir (jh/parent-dir the-file))
          (class (spt/file-name-to-class-name the-file))
          (other-file (cond
                        ((string-match-p "^.*Impl$" class)
                          (expand-file-name
                            (concat (replace-regexp-in-string "Impl$" "" class) ".java")
                            (jh/parent-dir dir)))
                        ((string-match-p "^.*\\(Repository\\|Service\\)$" class)
                          (expand-file-name
                            (concat class "Impl.java")
                            (expand-file-name "impl" dir)))
                        (t nil))))
    (unless (null other-file) (spt/find-file other-file))))

(defun spt/jump-to-entity-field (&optional file)
  "jump to entity field."
  (interactive)
  (when (string-match-p "^.*/entity/.*.java$" (or file (buffer-file-name)))
    (let* ((cache (spt/read-field-in-entity-class file))
            (fields (hash-table-keys cache))
            (field (completing-read "jump to > " fields))
            (regexp (format "private %s %s;" (gethash field cache) field)))
      (progn
        (beginning-of-buffer)
        (re-search-forward regexp nil)))))

;; -----------------------------------------------------------------------------
;; key bindings
;; -----------------------------------------------------------------------------
(progn
  (define-prefix-command 'spt/leader-key-map)
  (define-key spt/leader-key-map (kbd "c") 'spt/switch-to-controller-file)
  (define-key spt/leader-key-map (kbd "e") 'spt/switch-to-entity-file)
  (define-key spt/leader-key-map (kbd "f") 'spt/format-java-source-code)
  (define-key spt/leader-key-map (kbd "i") 'spt/toggle-interface-and-implement)
  (define-key spt/leader-key-map (kbd "j") 'spt/jump-to-entity-field)
  (define-key spt/leader-key-map (kbd "r") 'spt/switch-to-repository-file)
  (define-key spt/leader-key-map (kbd "s") 'spt/switch-to-service-file)
  (define-key spt/leader-key-map (kbd "t") 'spt/toggle-test-and-source)
  (define-key spt/leader-key-map (kbd "RET") 'spt/try-import-class))
(global-set-key (kbd "M-RET") 'spt/leader-key-map)

(provide 'init-springboot)
