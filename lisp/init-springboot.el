;; -----------------------------------------------------------------------------
;; Spring Boot 项目的目录结构
;; -----------------------------------------------------------------------------
;;
;; - package-root
;;   +- module (业务模块包)
;;     +- controller (控制器类包)
;;     +- domain (问题模型包)
;;       +- entity (实体类包)
;;       +- repo (仓库类包)
;;         +- impl (仓库实现类包)
;;     +- service (服务类包)
;;       +- impl (服务实现类包)
;;
;; -----------------------------------------------------------------------------
;; Spring Boot 文档的目录结构
;; -----------------------------------------------------------------------------
;;
;; - doc
;;   +- module
;;     - readme.md
;;     +- base (基础 URI 的名字)
;;       - readme.md
;;       - {get|post|put|delete}FunctionName.md (控制器函数名称)
;;

;; (setq tmpfile (expand-file-name (concat (if (jh/windows?) "e:" "~") "/Code/work/avic/skree/src/main/java/com/avic/mti/skree/user/service/EmployeeService.java")))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------
(defun spt/project-root (&optional file)
  "Return current project root dir."
  (jh/git-project-root-dir-from-file file))

(defun spt/source-files (&optional file)
  "Return a list of `*.java' files in the project."
  (let ((dir (expand-file-name "src" (spt/project-root file))))
    (unless (null dir) (directory-files-recursively dir "^.*\\.java$"))))

(defun spt/find-file (file)
  "Open a FILE."
  (find-file file))

(defun spt/flatten-dirs-list (dirs-list)
  "flatten a nested list."
  (let (value)
    (dolist (elt dirs-list value)
      (setq value (concatenate 'list value elt)))))

(defun spt/file-to-entity (file)
  "Return the entity name from a file name."
  (let ((class (jh/filename-without-extension file))
         (re "\\(RepositoryImpl\\|ServiceImpl\\|Repository\\|Service\\|Controller\\)$"))
    (when (spt/component? file)
      (replace-regexp-in-string re "" class))))

;; -----------------------------------------------------------------------------
;; Predictors
;; -----------------------------------------------------------------------------
(defun spt/entity? (file)
  "Return ture if FILE is a entity."
  (not (null (string-match-p "^.*/entity/[0-9a-zA-Z]*\\.java$" file))))

(defun spt/repository? (file)
  "Return ture if FILE is a repository."
  (not (null (string-match-p "^.*/repo/[0-9a-zA-Z]*Repository\\.java$" file))))

(defun spt/controller? (file)
  "Return ture if FILE is a controller."
  (not (null (string-match-p "^.*/controller/[0-9a-zA-Z]*Controller\\.java$" file))))

(defun spt/service? (file)
  "Return ture if FILE is a service."
  (not (null (string-match-p "^.*/service/[0-9a-zA-Z]*Service\\.java$" file))))

(defun spt/implement? (file)
  "Return ture if FILE is a implement."
  (not (null (string-match-p "^.*/impl/[0-9a-zA-Z]*Impl\\.java$" file))))

(defun spt/component? (file)
  "Return ture if FILE is a component"
  (or (spt/entity? file)
    (spt/repository? file)
    (spt/controller? file)
    (spt/service? file)
    (spt/implement? file)))

(defun spt/testcase? (file)
  "Return ture if FILE is a entity."
  (not (null (string-match-p "^.*/src/test/java/.*/[0-9a-zA-Z]*Test\\.java$" file))))

(defun spt/apiname? (func)
  "Return ture if func is a api name."
  (not (null (string-match-p "^\\(get\\|post\\|put\\|delete\\)" func))))

;; -----------------------------------------------------------------------------
;; Operators
;; -----------------------------------------------------------------------------
(defun spt/insert-import-package-statement (prefix package class)
  "Insert `import com.package.ClassName;'"
  (save-excursion
    (progn
      (beginning-of-buffer)
      (next-line)
      (newline)
      (insert (format "import %s%s.%s;" prefix package class)))))

;; -----------------------------------------------------------------------------
;; Extractors
;; -----------------------------------------------------------------------------
(defun spt/extract-java-package-class (line)
  "Extract package name and class name from line."
  (save-match-data
    (and (string-match "^import \\(static \\|\\)\\([^;]*\\)\\.\\([a-zA-Z0-9]*\\);$" line)
      (setq prefix (match-string 1 line)
        package (match-string 2 line)
        class (match-string 3 line))
      (list prefix package class))))

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
    (and (string-match ".*src/\\(main\\|test\\)/java/.*\\(entity\\|repo\\|service\\|controller\\)/\\([0-9a-zA-Z]*\\)\\.java$" file)
      (setq folder (match-string 1 file)
        type (match-string 2 file)
        class (match-string 3 file))
      (list folder type class))))

(defun spt/extract-java-controller-base (line)
  "Extract spring boot controller base url and more."
  (save-match-data
    (and (string-match "^@RequestMapping\(\"\\([^\"]*\\)\"\)$" line)
      (setq base (match-string 1 line))
      base)))

(defun spt/extract-java-controller-module (line)
  "Extract spring boot controller module name."
  (save-match-data
    (and (string-match "^package .*\\.\\([^.]*\\)\\.controller;$" line)
      (setq module (match-string 1 line))
      module)))

(defun spt/extract-java-method (line)
  "Extract java method signature."
  (save-match-data
    (and (string-match "^[ \t]*\\(public static\\|public\\|private\\|\\)[ \t]+\\([^(]+\\)[ \t]+\\([_a-zA-Z][_a-zA-Z0-9]*\\)\(" line)
      (setq prefix (match-string 1 line)
        return (match-string 2 line)
        func (match-string 3 line))
      (list prefix return func))))

;; -----------------------------------------------------------------------------
;; Cache builders
;; -----------------------------------------------------------------------------
(defun spt/build-imported-class-from-file (file)
  "Read imported class in the FILE, then put them into a cache."
  (let ((cache (make-hash-table :test 'equal)))
    (puthash (jh/java-class-name file) (list "" (jh/java-package-name file)) cache)
    (dolist (val (mapcar #'spt/extract-java-package-class (jh/read-file-content-as-lines file)))
      (unless (null val)
        (let ((prefix (car val)) (package (cadr val)) (class (caddr val)))
          (puthash class (list prefix package) cache))))
    cache))

(defun spt/build-imported-class-in-project ()
  "Read imported class in the whole project, then put them into a cache."
  (let ((cache (make-hash-table :test 'equal)))
    (dolist (file (spt/source-files))
      (puthash (jh/java-class-name file) (list "" (jh/java-package-name file)) cache)
      (dolist (val (mapcar #'spt/extract-java-package-class (jh/read-file-content-as-lines file)))
        (unless (null val)
          (let ((prefix (car val)) (package (cadr val)) (class (caddr val)))
            (puthash class (list prefix package) cache)))))
    cache))

(defun spt/build-components-in-project (&optional buffile)
  "Return a list that contains all component in the project."
  (let ((cache (make-hash-table :test 'equal)))
    (dolist (file (spt/source-files buffile))
      (let ((cpnt (spt/extract-java-component file)))
        (unless (null cpnt)
          (puthash (caddr cpnt) file cache))))
    cache))

(defun spt/build-field-in-entity-class (file)
  "Read all field and type in a entity class."
  (let ((cache (make-hash-table :test 'equal)))
    (dolist (fld (mapcar #'spt/extract-java-entity-field (jh/read-file-content-as-lines file)))
      (unless (null fld) (puthash (cadr fld) (car fld) cache)))
    cache))

;; -----------------------------------------------------------------------------
;; Readers
;; -----------------------------------------------------------------------------
(defun spt/read-java-controller-module-base (file)
  "read the base url in a java controller file."
  (when (spt/controller? file)
    (let*
      ((lines (jh/read-file-content-as-lines file))
        (module (car (remove-if 'null (mapcar #'spt/extract-java-controller-module lines))))
        (base (car (remove-if 'null (mapcar #'spt/extract-java-controller-base lines)))))
      (unless (or (null module) (null base)) (list module base)))))

(defun spt/read-component-in-project (predictor file)
  "Read file with given PREDICTOR."
  (remove-if-not predictor (spt/source-files file)))

;; (spt/read-component-in-project #'spt/controller? tmpfile)

;; -----------------------------------------------------------------------------
;; Transfer file to other
;; -----------------------------------------------------------------------------
(defun spt/trans-file-name (file path formula)
  "Transfer file to given relative path, with a formula, like `%sRepository.java'."
  (let ((entity (spt/file-to-entity file))
         (folder (expand-file-name path (jh/parent-dir file))))
    (expand-file-name (replace-regexp-in-string "%s" entity formula) folder)))

(defun spt/trans-doc-markdown-file (func &optional file)
  "Transfer file to document file."
  (let* ((file (or file (buffer-file-name)))
          (module-base (spt/read-java-controller-module-base file))
          (module (car module-base))
          (base (cadr (split-string (cadr module-base) "/"))))
    (expand-file-name (format "doc/%s/%s/%s.md" module base func) (spt/project-root file))))

(defun spt/trans-test-and-source (&optional file)
  "Transfer file between test and source."
  (let ((file (or file (buffer-file-name))))
    (if (spt/testcase? file)
      (replace-regexp-in-string "Test\\.java$" ".java"
        (replace-regexp-in-string "src/test/java" "src/main/java" file))
      (replace-regexp-in-string "\\.java$" "Test.java"
        (replace-regexp-in-string "src/main/java" "src/test/java" file)))))

(defun spt/trans-impl-and-inter (&optional file)
  "Transfer file between implementation and interface."
  (let ((file (or file (buffer-file-name))))
    (if (spt/implement? file)
      (replace-regexp-in-string "/impl/" "/"
        (replace-regexp-in-string "Impl\\.java$" ".java" file))
      (replace-regexp-in-string "/\\([_A-Za-z][_A-Za-z0-9]*\\.java\\)$" "/impl/\\1"
        (replace-regexp-in-string "\\.java$" "Impl.java" file)))))

;; -----------------------------------------------------------------------------
;; keybind interactive function
;; -----------------------------------------------------------------------------
(defun spt/try-import-class (&optional class)
  "Try to import CLASS."
  (interactive)
  (save-buffer)
  (let* ((clz (or class (word-at-point)))
          (project-class-cache (spt/build-imported-class-in-project))
          (file-class-cache (spt/build-imported-class-from-file (buffer-file-name)))
          (prefix-package (gethash clz project-class-cache))
          (file-pkg (gethash clz file-class-cache)))
    (when (and
            (null file-pkg)
            (not (null prefix-package))
            (not (string-equal (cadr prefix-package) (jh/java-package-name))))
      (let ((pre (car prefix-package)) (pkg (cadr prefix-package)))
        (spt/insert-import-package-statement pre pkg clz)))))

(defun spt/switch-to-component-file (path formula)
  "Switch to a component file in the project."
  (let ((entity (spt/file-to-entity (buffer-file-name)))
         (cache (spt/build-components-in-project)))
    (or (null entity) (spt/testcase? (buffer-file-name))
      (spt/find-file (spt/trans-file-name (gethash entity cache) path formula)))))

(defun spt/switch-to-entity-file ()
  "Switch to entity file."
  (interactive)
  (let ((entity (spt/file-to-entity (buffer-file-name)))
         (cache (spt/build-components-in-project)))
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

(defun spt/switch-to-controller-api-doc ()
  "Switch to controller api document file."
  (interactive)
  (let ((file (buffer-file-name))
         (func (thing-at-point 'symbol)))
    (and (spt/controller? file) (spt/apiname? func)
      (spt/find-file (spt/trans-doc-markdown-file func file)))))

(defun spt/format-java-source-code ()
  "Format java source file code."
  (interactive)
  (let ((prev-point (point)))
    (progn
      (meghanada-code-beautify)
      (save-buffer)
      (let ((new-point (if (< prev-point (point-max)) prev-point (point-max))))
        (goto-char new-point)))))

(defun spt/toggle-test-and-source ()
  "Toggle between implementation and test."
  (interactive)
  (let* ((buffile (buffer-file-name))
          (file (if (spt/implement? buffile) (spt/trans-impl-and-inter buffile) buffile)))
    (spt/find-file (spt/trans-test-and-source file))))

(defun spt/toggle-interface-and-implement (&optional file)
  "Toggle interface and implement file."
  (interactive)
  (let* ((the-file (or file (buffer-file-name)))
          (other-file (spt/trans-impl-and-inter the-file)))
    (and (not (spt/testcase? the-file)) (spt/find-file other-file))))

(defun spt/jump-to-entity-field (&optional file)
  "jump to entity field."
  (interactive)
  (setq file (or file (buffer-file-name)))
  (when (spt/entity? file)
    (let* ((cache (spt/build-field-in-entity-class file))
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
  (define-key spt/leader-key-map (kbd "d") 'spt/switch-to-controller-api-doc)
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
