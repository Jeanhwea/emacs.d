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
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------
(defun spt/project-root ()
  "Return current project root dir."
  (let ((dir (jh/git-project-root-dir default-directory)))
    (when (spt/maven-project?) dir)))

(defun spt/app-root ()
  "Return current source root dir."
  (let ((dirs
          (remove-if-not
            (lambda (dir) (file-exists-p (expand-file-name "Application.java" dir)))
            (jh/directory-sequence default-directory))))
    (unless (null dirs) (car dirs))))

(defun spt/doc-root ()
  "Return current document root dir."
  (let* ((dir (spt/project-root))
          (doc (file-name-as-directory (expand-file-name "doc" dir))))
    (when (file-exists-p doc) doc)))

(defun spt/module-root (file)
  "Return the root dir of module."
  (cond
    ((spt/controller? file)
      (expand-file-name ".." (jh/parent-dir file)))
    ((spt/service? file)
      (expand-file-name ".." (jh/parent-dir file)))
    ((and (spt/implement? file) (string-match-p ".*/service/.*" file))
      (expand-file-name "../.." (jh/parent-dir file)))
    ((spt/entity? file)
      (expand-file-name "../.." (jh/parent-dir file)))
    ((spt/repository? file)
      (expand-file-name "../.." (jh/parent-dir file)))))

(defun spt/source-files ()
  "Return a list of `*.java' files in the project."
  (let ((dir (expand-file-name "src" (spt/project-root))))
    (when (file-directory-p dir)
      (directory-files-recursively dir "^.*\\.java$"))))

(defun spt/find-file (file)
  "Open a FILE."
  (find-file file))

(defun spt/file-to-entity (file)
  "Return the entity name from a file name."
  (let ((class (jh/java-class-name file))
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

(defun spt/maven-project? ()
  "Return ture if current project is a maven project."
  (file-exists-p
    (expand-file-name "pom.xml"
      (jh/git-project-root-dir default-directory))))

;; -----------------------------------------------------------------------------
;; Modifiers
;; -----------------------------------------------------------------------------
(defun spt/insert-import-package-statement (prefix package class)
  "Insert `import com.package.ClassName;'"
  (save-excursion
    (progn
      (end-of-buffer)
      (unless (search-backward-regexp "^import \\(static \\|\\)\\([^;]*\\)\\.\\([a-zA-Z0-9]*\\);$" nil t)
        (progn
          (beginning-of-buffer)
          (next-line)))
      (end-of-line)
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

(defun spt/extract-java-public-method (line)
  "Extract java method signature."
  (save-match-data
    (and (string-match "^  public \\(static \\|\\)\\([^(]+\\) \\([_a-zA-Z][_a-zA-Z0-9]*\\)\(" line)
      (setq prefix (match-string 1 line)
        return (match-string 2 line)
        func (match-string 3 line))
      (list prefix return func))))

(defun spt/extract-java-controller-api (text)
  "Extract all api information in controller."
  (let
    ((regex
       (concat
         "^  @\\(Get\\|Post\\|Put\\|Delete\\)Mapping(\\(value = \\|\\)\"\\([^\"]*\\).*)[ \t\n]*"
         "  public \\(static \\|\\)\\([^(]+\\) \\([_a-zA-Z][_a-zA-Z0-9]*\\)(\\([^{;]*\\))\\(;\\| {\\)$"))
      (start 0)
      (res))
    (while start
      (save-match-data
        (setq start (string-match regex text start))
        (and start
          (setq method (jh/upcase (match-string 1 text))
            url (match-string 3 text)
            type (match-string 5 text)
            name (match-string 6 text)
            args (string-trim (match-string 7 text)))
          (setq res (cons (list method url type name args start) res))
          (setq start (+ start 1)))))
    (reverse res)))

(defun spt/extract-java-controller-info (file)
  "read the base url in a java controller file."
  (when (spt/controller? file)
    (let*
      ((lines (jh/read-file-content-as-lines file))
        (text (jh/read-file-content file))
        (module (car (remove-if 'null
                       (mapcar #'spt/extract-java-controller-module lines))))
        (full (car (remove-if 'null
                     (mapcar #'spt/extract-java-controller-base lines))))
        (apis (spt/extract-java-controller-api text))
        (base (cadr (split-string (or full "/") "/"))))
      (unless (or (null module) (null base)) (list module base full file apis)))))

;; -----------------------------------------------------------------------------
;; Cache builders
;; -----------------------------------------------------------------------------
(defun spt/cache-of-imported-class (file)
  "Read imported class in the FILE, then put them into a cache."
  (let ((cache (make-hash-table :test 'equal))
         (values (mapcar #'spt/extract-java-package-class
                   (jh/read-file-content-as-lines file))))
    (dolist (val values)
      (unless (null val)
        (let ((prefix (car val)) (package (cadr val)) (class (caddr val)))
          (puthash class (list prefix package) cache))))
    cache))

(defun spt/cache-of-all-imported-class ()
  "Read imported class in the whole project, then put them into a cache."
  (let ((cache (make-hash-table :test 'equal)))
    (dolist (file (spt/source-files))
      (puthash
        (jh/java-class-name file)
        (list "" (jh/java-package-name file))
        cache)
      (maphash
        (lambda (k v) (puthash k v cache))
        (spt/cache-of-imported-class file)))
    cache))

(defun spt/cache-of-class-in-project-if (pred)
  "Return a list that contains all component in the project."
  (let ((cache (make-hash-table :test 'equal))
         (files (remove-if-not pred (spt/source-files))))
    (dolist (file files)
      (puthash (jh/java-class-name file) file cache))
    cache))

(defun spt/cache-of-fields (file)
  "Read all field and type in a entity class."
  (let ((cache (make-hash-table :test 'equal))
         (fields (mapcar #'spt/extract-java-entity-field
                   (jh/read-file-content-as-lines file))))
    (dolist (field fields)
      (unless (null field)
        (let ((name (cadr field)) (type (car field)))
          (puthash name type cache))))
    cache))

(defun spt/cache-of-controller-api (file)
  "Read all api information in the controller FILE."
  (let* ((cache (make-hash-table :test 'equal))
          (info (spt/extract-java-controller-info file))
          (module (nth 0 info))
          (base (nth 1 info))
          (full (nth 2 info))
          (file (nth 3 info))
          (apis (nth 4 info)))
    (dolist (api apis)
      (setq method (nth 0 api)
        url (nth 1 api)
        type (nth 2 api)
        name (nth 3 api)
        args (nth 4 api)
        start (nth 5 api))
      (puthash
        (format "%s/%s/%s.md" module base name)
        (list file start (format "%s %s%s" method full url) type name args)
        cache))
    cache))

(defun spt/cache-of-all-controller-api ()
  "Read all api information in the whole project."
  (let ((cache (make-hash-table :test 'equal)))
    (dolist (file (spt/source-files))
      (when (spt/controller? file)
        (maphash
          (lambda (k v) (puthash k v cache))
          (spt/cache-of-controller-api file))))
    cache))

;; -----------------------------------------------------------------------------
;; Transfer file to others
;; -----------------------------------------------------------------------------
(defun spt/trans-test-and-source (file)
  "Transfer file between test and source."
  (if (spt/testcase? file)
    (replace-regexp-in-string "Test\\.java$" ".java"
      (replace-regexp-in-string "src/test/java" "src/main/java" file))
    (replace-regexp-in-string "\\.java$" "Test.java"
      (replace-regexp-in-string "src/main/java" "src/test/java" file))))

(defun spt/trans-impl-and-inter (file)
  "Transfer file between implementation and interface."
  (if (spt/implement? file)
    (replace-regexp-in-string "/impl/" "/"
      (replace-regexp-in-string "Impl\\.java$" ".java" file))
    (replace-regexp-in-string "/\\([_A-Za-z][_A-Za-z0-9]*\\.java\\)$" "/impl/\\1"
      (replace-regexp-in-string "\\.java$" "Impl.java" file))))

(defun spt/trans-doc-markdown-file (func file)
  "Transfer file to document file."
  (if (spt/controller? file)
    (let* ((info (spt/extract-java-controller-info file))
            (module (car info))
            (base (cadr info)))
      (expand-file-name
        (format "%s/%s/%s.md" module base func)
        (spt/doc-root)))))

(defun spt/trans-module-file (formula file)
  "Transfer file to absolute path."
  (let ((entity (spt/file-to-entity file))
         (base (spt/module-root file)))
    (expand-file-name (format formula entity) base)))

;; -----------------------------------------------------------------------------
;; keybind interactive function
;; -----------------------------------------------------------------------------
(defun spt/try-import-class ()
  "Try to import CLASS."
  (interactive)
  (save-buffer)
  (let* ((clz (word-at-point))
          (project-class-cache (spt/cache-of-all-imported-class))
          (file-class-cache (spt/cache-of-imported-class (buffer-file-name)))
          (prefix-package (gethash clz project-class-cache))
          (file-pkg (gethash clz file-class-cache)))
    (when (and
            (null file-pkg)
            (not (null prefix-package))
            (not (string-equal (cadr prefix-package) (jh/java-package-name))))
      (let ((pre (car prefix-package)) (pkg (cadr prefix-package)))
        (spt/insert-import-package-statement pre pkg clz)))))

(defun spt/switch-to-any-file (pred prompt)
  "Switch to controller file."
  (let* ((cache (spt/cache-of-class-in-project-if pred))
          (key (completing-read prompt (hash-table-keys cache))))
    (unless (null key)
      (spt/find-file (gethash key cache)))))

(defun spt/switch-to-entity-file ()
  "Switch to entity file."
  (interactive)
  (let ((file (buffer-file-name)))
    (when (spt/component? file)
      (spt/find-file (spt/trans-module-file "domain/entity/%s.java" file)))))

(defun spt/switch-to-any-entity-file ()
  "Switch to any entity file."
  (interactive)
  (spt/switch-to-any-file 'spt/entity? "Entity >> "))

(defun spt/switch-to-repository-file ()
  "Switch to repository file."
  (interactive)
  (let ((file (buffer-file-name)))
    (when (spt/component? file)
      (spt/find-file (spt/trans-module-file "domain/repo/%sRepository.java" file)))))

(defun spt/switch-to-any-repository-file ()
  "Switch to any repository file."
  (interactive)
  (spt/switch-to-any-file 'spt/repository? "Repository >> "))

(defun spt/switch-to-service-file ()
  "Switch to service file."
  (interactive)
  (let ((file (buffer-file-name)))
    (when (spt/component? file)
      (spt/find-file (spt/trans-module-file "service/%sService.java" file)))))

(defun spt/switch-to-any-service-file ()
  "Switch to any service file."
  (interactive)
  (spt/switch-to-any-file 'spt/service? "Service >> "))

(defun spt/switch-to-controller-file ()
  "Switch to controller file."
  (interactive)
  (let ((file (buffer-file-name)))
    (when (spt/component? file)
      (spt/find-file (spt/trans-module-file "controller/%sController.java" file)))))

(defun spt/switch-to-any-controller-file ()
  "Switch to any controller file."
  (interactive)
  (spt/switch-to-any-file 'spt/controller? "Controller >> "))

(defun spt/switch-to-controller-api-doc ()
  "Switch to controller api document file."
  (interactive)
  (let* ((file (buffer-file-name))
          (func (thing-at-point 'symbol)))
    (if (spt/controller? file)
      (when (spt/apiname? func)
        (spt/find-file (spt/trans-doc-markdown-file func file)))
      (let* ((cache (spt/cache-of-all-controller-api))
              (path (jh/relative-path file (spt/doc-root)))
              (signature (gethash path cache))
              (file (car signature))
              (pos (cadr signature)))
        (progn
          (spt/find-file file)
          (goto-char pos)
          (search-forward-regexp "{$"))))))

(defun spt/format-java-source-code ()
  "Format java source file code."
  (interactive)
  (let ((file (buffer-file-name))
         (prev-point (point)))
    (when (string-match-p "\\.java$" file)
      (progn
        (meghanada-code-beautify)
        (save-buffer)
        (if (> prev-point (point-max))
          (goto-char (point-max))
          (goto-char prev-point))))))

(defun spt/toggle-test-and-source ()
  "Toggle between implementation and test."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (spt/implement? file)
      (spt/find-file (spt/trans-test-and-source (spt/trans-impl-and-inter file)))
      (spt/find-file (spt/trans-test-and-source file)))))

(defun spt/toggle-interface-and-implement ()
  "Toggle interface and implement file."
  (interactive)
  (let ((file (buffer-file-name))
         (other-file (spt/trans-impl-and-inter (buffer-file-name))))
    (unless (spt/testcase? file) (spt/find-file other-file))))

(defun spt/jump-to-entity-field ()
  "jump to entity field."
  (interactive)
  (when (spt/entity? (buffer-file-name))
    (let* ((cache (spt/cache-of-fields (buffer-file-name)))
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
  (define-key spt/leader-key-map (kbd "C") 'spt/switch-to-any-controller-file)
  (define-key spt/leader-key-map (kbd "d") 'spt/switch-to-controller-api-doc)
  (define-key spt/leader-key-map (kbd "e") 'spt/switch-to-entity-file)
  (define-key spt/leader-key-map (kbd "E") 'spt/switch-to-any-entity-file)
  (define-key spt/leader-key-map (kbd "f") 'spt/format-java-source-code)
  (define-key spt/leader-key-map (kbd "i") 'spt/toggle-interface-and-implement)
  (define-key spt/leader-key-map (kbd "j") 'spt/jump-to-entity-field)
  (define-key spt/leader-key-map (kbd "r") 'spt/switch-to-repository-file)
  (define-key spt/leader-key-map (kbd "R") 'spt/switch-to-any-repository-file)
  (define-key spt/leader-key-map (kbd "s") 'spt/switch-to-service-file)
  (define-key spt/leader-key-map (kbd "S") 'spt/switch-to-any-service-file)
  (define-key spt/leader-key-map (kbd "t") 'spt/toggle-test-and-source)
  (define-key spt/leader-key-map (kbd "RET") 'spt/try-import-class))
(global-set-key (kbd "M-RET") 'spt/leader-key-map)

(provide 'init-springboot)
