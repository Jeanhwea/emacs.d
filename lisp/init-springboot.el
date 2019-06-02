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
    (and dirs (car dirs))))

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

(defun spt/compilation-start (cmd &optional dir)
  "Run compilation command."
  (let ((default-directory (or dir (spt/project-root))))
    (compilation-start cmd)))

(defun spt/file-to-entity (file)
  "Return the entity name from a file name."
  (let ((class (jh/java-class-name file))
         (regexp "\\(RepositoryImpl\\|ServiceImpl\\|Repository\\|Service\\|Controller\\)$"))
    (when (spt/component? file)
      (replace-regexp-in-string regexp "" class))))

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

(defun spt/maven-project? ()
  "Return ture if current project is a maven project."
  (file-exists-p
    (expand-file-name "pom.xml"
      (jh/git-project-root-dir default-directory))))

;; -----------------------------------------------------------------------------
;; Modifiers and Picker
;; -----------------------------------------------------------------------------
(defun spt/insert-import-package-statement (static package class)
  "Insert `import com.package.ClassName;'"
  (save-excursion
    (progn
      (goto-char (point-max))
      (or (search-backward-regexp "^import \\(static \\|\\)\\([^;]*\\)\\.\\([a-zA-Z0-9]*\\);$" nil t)
        (progn
          (goto-char (point-min))
          (next-line)))
      (end-of-line)
      (newline)
      (insert
        (if static
          (format "import %s %s.%s;" static package class)
          (format "import %s.%s;" package class))))))

(defun spt/pick-method-name ()
  "Pick the api function name in controller"
  (let ((regexp
          (concat
            "^  \\(public\\|private\\|protected\\)[ \t]*"
            "\\(static\\|\\)[ \t]*"
            "\\([a-zA-Z][ ,<>a-zA-Z0-9]* \\|\\)"
            "\\([a-zA-Z][_a-zA-Z0-9]*\\)[ \t]*"
            "(\\([^;{]*\\))[ \t]*"
            "\\(throws\\|\\)[ \t]*"
            "\\([a-zA-Z][_a-zA-Z0-9]*\\|\\)[ \t]*"
            "\\( {\\|;\\)$"))
         (func))
    (save-excursion
      (progn
        (search-backward-regexp regexp nil t)
        (search-forward-regexp "(" nil t)
        (backward-word)
        (setq func (thing-at-point 'symbol))))
    func))

(defun spt/goto-function-body (file addr)
  "Goto a function body"
  (progn
    (spt/find-file file)
    (goto-char addr)
    (search-forward-regexp "{$")))

;; -----------------------------------------------------------------------------
;; Runner
;; -----------------------------------------------------------------------------
;; maven springboot test
;; http://maven.apache.org/surefire/maven-surefire-plugin/examples/single-test.html
(defun spt/run-test-class-command ()
  "Run a test command."
  (interactive)
  (when (spt/testcase? (buffer-file-name))
    (let ((class (jh/java-class-name)))
      (spt/compilation-start (format "mvn test -q -B -Dtest=%s" class)))))

(defun spt/run-test-method-command ()
  "Run a test command."
  (interactive)
  (when (spt/testcase? (buffer-file-name))
    (let ((class (jh/java-class-name))
           (nearest-method
             (save-excursion
               (progn
                 (and (search-backward-regexp "^[ \t]*@Test" nil t)
                   (search-forward-regexp "test[_0-9A-Za-z]+")
                   (word-at-point))))))
      (spt/compilation-start
        (if nearest-method
          (format "mvn test -q -B -Dtest=%s#%s" class nearest-method)
          (format "mvn test -q -B -Dtest=%s" class))))))

;; -----------------------------------------------------------------------------
;; Extractors
;; -----------------------------------------------------------------------------
(defun spt/extract-java-imported-class (line)
  "Extract package name and class name from line."
  (save-match-data
    (and (string-match "^import \\(static\\|\\)[ \t]*\\([^;]*\\)\\.\\([a-zA-Z0-9]*\\);$" line)
      (setq static (match-string 1 line)
        package (match-string 2 line)
        class (match-string 3 line))
      (list static package class))))

(defun spt/extract-java-class-methods (text)
  "Extract java methods, return a list of signature."
  (let ((regexp
          (concat
            "^  \\(public\\|private\\|protected\\)[ \t]*"
            "\\(static\\|\\)[ \t]*"
            "\\([a-zA-Z][ ,<>a-zA-Z0-9]* \\|\\)"
            "\\([a-zA-Z][_a-zA-Z0-9]*\\)[ \t]*"
            "(\\([^;{]*\\))"
            "\\(throws\\|\\)[ \t]*"
            "\\([a-zA-Z][_a-zA-Z0-9]*\\|\\)[ \t]*"
            "\\( {\\|;\\)$"))
         (addr 0)
         (res))
    (while addr
      (save-match-data
        (setq addr (string-match regexp text addr))
        (and addr
          (setq
            visb (match-string 1 text)
            static (match-string 2 text)
            return (match-string 3 text)
            func (match-string 4 text)
            args (match-string 5 text))
          (setq
            sign (list visb static (jh/trim-blank return) func (jh/trim-blank args) addr)
            res (cons sign res)
            addr (+ addr 1)))))
    (reverse res)))

(defun spt/extract-java-inter-methods (text)
  "Extract java method in interface."
  (let ((regexp
          (concat
            "^  \\(public \\|\\)"
            "\\([a-zA-Z][ ,<>a-zA-Z0-9]* \\|\\)"
            "\\([a-zA-Z][_a-zA-Z0-9]*\\)[ \t]*"
            "(\\([^;{]*\\));$"))
         (addr 0)
         (res))
    (while addr
      (save-match-data
        (setq addr (string-match regexp text addr))
        (and addr
          (setq
            return (match-string 2 text)
            func (match-string 3 text)
            args (match-string 4 text))
          (setq
            sign (list (jh/trim-blank return) func (jh/trim-blank args) addr)
            res (cons sign res)
            addr (+ addr 1)))))
    (reverse res)))

(defun spt/extract-java-impl-override-methods (text)
  "Extract java method in interface."
  (let ((regexp
          (concat
            "^  @Override[ \t\n]*"
            "public "
            "\\([a-zA-Z][ ,<>a-zA-Z0-9]* \\|\\)"
            "\\([a-zA-Z][_a-zA-Z0-9]*\\)[ \t]*"
            "(\\([^;{]*\\))"
            "\\( {\\|;\\)$"))
         (addr 0)
         (res))
    (while addr
      (save-match-data
        (setq addr (string-match regexp text addr))
        (and addr
          (setq
            return (match-string 1 text)
            func (match-string 2 text)
            args (match-string 3 text))
          (setq
            sign (list (jh/trim-blank return) func (jh/trim-blank args) addr)
            res (cons sign res)
            addr (+ addr 1))
          (setq addr (+ addr 1)))))
    (reverse res)))

(defun spt/extract-java-controller-module (text)
  "Extract spring boot controller module name."
  (let ((regexp "^package .*\\.\\([^.]*\\)\\.controller;$")
         (addr 0)
         (res))
    (and addr
      (save-match-data
        (setq addr (string-match regexp text addr))
        (and addr
          (setq module (match-string 1 text))
          (setq res module))))
    res))

(defun spt/extract-java-controller-router (line)
  "Extract spring boot controller base url and more."
  (let ((regexp "^@RequestMapping\(\"\\([^\"]*\\)\"\)$")
         (addr 0)
         (res))
    (and addr
      (save-match-data
        (setq addr (string-match regexp text addr))
        (and addr
          (setq router (match-string 1 text))
          (setq res router))))
    res))

(defun spt/extract-java-controller-apis (text)
  "Extract all api information in controller."
  (let ((regexp
          (concat
            "^  @\\(Get\\|Post\\|Put\\|Delete\\)Mapping"
            "(\\(value = \\|\\)\"\\([^\"]*\\).*)[ \t\n]*"
            "public \\(static\\|\\)[ \t]*"
            "\\([a-zA-Z][ ,<>a-zA-Z0-9]* \\|\\)"
            "\\([a-zA-Z][_a-zA-Z0-9]*\\)[ \t]*"
            "(\\([^;{]*\\)) {$"))
         (addr 0)
         (res))
    (while addr
      (save-match-data
        (setq addr (string-match regexp text addr))
        (and addr
          (setq
            method (match-string 1 text)
            uri (match-string 3 text)
            return (match-string 5 text)
            func (match-string 6 text)
            args (match-string 7 text))
          (setq
            api (list
                  (jh/upcase method)
                  uri
                  (jh/trim-blank return)
                  func
                  (jh/trim-blank args)
                  addr)
            res (cons api res))
          (setq addr (+ addr 1)))))
    (reverse res)))


;; -----------------------------------------------------------------------------
;; Cache builders
;; -----------------------------------------------------------------------------
(defun spt/cache-of-imported-class (file)
  "Read imported class in the FILE, then put them into a cache."
  (let ((cache (make-hash-table :test 'equal))
         (values (mapcar #'spt/extract-java-imported-class
                   (jh/read-file-content-as-lines file))))
    (dolist (val values)
      (and val
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

(defun spt/cache-of-file-meta (file)
  "Extract java class file meta information, such as class/interface, parent"
  (let ((regexp-package-declare "^package \\([^;]*\\);$")
         (regexp-class-declare
           (concat
             "^\\(\\|public\\)[ \t]*"
             "\\(class\\|interface\\)[ \t]*"
             "\\([_A-Za-z][_A-Za-z0-9]*\\)[ \t]*"
             "\\(extends\\|implements\\|\\)[ \t]*"
             "\\([_A-Za-z][_A-Za-z0-9]*\\|\\)[ \t]*"
             "{$"))
         (text (jh/read-file-content file))
         (cache (make-hash-table :test 'equal)))
    (save-match-data
      (and (string-match regexp-package-declare text)
        (setq package (match-string 1 text))
        (puthash 'package package cache)))
    (save-match-data
      (and (string-match regexp-class-declare text)
        (setq
          class-inter (match-string 2 text)
          name (match-string 3 text)
          extends-impl (match-string 4 text)
          parent (match-string 5 text))
        (puthash 'class-inter class-inter cache)
        (puthash 'name name cache)
        (puthash 'extends-impl extends-impl cache)
        (puthash 'parent parent cache)))
    cache))

(defun spt/cache-of-class-in-project-if (pred)
  "Return a list that contains all component in the project."
  (let ((cache (make-hash-table :test 'equal))
         (files (remove-if-not pred (spt/source-files))))
    (dolist (file files)
      (puthash (jh/java-class-name file) file cache))
    cache))

(defun spt/cache-of-inter-method (file)
  "Read all cache of all method in a interface."
  (let ((cache (make-hash-table :test 'equal))
         (signs (spt/extract-java-inter-methods (jh/read-file-content file))))
    (dolist (sign signs) (puthash (apply 'format "%s$%s$%s" sign) sign cache))
    cache))

(defun spt/cache-of-impl-override-method (file)
  "Read all cache of all override method in a implement."
  (let ((cache (make-hash-table :test 'equal))
         (signs (spt/extract-java-impl-override-methods (jh/read-file-content file))))
    (dolist (sign signs) (puthash (apply 'format "%s$%s$%s" sign) sign cache))
    cache))

(defun spt/cache-of-controller-api (file)
  "Read all api information in the controller FILE."
  (when (spt/controller? file)
    (let* ((cache (make-hash-table :test 'equal))
            (text (jh/read-file-content file))
            (module (spt/extract-java-controller-module text))
            (router (or (spt/extract-java-controller-router text) ""))
            (base (or (cadr (split-string (or router "/") "/")) ""))
            (apis (spt/extract-java-controller-apis text)))
      (dolist (api apis)
        (setq
          method (nth 0 api)
          uri (nth 1 api)
          return (nth 2 api)
          func (nth 3 api)
          args (nth 4 api)
          addr (nth 5 api))
        (puthash
          (format "%s/%s/%s.md" module base func)
          (list (format "%s %s%s" method router uri) return func args addr file)
          cache))
      cache)))

(defun spt/cache-of-all-controller-api ()
  "Read all api information in the whole project."
  (let ((cache (make-hash-table :test 'equal)))
    (dolist (file (spt/source-files))
      (maphash
        (lambda (k v) (puthash k v cache))
        (spt/cache-of-controller-api file)))
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
         (module (spt/module-root file)))
    (expand-file-name (format formula entity) module)))

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
    (and key (spt/find-file (gethash key cache)))))

(defun spt/switch-to-entity-file ()
  "Switch to entity file."
  (interactive)
  (let ((file (buffer-file-name)))
    (when (spt/component? file)
      (spt/find-file (spt/trans-module-file "domain/entity/%s.java" file)))))

(defun spt/switch-to-any-entity-file ()
  "Switch to any entity file."
  (interactive)
  (spt/switch-to-any-file #'spt/entity? "Entity >> "))

(defun spt/switch-to-repository-file ()
  "Switch to repository file."
  (interactive)
  (let ((file (buffer-file-name)))
    (when (spt/component? file)
      (spt/find-file (spt/trans-module-file "domain/repo/%sRepository.java" file)))))

(defun spt/switch-to-any-repository-file ()
  "Switch to any repository file."
  (interactive)
  (spt/switch-to-any-file #'spt/repository? "Repository >> "))

(defun spt/switch-to-service-file ()
  "Switch to service file."
  (interactive)
  (let ((file (buffer-file-name)))
    (when (spt/component? file)
      (spt/find-file (spt/trans-module-file "service/%sService.java" file)))))

(defun spt/switch-to-any-service-file ()
  "Switch to any service file."
  (interactive)
  (spt/switch-to-any-file #'spt/service? "Service >> "))

(defun spt/switch-to-controller-file ()
  "Switch to controller file."
  (interactive)
  (let ((file (buffer-file-name)))
    (when (spt/component? file)
      (spt/find-file (spt/trans-module-file "controller/%sController.java" file)))))

(defun spt/switch-to-any-controller-file ()
  "Switch to any controller file."
  (interactive)
  (spt/switch-to-any-file #'spt/controller? "Controller >> "))

(defun spt/switch-to-controller-api-doc ()
  "Switch to controller api document file."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (spt/controller? file)
      (let ((func (spt/pick-method-name)))
        (and func (spt/find-file (spt/trans-doc-markdown-file func file))))
      (let* ((cache (spt/cache-of-all-controller-api))
              (path (jh/relative-path file (spt/doc-root)))
              (sign (gethash path cache))
              (file (car (reverse sign)))
              (addr (cadr (reverse sign))))
        (and sign (spt/goto-function-body file addr))))))

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
    (or (spt/testcase? file) (spt/find-file other-file))))

;; -----------------------------------------------------------------------------
;; key bindings
;; -----------------------------------------------------------------------------
(progn
  (define-prefix-command 'spt/leader-key-map)
  (define-key spt/leader-key-map (kbd "C") 'spt/switch-to-any-controller-file)
  (define-key spt/leader-key-map (kbd "E") 'spt/switch-to-any-entity-file)
  (define-key spt/leader-key-map (kbd "P") 'spt/run-test-class-command)
  (define-key spt/leader-key-map (kbd "R") 'spt/switch-to-any-repository-file)
  (define-key spt/leader-key-map (kbd "S") 'spt/switch-to-any-service-file)
  (define-key spt/leader-key-map (kbd "c") 'spt/switch-to-controller-file)
  (define-key spt/leader-key-map (kbd "d") 'spt/switch-to-controller-api-doc)
  (define-key spt/leader-key-map (kbd "e") 'spt/switch-to-entity-file)
  (define-key spt/leader-key-map (kbd "f") 'spt/format-java-source-code)
  (define-key spt/leader-key-map (kbd "i") 'spt/toggle-interface-and-implement)
  (define-key spt/leader-key-map (kbd "p") 'spt/run-test-method-command)
  (define-key spt/leader-key-map (kbd "r") 'spt/switch-to-repository-file)
  (define-key spt/leader-key-map (kbd "s") 'spt/switch-to-service-file)
  (define-key spt/leader-key-map (kbd "t") 'spt/toggle-test-and-source)
  (define-key spt/leader-key-map (kbd "RET") 'spt/try-import-class))
(global-set-key (kbd "M-RET") 'spt/leader-key-map)

(provide 'init-springboot)
