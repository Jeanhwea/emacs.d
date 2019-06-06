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
(defun spt/source? (file)
  "Return ture if FILE is a java source file."
  (string-match-p "\\.java$" file))

(defun spt/entity? (file)
  "Return ture if FILE is a entity."
  (string-match-p "^.*/entity/[_A-Za-z0-9]*\\.java$" file))

(defun spt/repository? (file)
  "Return ture if FILE is a repository."
  (string-match-p "^.*/repo/[_A-Za-z0-9]*Repository\\.java$" file))

(defun spt/controller? (file)
  "Return ture if FILE is a controller."
  (string-match-p "^.*/controller/[_A-Za-z0-9]*Controller\\.java$" file))

(defun spt/service? (file)
  "Return ture if FILE is a service."
  (string-match-p "^.*/service/[_A-Za-z0-9]*Service\\.java$" file))

(defun spt/implement? (file)
  "Return ture if FILE is a implement."
  (string-match-p "^.*/impl/[_A-Za-z0-9]*Impl\\.java$" file))

(defun spt/component? (file)
  "Return ture if FILE is a component"
  (or (spt/entity? file)
    (spt/repository? file)
    (spt/controller? file)
    (spt/service? file)
    (spt/implement? file)))

(defun spt/testcase? (file)
  "Return ture if FILE is a entity."
  (string-match-p "^.*/src/test/java/.*/[_A-Za-z0-9]*Test\\.java$" file))

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
      (or (search-backward-regexp "^import \\(static \\|\\)\\([^;]*\\)\\.\\([_A-Za-z0-9]*\\);$" nil t)
        (progn
          (goto-char (point-min))
          (next-line)))
      (end-of-line)
      (newline)
      (insert (jh/trim-blank (format "import %s %s.%s;" static package class))))))

(defun spt/pick-method-name ()
  "Pick the method name in controller."
  (let ((regexp
          (concat
            "^  \\(public\\|private\\|protected\\)[ \t]*"
            "\\(static\\|\\)[ \t]*"
            "\\([_A-Za-z][ ,<>_A-Za-z0-9]* \\|[_A-Za-z][_A-Za-z0-9 ]*\\[\\] \\|\\)"
            "\\([_A-Za-z][_A-Za-z0-9]*\\)[ \t]*"
            "(\\([^;{]*\\))[ \t]*"
            "\\(throws\\|\\)[ \t]*"
            "\\([_A-Za-z][_A-Za-z0-9]*\\|\\)[ \t]*"
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
(defun spt/maven-test-command (class &optional package)
  "Return maven test command."
  (let ((subjects (if package (concat class "#" package) class)))
    (concat "mvn test"
      " -Dtest=" subjects
      " -Dfile.encoding=UTF-8"
      " -Dlogging.level.root=OFF"
      " -Dlogging.level.org.springframework=OFF"
      " -Dorg.slf4j.simpleLogger.defaultLogLevel=WARN"
      " --quiet --batch-mode")))

(defun spt/run-test-class-command ()
  "Run a test command."
  (interactive)
  (when (spt/testcase? (buffer-file-name))
    (let ((this-class (jh/java-class-name)))
      (spt/compilation-start (spt/maven-test-command this-class)))))

(defun spt/run-test-method-command ()
  "Run a test command."
  (interactive)
  (when (spt/testcase? (buffer-file-name))
    (let* ((test-methods (mapcar #'cadr
                           (spt/extract-java-junit-test-methods
                             (jh/current-buffer))))
            (nearest-method (spt/pick-method-name))
            (this-class (jh/java-class-name))
            (this-method (if (member nearest-method test-methods) nearest-method)))
      (spt/compilation-start (spt/maven-test-command this-class this-method)))))

;; -----------------------------------------------------------------------------
;; Extractors
;; -----------------------------------------------------------------------------
(defun spt/extract-java-package (text)
  "Extract java package name."
  (let ((regexp "^package \\([^;]*\\);$")
         (addr 0)
         (package))
    (save-match-data
      (setq addr (string-match regexp text addr))
      (and addr
        (setq package (match-string 1 text))))
    package))

(defun spt/extract-java-imported-classes (text)
  "Extract package name and class name from line."
  (let ((regexp "^import \\(static\\|\\)[ \t]*\\([^;]*\\)\\.\\([_A-Za-z0-9]*\\);$")
         (addr 0)
         (res))
    (while addr
      (save-match-data
        (setq addr (string-match regexp text addr))
        (and addr
          (setq
            static (match-string 1 text)
            package (match-string 2 text)
            class (match-string 3 text))
          (setq
            res (cons (list static package class) res))
          (setq addr (+ addr 1)))))
    (reverse res)))

(defun spt/extract-java-clazz (text)
  "Extract java package name."
  (let ((regexp
          (concat
            "^\\(\\|public\\|private\\)[ \t]*"
            "\\(class\\|interface\\)[ \t]*"
            "\\([_A-Za-z][_A-Za-z0-9]*\\)[ \t]*"
            "\\(extends\\|implements\\|\\)[ \t]*"
            "\\([_A-Za-z][_A-Za-z0-9]*\\|\\)[ \t]*"
            "{$"))
         (addr 0)
         (clazz))
    (save-match-data
      (setq addr (string-match regexp text addr))
      (and addr
        (setq
          visb (match-string 1 text)
          class-inter (match-string 2 text)
          name (match-string 3 text)
          extends-impl (match-string 4 text)
          parent (match-string 5 text))
        (setq clazz (list visb class-inter name extends-impl parent))))
    clazz))

(defun spt/extract-java-class-methods (text)
  "Extract java methods, return a list of signature."
  (let ((regexp
          (concat
            "^  \\(public\\|private\\|protected\\)[ \t]*"
            "\\(static\\|\\)[ \t]*"
            "\\([_A-Za-z][ ,<>_A-Za-z0-9]* \\|[_A-Za-z][_A-Za-z0-9 ]*\\[\\] \\|\\)"
            "\\([_A-Za-z][_A-Za-z0-9]*\\)[ \t]*"
            "(\\([^;{]*\\))[ \t]*"
            "\\(throws\\|\\)[ \t]*"
            "\\([_A-Za-z][_A-Za-z0-9]*\\|\\)[ \t]*"
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
            sign (list
                   (jh/trim-blank visb)
                   static
                   (jh/trim-blank return)
                   func
                   (jh/trim-blank args)
                   addr)
            res (cons sign res)
            addr (+ addr 1)))))
    (reverse res)))

(defun spt/extract-java-inter-methods (text)
  "Extract java method in interface."
  (let ((regexp
          (concat
            "^  \\(public \\|\\)"
            "\\([_A-Za-z][ ,<>_A-Za-z0-9]* \\|[_A-Za-z][_A-Za-z0-9 ]*\\[\\] \\|\\)"
            "\\([_A-Za-z][_A-Za-z0-9]*\\)[ \t]*"
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
            "\\([_A-Za-z][ ,<>_A-Za-z0-9]* \\|[_A-Za-z][_A-Za-z0-9 ]*\\[\\] \\|\\)"
            "\\([_A-Za-z][_A-Za-z0-9]*\\)[ \t]*"
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

(defun spt/extract-java-junit-test-methods (text)
  "Extract java method in interface."
  (let ((regexp
          (concat
            "^  @\\(Test\\|Test([^)]*)\\)[ \t\n]*"
            "public "
            "\\([_A-Za-z][ ,<>_A-Za-z0-9]* \\|[_A-Za-z][_A-Za-z0-9 ]*\\[\\] \\|\\)"
            "\\([_A-Za-z][_A-Za-z0-9]*\\)[ \t]*"
            "(\\([^;{]*\\))[ \t]*"
            "\\(throws\\|\\)[ \t]*"
            "\\([_A-Za-z][_A-Za-z0-9]*\\|\\)[ \t]*"
            "\\( {\\|;\\)$"))
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
            addr (+ addr 1))
          (setq addr (+ addr 1)))))
    (reverse res)))

(defun spt/extract-java-entity-fields (text)
  "Extract all fields information in entity."
  (let ((regexp
          (concat
            "^  @\\(JoinColumn\\|Column\\)(.*)[ \t\n]*"
            "private[ \t]+\\([_a-zA-Z0-9]+\\|[_a-zA-Z0-9]+\\[\\]\\)"
            "[ \t]+\\([_a-zA-Z0-9]+\\);"))
         (addr 0)
         (res))
    (while addr
      (save-match-data
        (setq addr (string-match regexp text addr))
        (and addr
          (setq
            type (match-string 2 text)
            name (match-string 3 text))
          (setq field (list name type addr)
            res (cons field res))
          (setq addr (+ addr 1)))))
    (reverse res)))

(defun spt/extract-java-controller-module (text)
  "Extract spring boot controller module name."
  (let ((regexp "^package .*\\.\\([^.]*\\)\\.controller;$")
         (addr 0)
         (module))
    (save-match-data
      (setq addr (string-match regexp text addr))
      (and addr
        (setq module (match-string 1 text))))
    module))

(defun spt/extract-java-controller-router (text)
  "Extract spring boot controller base url and more."
  (let ((regexp "^@RequestMapping\(\"\\([^\"]*\\)\"\)$")
         (addr 0)
         (router))
    (save-match-data
      (setq addr (string-match regexp text addr))
      (and addr
        (setq router (match-string 1 text))))
    router))

(defun spt/extract-java-controller-apis (text)
  "Extract all api information in controller."
  (let ((regexp
          (concat
            "^  @\\(Get\\|Post\\|Put\\|Delete\\)Mapping"
            "(\\(value = \\|\\)\"\\([^\"]*\\).*)[ \t\n]*"
            "public \\(static\\|\\)[ \t]*"
            "\\([_A-Za-z][ ,<>_A-Za-z0-9]* \\|[_A-Za-z][_A-Za-z0-9 ]*\\[\\] \\)"
            "\\([_A-Za-z][_A-Za-z0-9]*\\)[ \t]*"
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

(defun spt/extract-java-entity-table (text)
  "Extract java package name."
  (let ((regexp "^@Table(\\(name = \\|\\)\"\\([^\"]*\\)\")[ \t]*$")
         (addr 0)
         (table))
    (save-match-data
      (setq addr (string-match regexp text addr))
      (and addr
        (setq table (match-string 2 text))))
    table))

;; -----------------------------------------------------------------------------
;; Database
;; -----------------------------------------------------------------------------
(defun spt/extract-table (line)
  "Extract table name."
  (let ((regexp
          (concat
            "[ \t]*\\([_A-Za-z0-9]+\\),"
            "\\(TABLE\\|VIEW\\),"
            "[ \t]*\\(.*\\)$"))
         (table))
    (and (save-match-data (string-match regexp line)
          (setq
            tabname (match-string 1 line)
            tabtype (match-string 2 line)
            comments (match-string 3 line))
          (and tabname
            (setq table (list tabname tabtype comments)))))
    table))

(defun spt/extract-table-column (line)
  "Extract table column information."
  (let ((regexp
          (concat
            "[ \t]*\\([_A-Za-z0-9]+\\),"
            "[ \t]*\\([_A-Za-z0-9]+\\),"
            "[ \t]*\\([_A-Za-z0-9]*\\),"
            "[ \t]*\\([_A-Za-z0-9]*\\),"
            "[ \t]*\\([_A-Za-z0-9]*\\),"
            "[ \t]*\\(.*\\)$"))
         (col))
    (and
      (save-match-data (string-match regexp line)
        (setq
          colname (match-string 1 line)
          dbtype (match-string 2 line)
          length (match-string 3 line)
          nullable (match-string 4 line)
          unique (match-string 5 line)
          comments (match-string 6 line))
        (and colname
          (setq col (list colname dbtype length nullable unique comments)))))
    col))

(defun spt/sql-execute (query)
  "Execute a query."
  (let ((sqlbuf (sql-find-sqli-buffer))
         (outbuf "*SQL RESULT SET*")
         (result))
    (unless sqlbuf
      (user-error "No SQL interactive buffer found"))
    (progn
      (switch-to-buffer outbuf)
      (sql-execute sqlbuf outbuf query nil nil)
      (setq result (buffer-string))
      (kill-buffer outbuf))
    result))

(defun spt/query-all-tables ()
  "Get all table information."
  (let* ((query
           (concat
             "SELECT USER_TAB_COMMENTS.TABLE_NAME ||"
             "         ',' || USER_TAB_COMMENTS.TABLE_TYPE ||"
             "         ',' || USER_TAB_COMMENTS.COMMENTS"
             "  FROM USER_TAB_COMMENTS"
             " ORDER BY USER_TAB_COMMENTS.TABLE_NAME;"))
          (lines (split-string (spt/sql-execute query) "\n")))
    (remove-if 'null (mapcar #'spt/extract-table lines))))

(defun spt/query-table-columns (tabname)
  "Query columns of a table."
  (let* ((query
           (concat
             "SELECT TAB.COLUMN_NAME ||"
             "         ',' || TAB.DATA_TYPE ||"
             "         ',' || TAB.DATA_LENGTH ||"
             "         ',' || TAB.NULLABLE ||"
             "         ',' || CONS.CONSTRAINT_TYPE ||"
             "         ',' || REPLACE(CMT.COMMENTS, TO_CHAR(CHR(13)) || TO_CHAR(CHR(10)), '')"
             "  FROM USER_TAB_COLUMNS TAB"
             "         LEFT JOIN"
             "         USER_CONS_COLUMNS CONS_NAME"
             "             ON TAB.TABLE_NAME = CONS_NAME.TABLE_NAME AND TAB.COLUMN_NAME = CONS_NAME.COLUMN_NAME"
             "         LEFT JOIN"
             "         USER_CONSTRAINTS CONS"
             "             ON CONS_NAME.CONSTRAINT_NAME = CONS.CONSTRAINT_NAME"
             "         LEFT JOIN"
             "         USER_COL_COMMENTS CMT"
             "             ON TAB.TABLE_NAME = CMT.TABLE_NAME AND TAB.COLUMN_NAME = CMT.COLUMN_NAME"
             " WHERE TAB.TABLE_NAME = '" tabname "'"
             "    ORDER BY CONS.CONSTRAINT_TYPE;"))
          (lines (split-string (spt/sql-execute query) "\n")))
    (remove-if 'null (mapcar #'spt/extract-table-column lines))))

(defun spt/cache-of-table-columns (tabname)
  "Read all table columns, then put them into a cache."
  (let ((cache (make-hash-table :test 'equal))
         (columns (spt/query-table-columns tabname)))
    (dolist (col columns)
      (let ((colname (car col)))
        (and colname (puthash colname col cache))))
    cache))

;; -----------------------------------------------------------------------------
;; Cache builders
;; -----------------------------------------------------------------------------
(defun spt/cache-of-imports (file)
  "Read imported class in the FILE, then put them into a cache."
  (let ((cache (make-hash-table :test 'equal))
         (imports (spt/extract-java-imported-classes (jh/read-file-content file))))
    (dolist (import imports)
      (and import
        (puthash (caddr import) import cache)))
    cache))

(defun spt/cache-of-all-imports ()
  "Read imported class in the whole project, then put them into a cache."
  (let ((cache (make-hash-table :test 'equal)))
    (dolist (file (spt/source-files))
      (let ((class (jh/java-class-name file))
             (package (jh/java-package-name file)))
        (puthash class (list "" package class) cache))
      (maphash
        (lambda (k v) (puthash k v cache))
        (spt/cache-of-imports file)))
    cache))

(defun spt/cache-of-file-meta (file)
  "Extract java class file meta information, such as class/interface, parent"
  (let* ((cache (make-hash-table :test 'equal))
          (text (jh/read-file-content file))
          (class-inter (cadr (spt/extract-java-clazz text))))
    (puthash 'package (spt/extract-java-package text) cache)
    (puthash 'class (spt/extract-java-clazz text) cache)
    (puthash 'imports (spt/extract-java-imported-classes text) cache)
    (puthash 'methods
      (if (string= "interface" class-inter)
        (spt/extract-java-inter-methods text)
        (spt/extract-java-class-methods text))
      cache)
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
    (dolist (sign signs) (puthash (apply #'format "%s$%s$%s" sign) sign cache))
    cache))

(defun spt/cache-of-impl-override-method (file)
  "Read all cache of all override method in a implement."
  (let ((cache (make-hash-table :test 'equal))
         (signs (spt/extract-java-impl-override-methods (jh/read-file-content file))))
    (dolist (sign signs) (puthash (apply #'format "%s$%s$%s" sign) sign cache))
    cache))

(defun spt/cache-of-controller-api (file)
  "Read api information in the controller FILE."
  (and (spt/controller? file)
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
          (list (format "%s %s%s" method router uri) return func args file addr)
          cache))
      cache)))

(defun spt/cache-of-all-controller-api ()
  "Read all api information in the whole project."
  (let ((cache (make-hash-table :test 'equal)))
    (dolist (file (spt/source-files))
      (and (spt/controller? file)
        (maphash
          (lambda (k v) (puthash k v cache))
          (spt/cache-of-controller-api file))))
    cache))

(defun spt/cache-of-entity-fields (file)
  "Read fields information in the entity FILE."
  (and (spt/entity? file)
    (let* ((cache (make-hash-table :test 'equal))
            (text (jh/read-file-content file))
            (fields (spt/extract-java-entity-fields text)))
      (dolist (field fields)
        (puthash (car field) field cache))
      cache)))

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
    (let* ((text (jh/read-file-content file))
            (module (spt/extract-java-controller-module text))
            (router (or (spt/extract-java-controller-router text) ""))
            (base (or (cadr (split-string (or router "/") "/")) "")))
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
  (progn
    (save-buffer)
    (let* ((word (thing-at-point 'word t))
            (importing (gethash word (spt/cache-of-all-imports)))
            (imported (gethash word (spt/cache-of-imports (buffer-file-name)))))
      (and importing (not imported)
        (not (string= (cadr importing) (jh/java-package-name)))
        (apply #'spt/insert-import-package-statement importing)))))

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

(defun spt/toggle-controller-and-doc ()
  "Switch to controller api document file."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (spt/controller? file)
      (let ((func (spt/pick-method-name))
             (available
               (mapcar #'caddr
                 (hash-table-values (spt/cache-of-controller-api file)))))
        (and func (member func available)
          (spt/find-file (spt/trans-doc-markdown-file func file))))
      (let* ((cache (spt/cache-of-all-controller-api))
              (path (jh/relative-path file (spt/doc-root)))
              (sign (gethash path cache))
              (addr (car (last sign)))
              (file (cadr (reverse sign))))
        (and sign (spt/goto-function-body file addr))))))

(defun spt/jump-to-class-methods ()
  "Jump to class methods"
  (interactive)
  (and (spt/source? (buffer-file-name))
    (let* ((signs (spt/extract-java-class-methods (jh/current-buffer)))
            (lookup (make-hash-table :test 'equal)))
      (progn
        (dolist (sign signs)
          (let
            ((key (jh/trim-blank (apply #'format "%s %s %s %s(%s)" sign)))
              (addr (car (last sign))))
            (puthash key addr lookup)))
        (setq read (completing-read "Goto method >> " (hash-table-keys lookup))
          addr (gethash read lookup))
        (and read addr (spt/goto-function-body (buffer-file-name) addr))))))

(defun spt/format-java-source-code ()
  "Format java source file code."
  (interactive)
  (let ((file (buffer-file-name))
         (prev-point (point)))
    (and (spt/source? file)
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
  (or (spt/testcase? (buffer-file-name))
    (let ((file (buffer-file-name))
           (other-file (spt/trans-impl-and-inter (buffer-file-name))))
      (if
        (or (not (file-exists-p other-file)) (spt/implement? file))
        (spt/find-file other-file)
        (let ((lookup (make-hash-table :test 'equal))
               (line (jh/trim-blank (jh/current-line)))
               (methods
                 (spt/extract-java-impl-override-methods
                   (jh/read-file-content other-file))))
          (progn
            (dolist (method methods)
              (let ((key (jh/trim-blank (apply #'format "%s %s(%s);" method)))
                     (addr (car (last method))))
                (puthash key addr lookup)))
            (setq addr (gethash line lookup))
            (if addr
              (spt/goto-function-body other-file addr)
              (spt/find-file other-file))))))))


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
  (define-key spt/leader-key-map (kbd "d") 'spt/toggle-controller-and-doc)
  (define-key spt/leader-key-map (kbd "e") 'spt/switch-to-entity-file)
  (define-key spt/leader-key-map (kbd "f") 'spt/format-java-source-code)
  (define-key spt/leader-key-map (kbd "i") 'spt/toggle-interface-and-implement)
  (define-key spt/leader-key-map (kbd "m") 'spt/jump-to-class-methods)
  (define-key spt/leader-key-map (kbd "p") 'spt/run-test-method-command)
  (define-key spt/leader-key-map (kbd "r") 'spt/switch-to-repository-file)
  (define-key spt/leader-key-map (kbd "s") 'spt/switch-to-service-file)
  (define-key spt/leader-key-map (kbd "t") 'spt/toggle-test-and-source)
  (define-key spt/leader-key-map (kbd "RET") 'spt/try-import-class))
(global-set-key (kbd "M-j") 'spt/leader-key-map)

(provide 'init-springboot)
