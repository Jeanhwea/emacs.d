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

;; variables
(defvar spt/bundle-of-interest
  '("entity" "repo" "service" "controller" "impl" "helper")
  "springboot bundle of interest names.")


;; -----------------------------------------------------------------------------
;;  __  __      _        ___        __
;; |  \/  | ___| |_ __ _|_ _|_ __  / _| ___
;; | |\/| |/ _ \ __/ _` || || '_ \| |_ / _ \
;; | |  | |  __/ || (_| || || | | |  _| (_) |
;; |_|  |_|\___|\__\__,_|___|_| |_|_|  \___/
;; -----------------------------------------------------------------------------

(defun spt/project-root ()
  "Return current project root dir."
  (or (jh/git-project-root-dir default-directory)
    (error "This file is not inside a GIT repository")))

(defun spt/app-root (&optional entry)
  "Return current source root dir."
  (let*
    ((re (format "^.*%s\\.java$" (or entry "Application")))
      (dirs (directory-files-recursively (spt/src-root) re)))
    (or dirs (error "Failed to get application root!"))
    (jh/parent-dir (car dirs))))

(defun spt/doc-root ()
  "Return current document root dir."
  (let
    ((dir
       (file-name-as-directory
         (expand-file-name "doc" (spt/project-root)))))
    (or (file-exists-p dir) (error "Folder `doc' is not exists!"))
    dir))

(defun spt/src-root ()
  "Return current source root dir."
  (let
    ((dir
       (file-name-as-directory
         (expand-file-name "src/main/java" (spt/project-root)))))
    (or (file-exists-p dir)
      (error "Folder `src/main/java' is not exists!"))
    dir))

(defun spt/test-root ()
  "Return current test case root dir."
  (let
    ((dir
       (file-name-as-directory
         (expand-file-name "src/test/java" (spt/project-root)))))
    (or (file-exists-p dir)
      (error "Folder `src/test/java' is not exists!"))
    dir))

(defun spt/project-name ()
  "Return project name."
  (let ((root (spt/project-root)))
    (replace-regexp-in-string "/" ""
      (replace-regexp-in-string (jh/parent-dir root) "" root))))


;; -----------------------------------------------------------------------------
;;   ____           _
;;  / ___|__ _  ___| |__   ___
;; | |   / _` |/ __| '_ \ / _ \
;; | |__| (_| | (__| | | |  __/
;;  \____\__,_|\___|_| |_|\___|
;; -----------------------------------------------------------------------------

(defvar spt/bundle-entity-cache nil
  "File cache that stores all java class file.")

(defun spt/bundle-entity-cache-key (fileinfo &optional bundle)
  "Construct file cache key via FILEINFO."
  (let ((bldname (or bundle (nth 1 fileinfo))))
    (concat bldname "/" (spt/coerce-to-entity fileinfo))))

(defun spt/bundle-entity-cache-init ()
  "Initial file cache if possible."
  (or spt/bundle-entity-cache
    (and
      (setq spt/bundle-entity-cache (make-hash-table :test 'equal))
      (dolist (fileinfo (spt/scan-source-files))
        (puthash
          (spt/bundle-entity-cache-key fileinfo) fileinfo spt/bundle-entity-cache)))))

(defun spt/bundle-entity-cache-put (fileinfo)
  "Put a file to file cache."
  (and (spt/bundle-entity-cache-init)
    (puthash (spt/bundle-entity-cache-key fileinfo) fileinfo spt/bundle-entity-cache)))

(defun spt/bundle-entity-cache-get (fileinfo bundle)
  "Get a file to file cache."
  (and (spt/bundle-entity-cache-init)
    (gethash (spt/bundle-entity-cache-key fileinfo bundle) spt/bundle-entity-cache)))


;; -----------------------------------------------------------------------------
;;  _   _ _____ _     ____  _____ ____
;; | | | | ____| |   |  _ \| ____|  _ \
;; | |_| |  _| | |   | |_) |  _| | |_) |
;; |  _  | |___| |___|  __/| |___|  _ <
;; |_| |_|_____|_____|_|   |_____|_| \_\
;; -----------------------------------------------------------------------------

(defun spt/source-files ()
  "Return a list of `*.java' files in the source folder."
  (directory-files-recursively (spt/src-root) "^.*\\.java$"))

(defun spt/test-files ()
  "Return a list of `*.java' files in the test folder."
  (directory-files-recursively (spt/test-root) "^.*\\.java$"))

(defun spt/doc-files ()
  "Return a list of `*.md' files in the document folder."
  (directory-files-recursively (spt/doc-root) "^.*\\.md$"))

(defun spt/filename-to-fileinfo (&optional file)
  "Convert filename to fileinfo."
  (let*
    (
      ;; preparing
      (file (or file (buffer-file-name)))
      (tail-folder-list
        (split-string
          (replace-regexp-in-string
            (spt/app-root) "" (jh/parent-dir file)) "/" t))
      ;;
      ;; class name
      (clzname
        (jh/pascalcase
          (jh/filename-without-extension file)))
      ;; bundle name
      (bldname (car (last tail-folder-list)))
      ;; module name
      (mdlname (car tail-folder-list))
      ;; package name
      (pkgname
        (mapconcat 'identity
          (split-string
            (replace-regexp-in-string (spt/src-root) ""
              (jh/parent-dir file))
            "/" t) ".")))
    (list clzname bldname mdlname pkgname file)))

(defun spt/scan-source-files ()
  "Scan source files, construct class, module, bundle and package name."
  (let ((fileinfos))
    (dolist (file (spt/source-files))
      (let*
        ((fileinfo (spt/filename-to-fileinfo file))
          (bldname (nth 1 fileinfo))
          (mdlname (nth 2 fileinfo)))
        (and mdlname
          (not (string= "common" mdlname))
          (member bldname spt/bundle-of-interest)
          (add-to-list 'fileinfos fileinfo t))))
    fileinfos))

(defun spt/coerce-to-entity (fileinfo)
  "Force to convert to entity name."
  (let
    ((clzname (nth 0 fileinfo)) (bldname (nth 1 fileinfo)))
    (cond
      ((string= "repo" bldname)
        (replace-regexp-in-string "Repository$" "" clzname))
      ((string= "impl" bldname)
        (replace-regexp-in-string
          "\\(Service\\|Repository\\)Impl$" "" clzname))
      (t (replace-regexp-in-string
           (concat (jh/pascalcase bldname) "$") "" clzname)))))

(defun spt/coerce-to-filename (fileinfo bundle)
  "Force fileinfo to bundle filename."
  (or (member bundle spt/bundle-of-interest)
    (error (concat "Unknown bundle type: " bundle)))
  (let*
    ((mdlname (nth 2 fileinfo))
      (ettname (spt/coerce-to-entity fileinfo))
      (mdldir (expand-file-name mdlname (spt/app-root)))
      (blddir
        (cond
          ((string= "impl" bundle) "service/impl")
          ((member bundle '("entity" "repo")) (format "domain/%s" bundle))
          (t bundle)))
      (filename
        (cond
          ((string= "impl" bundle) (format "%sServiceImpl.java" ettname))
          ((string= "repo" bundle) (format "%sRepository.java" ettname))
          ((string= "entity" bundle) (format "%s.java" ettname))
          (t (format "%s%s.java" ettname (jh/pascalcase bundle))))))
    (expand-file-name filename (expand-file-name blddir mdldir))))

(defun spt/get-alternative-filename (bundle)
  "Get alternative filename with selected BUNDLE."
  (let*
    ((fileinfo (spt/filename-to-fileinfo (buffer-file-name)))
      (lookup (spt/bundle-entity-cache-get fileinfo bundle)))
    (if lookup
      ;; if found, return the first filename
      (car (last lookup))
      ;; otherwise, construct a filename
      (and
        (spt/bundle-entity-cache-put fileinfo)
        (spt/coerce-to-filename fileinfo bundle)))))

(defun spt/switch-to-file (&optional bundle)
  "Switch to related file."
  (find-file
    (spt/get-alternative-filename
      (or bundle
        (completing-read "Switch to >> " spt/bundle-of-interest)))))


;; -----------------------------------------------------------------------------
;;  ____    _  _____  _    ____    _    ____  _____
;; |  _ \  / \|_   _|/ \  | __ )  / \  / ___|| ____|
;; | | | |/ _ \ | | / _ \ |  _ \ / _ \ \___ \|  _|
;; | |_| / ___ \| |/ ___ \| |_) / ___ \ ___) | |___
;; |____/_/   \_\_/_/   \_\____/_/   \_\____/|_____|
;; -----------------------------------------------------------------------------

(defun spt/query-all-tables ()
  "Get all table information."
  (jh/oracle-list-tables))

(defun spt/query-table-columns (tabname)
  "Query columns of a table."
  (jh/oracle-list-columns tabname))


;; -----------------------------------------------------------------------------
;;  ____   _    ____  ____  _____ ____
;; |  _ \ / \  |  _ \/ ___|| ____|  _ \
;; | |_) / _ \ | |_) \___ \|  _| | |_) |
;; |  __/ ___ \|  _ < ___) | |___|  _ <
;; |_| /_/   \_\_| \_\____/|_____|_| \_\
;; -----------------------------------------------------------------------------

(defun spt/t ()
  (jh/read-file-content (buffer-file-name)))

(defun spt/l ()
  (jh/read-file-content-as-lines (buffer-file-name)))

(defun spt/parse-java-package (text)
  "Parse java package name in source file."
  (let
    ((regexp "^package \\([^;]*\\);$")
      (addr 0)
      (package))
    (save-match-data
      (setq addr (string-match regexp text addr))
      (and addr
        (setq package (match-string 1 text))))
    package))

(defun spt/parse-java-define (text)
  "Parse java define info."
  (let
    ((params (make-hash-table :test 'equal))
      (regexp
        (concat
          "^\\(\\|public\\|private\\)[ \t]*"
          "\\(class\\|interface\\)[ \t]*"
          "\\([_A-Za-z][_A-Za-z0-9]*\\)[ \t]*"
          "\\(extends\\|implements\\|\\)[ \t]*"
          "\\([_A-Za-z][_A-Za-z0-9]*\\|\\)[ \t]*"
          "{$"))
      (addr 0))
    (save-match-data
      (setq addr (string-match regexp text addr))
      (and addr
        (and
          (puthash 'visibility (match-string 1 text) params)
          (setq flag1 (match-string 2 text)) ;; class or interface
          (setq flag2 (match-string 4 text)) ;; extends or implements
          (cond
            ((string= flag1 "class")
              (puthash 'clzname (match-string 3 text) params))
            ((string= flag1 "interface")
              (puthash 'ifacename (match-string 3 text) params)))
          (cond
            ((string= flag2 "extends")
              (puthash 'supername (match-string 5 text) params))
            ((string= flag2 "implements")
              (puthash 'implname (match-string 5 text) params))))))
    params))

(defun spt/parse-java-imports (text)
  "Parse java imports, return a list of (static pkgname clzname)."
  (let
    ((regexp "^import \\(static\\|\\)[ \t]*\\([^;]*\\)\\.\\([_A-Za-z0-9]*\\);$")
      (addr 0)
      (imports))
    (while addr
      (save-match-data
        (setq addr (string-match regexp text addr))
        (and addr
          (let
            ((import (make-hash-table :test 'equal :size 3))
              (str1 (match-string 1 text))
              (str2 (match-string 2 text))
              (str3 (match-string 3 text)))
            ;; put value
            (and (string= "static" str1) (puthash 'static t import))
            (puthash 'pkgname str2 import)
            (puthash 'clzname str3 import)
            ;; append import
            (add-to-list 'imports import t))
          ;; next
          (setq addr (+ addr 1)))))
    imports))

(defun spt/parse-java-fields (text)
  "Parse all fields information in class."
  (let ((regexp
          (concat
            "^  \\(public\\|private\\|protected\\)[ \t]*"
            "\\(static\\|\\)[ \t]*"
            "\\(final\\|\\)[ \t]*"
            "\\([_a-zA-Z0-9]+\\|[_a-zA-Z0-9]+\\[\\]\\)[ \t]*"
            "\\([_a-zA-Z0-9]+\\|[_a-zA-Z0-9]+\\[\\]\\)"
            "\\( = \\|;\\)[ \t]*"))
         (addr 0)
         (fields))
    (while addr
      (save-match-data
        (setq addr (string-match regexp text addr))
        (and addr
          ;; add a new field
          (let
            ((field (make-hash-table :test 'equal :size 5))
              (str1 (match-string 1 text))
              (str2 (match-string 2 text))
              (str3 (match-string 3 text))
              (str4 (match-string 4 text))
              (str5 (match-string 5 text)))
            ;; put value
            (puthash 'visibility str1 field)
            (and (string= "static" str2) (puthash 'static t field))
            (and (string= "final" str3) (puthash 'final t field))
            (puthash 'type str4 field)
            (puthash 'name str5 field)
            (puthash 'addr addr field)
            ;; append field to list
            (add-to-list 'fields field t))
          ;; next
          (setq addr (+ addr 1)))))
    fields))

(defun spt/parse-java-class-methods (text)
  "Parse java class methods, return a list of signature."
  (let
    ((regexp
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
      (methods))
    (while addr
      (save-match-data
        (setq addr (string-match regexp text addr))
        (and addr
          ;; add a new method
          (let
            ((method (make-hash-table :test 'equal :size 10))
              (str1 (match-string 1 text))
              (str2 (match-string 2 text))
              (str3 (match-string 3 text))
              (str4 (match-string 4 text))
              (str5 (match-string 5 text)))
            ;; put value
            (and (string= "static" str2) (puthash 'static t method))
            (puthash 'visibility str1 method)
            (puthash 'return (jh/strip str3) method)
            (puthash 'funcname str4 method)
            (puthash 'args (jh/strip str5) method)
            (puthash 'addr addr method)
            ;; append method to list
            (add-to-list 'methods method t))
          ;; next
          (setq addr (+ addr 1)))))
    methods))

(defun spt/parse-java-iface-methods (text)
  "Extract java interface methods in interface."
  (let
    ((regexp
       (concat
         "^  \\(public \\|\\)"
         "\\([_A-Za-z][ ,<>_A-Za-z0-9]* \\|[_A-Za-z][_A-Za-z0-9 ]*\\[\\] \\|\\)"
         "\\([_A-Za-z][_A-Za-z0-9]*\\)[ \t]*"
         "(\\([^;{]*\\));$"))
      (addr 0)
      (methods))
    (while addr
      (save-match-data
        (setq addr (string-match regexp text addr))
        (and addr
          ;; add a new method
          (let
            ((method (make-hash-table :test 'equal :size 10))
              (str2 (match-string 2 text))
              (str3 (match-string 3 text))
              (str4 (match-string 4 text)))
            ;; put value
            (puthash 'return (jh/strip str2) method)
            (puthash 'funcname str3 method)
            (puthash 'args (jh/strip str4) method)
            (puthash 'addr addr method)
            ;; append method to list
            (add-to-list 'methods method t))
          ;; next
          (setq addr (+ addr 1)))))
    methods))

(defun spt/parse-java-meta (text)
  "Parse java class meta info to hashtable."
  (let
    ((metainfo (spt/parse-java-define text)))

    ;; add pacakage name
    (puthash 'pkgname (spt/parse-java-package text) metainfo)

    ;; parse imports
    (puthash 'imports (spt/parse-java-imports text) metainfo)

    ;; parse fields
    (puthash 'fields (spt/parse-java-fields text) metainfo)

    ;; parse interface methods
    (and
      (gethash 'ifacename metainfo)
      (puthash 'methods (spt/parse-java-iface-methods text) metainfo))

    ;; parse class methods
    (and
      (gethash 'clzname metainfo)
      (puthash 'methods (spt/parse-java-class-methods text) metainfo))

    metainfo))

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
            sign (list (jh/strip return) func (jh/strip args) addr)
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
            sign (list (jh/strip return) func (jh/strip args) addr)
            res (cons sign res)
            addr (+ addr 1))
          (setq addr (+ addr 1)))))
    (reverse res)))

(defun spt/extract-java-entity-fields (text)
  "Extract all fields information in entity."
  (let ((regexp
          (concat
            "^  @\\(JoinColumn\\|Column\\)"
            "(\\(name = \\|\\)\"\\([^\"]*\\)[^)]*)[ \t\n]*"
            "private[ \t]+\\([_a-zA-Z0-9]+\\|[_a-zA-Z0-9]+\\[\\]\\)"
            "[ \t]+\\([_a-zA-Z0-9]+\\);"))
         (addr 0)
         (res))
    (while addr
      (save-match-data
        (setq addr (string-match regexp text addr))
        (and addr
          (setq
            colname (match-string 3 text)
            type (match-string 4 text)
            name (match-string 5 text))
          (setq field (list colname type name addr)
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
            "([ \t\n]*\\(value = \\|\\)\"\\([^\"]*\\)[^)]*)[ \t\n]*"
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
            api (list (jh/upcase method) uri (jh/strip return) func (jh/strip args) addr)
            res (cons api res))
          (setq addr (+ addr 1)))))
    (reverse res)))

(defun spt/extract-java-entity-table (text)
  "Extract java package name."
  (let ((regexp "^@Table(\\(name = \\|\\)\"\\([^\"]*\\)\"[^)]*)[ \t]*$")
         (addr 0)
         (table))
    (save-match-data
      (setq addr (string-match regexp text addr))
      (and addr
        (setq table (match-string 2 text))))
    table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun spt/compilation-start (cmd &optional dir)
  "Run compilation command."
  (let ((default-directory (or dir (spt/project-root))))
    (compilation-start cmd)))

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
      (insert (jh/strip (format "import %s %s.%s;" static package class))))))

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
    (find-file file)
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
    (puthash 'package (spt/parse-java-package text) cache)
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
        (puthash (caddr field) field cache))
      cache)))

(defun spt/cache-of-table-columns (tabname)
  "Read all table columns, then put them into a cache."
  (let ((cache (make-hash-table :test 'equal))
         (columns (spt/query-table-columns tabname)))
    (dolist (col columns)
      (let ((colname (car col)))
        (and colname (puthash colname col cache))))
    cache))

;; -----------------------------------------------------------------------------
;; company
;; -----------------------------------------------------------------------------
(defun spt/company-jpa-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'spt/company-jpa-backend))
    (prefix
      (and (eq major-mode 'java-mode)
        (looking-back "find\\>")
        (match-string 0)))
    (candidates
      (when (equal arg "find")
        (list "findById" "findByCode" "findAllById" "findAllByCode")))
    (meta (format "This value is named %s" arg))))

;; (add-to-list 'company-backends 'spt/company-jpa-backend)

;; -----------------------------------------------------------------------------
;; Transfer file to others
;; -----------------------------------------------------------------------------

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
          (find-file (spt/trans-doc-markdown-file func file))))
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
            ((key (jh/strip (apply #'format "%s %s %s %s(%s)" sign)))
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

;; (defun spt/toggle-interface-and-implement ()
;;   "Toggle interface and implement file."
;;   (interactive)
;;   (or (spt/testcase? (buffer-file-name))
;;     (let ((file (buffer-file-name))
;;            (other-file (spt/trans-impl-and-inter (buffer-file-name))))
;;       (if
;;         (or (not (file-exists-p other-file)) (spt/implement? file))
;;         (find-file other-file)
;;         (let ((lookup (make-hash-table :test 'equal))
;;                (line (jh/strip (jh/current-line)))
;;                (methods
;;                  (spt/extract-java-impl-override-methods
;;                    (jh/read-file-content other-file))))
;;           (progn
;;             (dolist (method methods)
;;               (let ((key (jh/strip (apply #'format "%s %s(%s);" method)))
;;                      (addr (car (last method))))
;;                 (puthash key addr lookup)))
;;             (setq addr (gethash line lookup))
;;             (if addr
;;               (spt/goto-function-body other-file addr)
;;               (find-file other-file))))))))


;; -----------------------------------------------------------------------------
;;  _  __            ____  _           _ _
;; | |/ /___ _   _  | __ )(_)_ __   __| (_)_ __   __ _ ___
;; | ' // _ \ | | | |  _ \| | '_ \ / _` | | '_ \ / _` / __|
;; | . \  __/ |_| | | |_) | | | | | (_| | | | | | (_| \__ \
;; |_|\_\___|\__, | |____/|_|_| |_|\__,_|_|_| |_|\__, |___/
;;           |___/                               |___/
;; -----------------------------------------------------------------------------
(progn
  (define-prefix-command 'spt/leader-key-map)

  ;; switcher keybinding
  (define-key spt/leader-key-map (kbd "e") #'(lambda () (interactive) (spt/switch-to-file "entity")))
  (define-key spt/leader-key-map (kbd "r") #'(lambda () (interactive) (spt/switch-to-file "repo")))
  (define-key spt/leader-key-map (kbd "s") #'(lambda () (interactive) (spt/switch-to-file "service")))
  (define-key spt/leader-key-map (kbd "i") #'(lambda () (interactive) (spt/switch-to-file "impl")))
  (define-key spt/leader-key-map (kbd "c") #'(lambda () (interactive) (spt/switch-to-file "controller")))
  (define-key spt/leader-key-map (kbd "h") #'(lambda () (interactive) (spt/switch-to-file "helper")))

  ;; todo
  (define-key spt/leader-key-map (kbd "P") 'spt/run-test-class-command)
  (define-key spt/leader-key-map (kbd "d") 'spt/toggle-controller-and-doc)
  (define-key spt/leader-key-map (kbd "j") 'spt/company-jpa-backend)
  (define-key spt/leader-key-map (kbd "m") 'spt/jump-to-class-methods)
  (define-key spt/leader-key-map (kbd "p") 'spt/run-test-method-command)
  (define-key spt/leader-key-map (kbd "RET") 'spt/try-import-class))
(global-set-key (kbd "M-[") 'spt/leader-key-map)

(provide 'init-springboot)
