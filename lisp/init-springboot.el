;; -----------------------------------------------------------------------------
;;   .   ____          _            __ _ _
;;  /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
;; ( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
;;  \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
;;   '  |____| .__|_| |_|_| |_\__, | / / / /
;;  =========|_|==============|___/=/_/_/_/
;;
;; -----------------------------------------------------------------------------
;; 1. 项目结构
;; -----------------------------------------------------------------------------
;; - app-root
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
;; 2. 文档结构
;; -----------------------------------------------------------------------------
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

(defun spt/app-test-root (&optional entry)
  "Return current source root dir."
  (let*
    ((re (format "^.*%s\\.java$" (or entry "ApplicationTest")))
      (dirs (directory-files-recursively (spt/test-root) re)))
    (or dirs (error "Failed to get application test root!"))
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
;;  \____\__,_|\___|_| |_|\___| for finding alternative files
;; -----------------------------------------------------------------------------

(defvar spt/bundle-entity-alist nil
  "File cache that stores all alternative java class file.")

(defun spt/bundle-entity-alist-key (fileinfo &optional bundle)
  "Construct file cache key via FILEINFO."
  (let ((bldname (or bundle (nth 1 fileinfo))))
    (concat bldname "#" (spt/coerce-to-entity fileinfo))))

(defun spt/bundle-entity-alist-init ()
  "Initialize file cache if possible."
  (let ((fileinfos (spt/scan-source-files)))
    (dolist (fileinfo fileinfos)
      (spt/bundle-entity-alist-put fileinfo))))

(defun spt/bundle-entity-alist-put (fileinfo)
  "Put value to cache."
  (let ((key (spt/bundle-entity-alist-key fileinfo)))
    (add-to-list 'spt/bundle-entity-alist (cons key fileinfo))))

(defun spt/bundle-entity-alist-get (fileinfo bundle)
  "Get value from cache."
  (assoc (spt/bundle-entity-alist-key fileinfo bundle) spt/bundle-entity-alist))


;; -----------------------------------------------------------------------------
;;   ____           _
;;  / ___|__ _  ___| |__   ___
;; | |   / _` |/ __| '_ \ / _ \
;; | |__| (_| | (__| | | |  __/
;;  \____\__,_|\___|_| |_|\___| for markdown documentation
;; -----------------------------------------------------------------------------

(defvar spt/base-endpoint-alist nil
  "File cache that stores all spring controller.")

(defun spt/base-endpoint-alist-key (basename funcname)
  "Construct file cache key via base and function."
  (concat basename "#" funcname))

(defun spt/base-endpoint-alist-init ()
  "Initialize cache if possible."
  (let*
    ((fileinfos (spt/scan-source-files))
      (ctrlinfos
        (remove-if-not
          #'(lambda (e) (string= "controller" (nth 1 e))) fileinfos)))
    (dolist (fileinfo ctrlinfos) ;; iterate all controller
      (let*
        ((file (car (last fileinfo)))
          (text (jh/read-file-content file))
          (endpoints (spt/read-endpoints text)))
        (dolist (endpoint endpoints)
          ;; add endpoints
          (spt/base-endpoint-alist-put endpoint))))))

(defun spt/base-endpoint-alist-put (endpoint)
  "Put a endpoint to cache."
  (let*
    ((prefix (gethash 'http-prefix endpoint))
      (suffix (gethash 'http-suffix endpoint))
      (method (gethash 'http-method endpoint))
      (funcname (gethash 'funcname endpoint))
      (addr (gethash 'addr endpoint))
      (basename (spt/http-prefix-to-basename prefix))
      (http-api (concat method " " prefix suffix))
      (key (spt/base-endpoint-alist-key basename funcname))
      (val (list http-api funcname addr file)))
    (add-to-list 'spt/base-endpoint-alist (cons key val))))

(defun spt/base-endpoint-alist-get (docinfo)
  "Get a endpoint to cache."
  (let
    ((key (spt/base-endpoint-alist-key (nth 1 docinfo) (nth 0 docinfo))))
    (assoc key spt/base-endpoint-alist)))


;; -----------------------------------------------------------------------------
;;  _   _ _____ _     ____  _____ ____
;; | | | | ____| |   |  _ \| ____|  _ \
;; | |_| |  _| | |   | |_) |  _| | |_) |
;; |  _  | |___| |___|  __/| |___|  _ <
;; |_| |_|_____|_____|_|   |_____|_| \_\ for source files
;; -----------------------------------------------------------------------------

(defun spt/source-files ()
  "Return a list of `*.java' files in the source folder."
  (directory-files-recursively (spt/src-root) "^.*\\.java$"))

(defun spt/filename-to-fileinfo (&optional file)
  "Convert filename to fileinfo."
  (let*
    (
      ;; preparing data
      (file (or file (buffer-file-name)))
      (tail-folder-list
        (split-string
          (replace-regexp-in-string
            (spt/app-root) "" (jh/parent-dir file)) "/" t))
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

(defun spt/find-alternative-source-file (bundle)
  "Find alternative filename with selected BUNDLE."
  (or spt/bundle-entity-alist (spt/bundle-entity-alist-init))
  (let*
    ((fileinfo (spt/filename-to-fileinfo (buffer-file-name)))
      (lookup (spt/bundle-entity-alist-get fileinfo bundle)))
    (if lookup
      ;; if found, return the first filename
      (car (last lookup))
      ;; otherwise, construct a filename
      (and
        (spt/bundle-entity-alist-put fileinfo)
        (spt/coerce-to-filename fileinfo bundle)))))

(defun spt/switch-to (&optional bundle)
  "Switch to related file."
  (let ((bundle (or bundle (completing-read "To >> " spt/bundle-of-interest))))
    (find-file (spt/find-alternative-source-file bundle))))


;; -----------------------------------------------------------------------------
;;  _   _ _____ _     ____  _____ ____
;; | | | | ____| |   |  _ \| ____|  _ \
;; | |_| |  _| | |   | |_) |  _| | |_) |
;; |  _  | |___| |___|  __/| |___|  _ <
;; |_| |_|_____|_____|_|   |_____|_| \_\ for test files
;; -----------------------------------------------------------------------------

(defun spt/test-files ()
  "Return a list of `*.java' files in the test folder."
  (directory-files-recursively (spt/test-root) "^.*\\.java$"))

(defun spt/testfile-to-testinfo (&optional file)
  "Convert test filename to testinfo."
  (let*
    (
      ;; preparing data
      (file (or file (buffer-file-name)))
      (tail-folder-list
        (split-string
          (replace-regexp-in-string
            (spt/app-test-root) "" (jh/parent-dir file)) "/" t))
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

(defun spt/scan-test-files ()
  "Scan test files."
  (let ((testinfos))
    (dolist (file (spt/test-files))
      (let*
        ((testinfo (spt/testfile-to-testinfo file))
          (bldname (nth 1 testinfo))
          (mdlname (nth 2 testinfo)))
        (and mdlname
          (not (string= "common" mdlname))
          (member bldname spt/bundle-of-interest)
          (add-to-list 'testinfos testinfo t))))
    testinfos))

(defun spt/coerce-to-testfile (file)
  "Force file to test file path."
  (let
    ((dir (jh/parent-dir file))
      (clzname (jh/filename-without-extension file)))
    (if (string-match-p ".*Test$" clzname) file
      (expand-file-name
        (format "%sTest.java" clzname)
        (replace-regexp-in-string "src/main/java" "src/test/java" dir)))))

(defun spt/coerce-to-srcfile (file)
  "Force file to source file path."
  (let
    ((dir (jh/parent-dir file))
      (clzname (jh/filename-without-extension file)))
    (if
      (string-match-p ".*Test$" clzname)
      (expand-file-name
        (format "%s.java" (replace-regexp-in-string "Test$" "" clzname))
        (replace-regexp-in-string "src/test/java" "src/main/java" dir))
      file)))

(defun spt/swap-test-and-source ()
  "Swap between source file and test file."
  (interactive)
  (let*
    ((file (buffer-file-name))
      (clzname (jh/filename-without-extension file)))
    (find-file
      (if (string-match-p ".*Test$" clzname)
        (spt/coerce-to-srcfile file) (spt/coerce-to-testfile file)))))


;; -----------------------------------------------------------------------------
;;  _   _ _____ _     ____  _____ ____
;; | | | | ____| |   |  _ \| ____|  _ \
;; | |_| |  _| | |   | |_) |  _| | |_) |
;; |  _  | |___| |___|  __/| |___|  _ <
;; |_| |_|_____|_____|_|   |_____|_| \_\ for document files
;; -----------------------------------------------------------------------------

(defun spt/doc-files ()
  "Return a list of `*.md' files in the document folder."
  (directory-files-recursively (spt/doc-root) "^.*\\.md$"))

(defun spt/docfile-to-docinfo (&optional file)
  "Convert doc file to docinfo. basename is the first word of http-prefix."
  (let*
    (
      ;; preparing data
      (file (or file (buffer-file-name)))
      (tail-folder-list
        (split-string
          (replace-regexp-in-string
            (spt/doc-root) "" (jh/parent-dir file)) "/" t))
      ;; function name
      (funcname (jh/filename-without-extension file))
      ;; base name
      (basename (car (last tail-folder-list)))
      ;; module name
      (mdlname (car tail-folder-list)))
    (list funcname basename mdlname file)))

(defun spt/scan-doc-files ()
  "Scan document files."
  (let ((docinfos))
    (dolist (file (spt/doc-files))
      (let*
        ((docinfo (spt/docfile-to-docinfo file))
          (funcname (nth 0 docinfo))
          (mdlname (nth 2 docinfo)))
        (and
          mdlname
          (not (string= "readme" funcname))
          (not (string= "common" mdlname))
          (add-to-list 'docinfos docinfo t))))
    docinfos))

(defun spt/http-prefix-to-basename (prefix)
  "Convert http prefix to basename."
  (car (split-string (replace-regexp-in-string "^/" "" prefix) "/")))

(defun spt/current-endpoint (file)
  "Find current endpoint, the endpoint under cursor."
  (let*
    ((endpoints
       (sort
         (spt/read-endpoints (jh/read-file-content file))
         #'(lambda (a b) (< (gethash 'addr a) (gethash 'addr b)))))
      (current-point (point))
      (lookup
        (remove-if
          #'(lambda (endpoint) (<= current-point (gethash 'addr endpoint)))
          endpoints)))
    (if lookup (car (last lookup)) (and endpoints (car endpoints)))))

(defun spt/find-endpoint (file funcname)
  "Find endpoint inside current file."
  (let*
    ((endpoints (spt/read-endpoints (jh/read-file-content file)))
      (lookup
        (remove-if-not
          #'(lambda (endpoint) (string= funcname (gethash 'funcname endpoint)))
          endpoints)))
    (and lookup (car lookup))))

(defun spt/coerce-to-markdown (file)
  "Force controller FILE to markdown file path list."
  (let*
    ((mdlname (nth 2 (spt/filename-to-fileinfo file)))
      (endpoint (spt/current-endpoint file))
      (funcname (gethash 'funcname endpoint))
      (basename (spt/http-prefix-to-basename (gethash 'http-prefix endpoint))))
    (expand-file-name
      (format "%s/%s/%s.md" mdlname basename funcname) (spt/doc-root))))

(defun spt/coerce-to-ctrlfile (file)
  "Force file to controller file."
  (or spt/base-endpoint-alist (spt/base-endpoint-alist-init))
  (let*
    ((docinfo (spt/docfile-to-docinfo file))
      (lookup (spt/base-endpoint-alist-get docinfo)))
    (and lookup (car (last lookup)))))

(defun spt/endpoint-addr (file)
  "Get endpoint address from markdown file."
  (or spt/base-endpoint-alist (spt/base-endpoint-alist-init))
  (let*
    ((docinfo (spt/docfile-to-docinfo file))
      (lookup (spt/base-endpoint-alist-get docinfo)))
    (and lookup (car (last lookup 2)))))

(defun spt/swap-markdown-and-endpoint ()
  "Swap between markdown and endpoint."
  (interactive)
  (let
    ((file (buffer-file-name)))
    (cond
      ;; Case 1: jump from markdown to endpoint
      ((string-match-p ".*\\.md$" file)
        (let ((addr (spt/endpoint-addr file)))
          (if addr
            ;; goto controller file and move cursor to function body
            (progn
              (find-file (spt/coerce-to-ctrlfile file))
              (goto-char addr)
              (search-forward-regexp "{$"))
            (message (concat "Ops, missing controller for: " file)))))
      ;; Case 2: jump from endpoint to markdown
      ((and
         (string-match-p ".*\\.java$" file)
         (string= "controller" (nth 1 (spt/filename-to-fileinfo file))))
        (find-file (spt/coerce-to-markdown file)))
      ;; default
      (t (error "Ops, neither a markdown file, nor controller file!")))))


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
;; |_| /_/   \_\_| \_\____/|_____|_| \_\  for JAVA
;; -----------------------------------------------------------------------------

(defun spt/parse-java-package (text)
  "Parse java package name. like `package com.example;' "
  (let
    ((regexp "^package \\([^;]*\\);$")
      (addr 0)
      (package))
    (save-match-data
      (setq addr (string-match regexp text addr))
      (and addr
        (setq package (match-string 1 text))))
    package))

(defun spt/parse-java-frontinfo (text)
  "Parse java front info. like `public class ClassName ...' "
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
  "Parse java imports, like `import com.example.ClassName;' "
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
            "\\( =\\|;\\)[ \t]*"))
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
  "Parse java class methods. like `public static funcName(...)' "
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
  "Parse java interface methods. like `public static funcName(...)'"
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
    ((metainfo (spt/parse-java-frontinfo text)))

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


;; -----------------------------------------------------------------------------
;;  ____   _    ____  ____  _____ ____
;; |  _ \ / \  |  _ \/ ___|| ____|  _ \
;; | |_) / _ \ | |_) \___ \|  _| | |_) |
;; |  __/ ___ \|  _ < ___) | |___|  _ <
;; |_| /_/   \_\_| \_\____/|_____|_| \_\  for Spring
;; -----------------------------------------------------------------------------

(defun spt/read-java-override-methods (text)
  "Read override methods. like `@Override ...' "
  (let
    ((regexp
       (concat
         "^  @Override[ \t\n]*"
         "\\(public\\|private\\|protected\\)[ \t]*"
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

(defun spt/read-junit-test-methods (text)
  "Read junit test method. like `@Test(...)' "
  (let
    ((regexp
       (concat
         "^  @\\(Test\\|Test([^)]*)\\)[ \t\n]*"
         "public [ \t]*"
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
            (puthash 'visibility "public" method)
            (puthash 'return (jh/strip str3) method)
            (puthash 'funcname str4 method)
            (puthash 'args (jh/strip str5) method)
            (puthash 'addr addr method)
            ;; append method to list
            (add-to-list 'methods method t))
          ;; next
          (setq addr (+ addr 1)))))
    methods))

(defun spt/read-endpoint-prefix (text)
  "Read controller endpoint prefix."
  (let
    ((regexp "^@RequestMapping\(\"\\([^\"]*\\)\"\)$")
      (addr 0)
      (prefix))
    (save-match-data
      (setq addr (string-match regexp text addr))
      (and addr
        (setq prefix (match-string 1 text))))
    prefix))

(defun spt/read-endpoint-suffixes (text)
  "Read controller endpoint suffixes."
  (let
    ((regexp
       (concat
         "^  @\\(Get\\|Post\\|Put\\|Delete\\)Mapping"
         "([ \t\n]*\\(value = \\|\\)\"\\([^\"]*\\)[^)]*)[ \t\n]*"
         "public \\(static\\|\\)[ \t]*"
         "\\([_A-Za-z][ ,<>_A-Za-z0-9]* \\|[_A-Za-z][_A-Za-z0-9 ]*\\[\\] \\)"
         "\\([_A-Za-z][_A-Za-z0-9]*\\)[ \t]*"
         "(\\([^;{]*\\)) {$"))
      (addr 0)
      (suffixes))
    (while addr
      (save-match-data
        (setq addr (string-match regexp text addr))
        (and addr
          ;; add a new method
          (let
            ((suffix (make-hash-table :test 'equal :size 10))
              (str1 (match-string 1 text))
              (str3 (match-string 3 text))
              (str5 (match-string 5 text))
              (str6 (match-string 6 text))
              (str7 (match-string 7 text)))
            ;; put value
            (puthash 'http-method (jh/upcase str1) suffix)
            (puthash 'http-suffix str3 suffix)
            (puthash 'return (jh/strip str5) suffix)
            (puthash 'funcname str6 suffix)
            (puthash 'args (jh/strip str7) suffix)
            (puthash 'addr addr suffix)
            ;; append suffix to list
            (add-to-list 'suffixes suffix t))
          ;; next
          (setq addr (+ addr 1)))))
    suffixes))

(defun spt/read-endpoints (text)
  "Read controller endpoints."
  (let
    ((prefix (spt/read-endpoint-prefix text))
      (endpoints (spt/read-endpoint-suffixes text)))
    (dolist (endpoint endpoints)
      (puthash 'http-prefix prefix endpoint))
    endpoints))

(defun spt/read-entity-tabname (text)
  "Read entity table name. like `@Table(...)' "
  (let
    ((regexp "^@Table(\\(name = \\|\\)\"\\([^\"]*\\)\"[^)]*)[ \t]*$")
      (addr 0)
      (tabname))
    (save-match-data
      (setq addr (string-match regexp text addr))
      (and addr
        (setq tabname (match-string 2 text))))
    tabname))

;; -----------------------------------------------------------------------------
;;   ____ ___  __  __ ____   _    _   ___   __
;;  / ___/ _ \|  \/  |  _ \ / \  | \ | \ \ / /
;; | |  | | | | |\/| | |_) / _ \ |  \| |\ V /
;; | |__| |_| | |  | |  __/ ___ \| |\  | | |
;;  \____\___/|_|  |_|_| /_/   \_\_| \_| |_|
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
;;  ____  _   _ _   _ _   _ _____ ____
;; |  _ \| | | | \ | | \ | | ____|  _ \
;; | |_) | | | |  \| |  \| |  _| | |_) |
;; |  _ <| |_| | |\  | |\  | |___|  _ <
;; |_| \_\\___/|_| \_|_| \_|_____|_| \_\ for Maven Springboot Test
;; -----------------------------------------------------------------------------

(defun spt/compilation-start (command &optional dir)
  "Run compilation command."
  (let ((default-directory (or dir (spt/project-root))))
    (compilation-start command)))

(defun spt/current-test-method-name ()
  "Find current test method, the method name near cursor."
  (let*
    ((file (buffer-file-name))
      (methods
        (sort
          (spt/read-junit-test-methods (jh/read-file-content file))
          #'(lambda (a b) (< (gethash 'addr a) (gethash 'addr b)))))
      (current-point (point))
      (lookup
        (remove-if
          #'(lambda (method) (<= current-point (gethash 'addr method)))
          methods)))
    (and lookup (gethash 'funcname (car (last lookup))))))

;; http://maven.apache.org/surefire/maven-surefire-plugin/examples/single-test.html
(defun spt/maven-test-command (clzname &optional method)
  "Return maven test command."
  (let
    ((subjects (if method (concat clzname "#" method) clzname))
      (args '("-Dfile.encoding=UTF-8" "--quiet" "--batch-mode")))
    (format "mvn test -Dtest=%s %s" subjects (mapconcat 'identity args " "))))

(defun spt/run-test-class-command ()
  "Run a test command."
  (interactive)
  (let*
    ((text (jh/read-file-content (buffer-file-name)))
      (clzname (gethash 'clzname (spt/parse-java-meta text))))
    (and clzname (string-match-p "Test$" clzname)
      (spt/compilation-start (spt/maven-test-command clzname)))))

(defun spt/run-test-method-command ()
  "Run a test command."
  (interactive)
  (let*
    ((text (jh/read-file-content (buffer-file-name)))
      (clzname (gethash 'clzname (spt/parse-java-meta text)))
      (method (spt/current-test-method-name)))
    (and clzname (string-match-p "Test$" clzname)
      (if method
        (spt/compilation-start (spt/maven-test-command clzname method))
        (spt/compilation-start (spt/maven-test-command clzname))))))


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


;; -----------------------------------------------------------------------------
;;  _  __            ____  _           _ _
;; | |/ /___ _   _  | __ )(_)_ __   __| (_)_ __   __ _ ___
;; | ' // _ \ | | | |  _ \| | '_ \ / _` | | '_ \ / _` / __|
;; | . \  __/ |_| | | |_) | | | | | (_| | | | | | (_| \__ \
;; |_|\_\___|\__, | |____/|_|_| |_|\__,_|_|_| |_|\__, |___/
;;           |___/                               |___/
;; -----------------------------------------------------------------------------

(progn
  (define-prefix-command 'spt/leader)

  ;; switcher keybinding
  (define-key spt/leader (kbd "e") #'(lambda () (interactive) (spt/switch-to "entity")))
  (define-key spt/leader (kbd "r") #'(lambda () (interactive) (spt/switch-to "repo")))
  (define-key spt/leader (kbd "s") #'(lambda () (interactive) (spt/switch-to "service")))
  (define-key spt/leader (kbd "i") #'(lambda () (interactive) (spt/switch-to "impl")))
  (define-key spt/leader (kbd "c") #'(lambda () (interactive) (spt/switch-to "controller")))
  (define-key spt/leader (kbd "h") #'(lambda () (interactive) (spt/switch-to "helper")))
  (define-key spt/leader (kbd "t") #'spt/swap-test-and-source)
  (define-key spt/leader (kbd "d") #'spt/swap-markdown-and-endpoint)

  ;; Unit test
  (define-key spt/leader (kbd "u") 'spt/run-test-method-command)
  (define-key spt/leader (kbd "U") 'spt/run-test-class-command)

  ;; todo
  (define-key spt/leader (kbd "j") 'spt/company-jpa-backend)
  (define-key spt/leader (kbd "m") 'spt/jump-to-class-methods)
  (define-key spt/leader (kbd "RET") 'spt/try-import-class))
(global-set-key (kbd "M-[") 'spt/leader)


;; for debugging
(defun spt/t ()
  (jh/read-file-content (buffer-file-name)))

(provide 'init-springboot)
