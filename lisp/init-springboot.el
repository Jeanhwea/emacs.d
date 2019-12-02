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
;;  ____                             _____ _ _
;; / ___|  ___  _   _ _ __ ___ ___  |  ___(_) | ___  ___
;; \___ \ / _ \| | | | '__/ __/ _ \ | |_  | | |/ _ \/ __|
;;  ___) | (_) | |_| | | | (_|  __/ |  _| | | |  __/\__ \
;; |____/ \___/ \__,_|_|  \___\___| |_|   |_|_|\___||___/
;; -----------------------------------------------------------------------------

(defvar spt/bundle-of-interest
  '("entity" "repo" "service" "controller" "impl" "helper")
  "springboot bundle of interest names.")

(defvar spt/sources-cache nil
  "File cache that stores all alternative java class file.")

(defun spt/sources-cache-key (fileinfo &optional bundle)
  "Construct file cache key via FILEINFO."
  (let ((bldname (or bundle (nth 1 fileinfo))))
    (concat bldname "#" (spt/coerce-to-entity fileinfo))))

(defun spt/sources-cache-init ()
  "Initialize file cache if possible."
  (let ((fileinfos (spt/scan-source-files)))
    (dolist (fileinfo fileinfos)
      (spt/sources-cache-put fileinfo))))

(defun spt/sources-cache-put (fileinfo)
  "Put value to cache."
  (let ((key (spt/sources-cache-key fileinfo)))
    (add-to-list 'spt/sources-cache (cons key fileinfo))))

(defun spt/sources-cache-get (fileinfo bundle)
  "Get value from cache."
  (assoc (spt/sources-cache-key fileinfo bundle) spt/sources-cache))

(defun spt/source-files ()
  "Return a list of `*.java' files in the source folder."
  (directory-files-recursively (spt/src-root) "^.*\\.java$"))

(defun spt/filename-to-fileinfo (file)
  "Convert filename to fileinfo."
  (let*
    (
      ;; preparing data
      (file (or file (buffer-file-name)))
      (re (format "^\\(%s\\|%s\\)" (spt/src-root) (spt/test-root)))
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
            (replace-regexp-in-string re "" (jh/parent-dir file)) "/" t) ".")))
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
  (or spt/sources-cache (spt/sources-cache-init))
  (let*
    ((fileinfo (spt/filename-to-fileinfo (buffer-file-name)))
      (lookup (spt/sources-cache-get fileinfo bundle)))
    (if lookup
      ;; if found, return the first filename
      (car (last lookup))
      ;; otherwise, construct a filename
      (and
        (spt/sources-cache-put fileinfo)
        (spt/coerce-to-filename fileinfo bundle)))))

(defun spt/switch-to (&optional bundle)
  "Switch to related file."
  (let ((bundle (or bundle (completing-read "To >> " spt/bundle-of-interest))))
    (find-file (spt/find-alternative-source-file bundle))))


;; -----------------------------------------------------------------------------
;;  _____         _     _____ _ _
;; |_   _|__  ___| |_  |  ___(_) | ___  ___
;;   | |/ _ \/ __| __| | |_  | | |/ _ \/ __|
;;   | |  __/\__ \ |_  |  _| | | |  __/\__ \
;;   |_|\___||___/\__| |_|   |_|_|\___||___/
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
      (re (format "^\\(%s\\|%s\\)" (spt/src-root) (spt/test-root)))
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
            (replace-regexp-in-string re "" (jh/parent-dir file)) "/" t) ".")))
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
;;  ____                                        _
;; |  _ \  ___   ___ _   _ _ __ ___   ___ _ __ | |_
;; | | | |/ _ \ / __| | | | '_ ` _ \ / _ \ '_ \| __|
;; | |_| | (_) | (__| |_| | | | | | |  __/ | | | |_
;; |____/ \___/ \___|\__,_|_| |_| |_|\___|_| |_|\__|
;; -----------------------------------------------------------------------------

(defvar spt/document-cache nil
  "File cache that stores all spring controller.")

(defun spt/document-cache-key (basename funcname)
  "Construct file cache key via base and function."
  (concat basename "#" funcname))

(defun spt/document-cache-init ()
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
          (spt/document-cache-put endpoint))))))

(defun spt/document-cache-put (endpoint)
  "Put a endpoint to cache."
  (let*
    ((prefix (gethash 'http-prefix endpoint))
      (suffix (gethash 'http-suffix endpoint))
      (method (gethash 'http-method endpoint))
      (funcname (gethash 'funcname endpoint))
      (addr (gethash 'addr endpoint))
      (basename (spt/http-prefix-to-basename prefix))
      (http-api (concat method " " prefix suffix))
      (key (spt/document-cache-key basename funcname))
      (val (list http-api funcname addr file)))
    (add-to-list 'spt/document-cache (cons key val))))

(defun spt/document-cache-get (docinfo)
  "Get a endpoint to cache."
  (let
    ((key (spt/document-cache-key (nth 1 docinfo) (nth 0 docinfo))))
    (assoc key spt/document-cache)))

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
  (or spt/document-cache (spt/document-cache-init))
  (let*
    ((docinfo (spt/docfile-to-docinfo file))
      (lookup (spt/document-cache-get docinfo)))
    (and lookup (car (last lookup)))))

(defun spt/endpoint-addr (file)
  "Get endpoint address from markdown file."
  (or spt/document-cache (spt/document-cache-init))
  (let*
    ((docinfo (spt/docfile-to-docinfo file))
      (lookup (spt/document-cache-get docinfo)))
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
      (clzname (gethash 'clzname (spt/parse-java-frontinfo text))))
    (and clzname (string-match-p "Test$" clzname)
      (spt/compilation-start (spt/maven-test-command clzname)))))

(defun spt/run-test-method-command ()
  "Run a test command."
  (interactive)
  (let*
    ((text (jh/read-file-content (buffer-file-name)))
      (clzname (gethash 'clzname (spt/parse-java-frontinfo text)))
      (method (spt/current-test-method-name)))
    (and clzname (string-match-p "Test$" clzname)
      (if method
        (spt/compilation-start (spt/maven-test-command clzname method))
        (spt/compilation-start (spt/maven-test-command clzname))))))


;; -----------------------------------------------------------------------------
;;  ___                            _
;; |_ _|_ __ ___  _ __   ___  _ __| |_ ___
;;  | || '_ ` _ \| '_ \ / _ \| '__| __/ __|
;;  | || | | | | | |_) | (_) | |  | |_\__ \
;; |___|_| |_| |_| .__/ \___/|_|   \__|___/
;;               |_|
;; -----------------------------------------------------------------------------

(defvar spt/imports-cache nil
  "Imports cache that stores all classes imported in this project.")

(defun spt/fileinfo-to-import (fileinfo)
  "Convert fileinfo to import."
  (let
    ((import (make-hash-table :test 'equal :size 3)))
    (puthash 'pkgname (nth 3 fileinfo) import)
    (puthash 'clzname (nth 0 fileinfo) import)
    import))

(defun spt/imports-cache-init ()
  "Initialize cache if possible."
  (let*
    ((fileinfos (spt/scan-source-files))
      (testinfos (spt/scan-test-files)))
    ;; import source files
    (dolist (fileinfo fileinfos)
      (let*
        ((file (car (last fileinfo)))
          (text (jh/read-file-content file))
          (imports (spt/parse-java-imports text)))
        (spt/imports-cache-put (spt/fileinfo-to-import fileinfo))
        ;; imports in java file
        (dolist (import imports) (spt/imports-cache-put import))))
    ;; import test files
    (dolist (testinfo testinfos)
      (let*
        ((file (car (last testinfo)))
          (text (jh/read-file-content file))
          (imports (spt/parse-java-imports text)))
        (spt/imports-cache-put (spt/fileinfo-to-import testinfo))
        ;; imports in java file
        (dolist (import imports) (spt/imports-cache-put import))))))

(defun spt/imports-cache-put (import)
  "Put a import to cache."
  (let ((clzname (gethash 'clzname import)))
    (or (assoc clzname spt/imports-cache)
      (add-to-list 'spt/imports-cache (cons clzname import)))))

(defun spt/imports-cache-get (clzname)
  "Get a import from cache."
  (assoc clzname spt/imports-cache))

(defun spt/imported-p (clazz text)
  "Return t if CLAZZ is already imported in FILE"
  (seq-reduce
    #'(lambda (a e) (or a (string= clazz (gethash 'clzname e))))
    (spt/parse-java-imports text) nil))

(defun spt/import-unknown-class ()
  "Import class."
  (interactive)
  (or spt/imports-cache (spt/imports-cache-init))
  (let*
    ((word (thing-at-point 'word t))
      (lookup (spt/imports-cache-get word)))
    (cond
      ((null lookup)
        (message (concat "Cannot find class: " word)))
      ((spt/imported-p word (jh/current-buffer))
        (message (concat "Aready imported class: " word)))
      (t
        (save-excursion
          (let*
            ((re "^import \\(static \\|\\)\\([^;]*\\)\\.\\([_A-Za-z0-9]*\\);$")
              (import (cdr lookup))
              (clzname (gethash 'clzname import))
              (pkgname (gethash 'pkgname import))
              (impstmt
                (if (gethash 'static import)
                  (format "import static %s.%s;" pkgname clzname)
                  (format "import %s.%s;" pkgname clzname))))
            (progn
              (goto-char (point-max))
              (or (re-search-backward re nil t) (goto-char (point-min)))
              (end-of-line)
              (newline)
              (insert impstmt))))))))


;; -----------------------------------------------------------------------------
;;  __  __ ___ ____   ____
;; |  \/  |_ _/ ___| / ___|
;; | |\/| || |\___ \| |
;; | |  | || | ___) | |___
;; |_|  |_|___|____/ \____|
;; -----------------------------------------------------------------------------

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

(defun spt/meghanada-format-code ()
  "Format java source file code."
  (interactive)
  (let*
    ((saved-point (point))
      (file (buffer-file-name))
      (fileinfo (spt/filename-to-fileinfo file)))
    (and (string-match-p "\\.java$" file)
      (progn
        (meghanada-code-beautify)
        (save-buffer)
        (if (> saved-point (point-max))
          (goto-char (point-max))
          (goto-char saved-point))))))


;; -----------------------------------------------------------------------------
;;  _  __            ____  _           _ _
;; | |/ /___ _   _  | __ )(_)_ __   __| (_)_ __   __ _ ___
;; | ' // _ \ | | | |  _ \| | '_ \ / _` | | '_ \ / _` / __|
;; | . \  __/ |_| | | |_) | | | | | (_| | | | | | (_| \__ \
;; |_|\_\___|\__, | |____/|_|_| |_|\__,_|_|_| |_|\__, |___/
;;           |___/                               |___/
;; -----------------------------------------------------------------------------

(progn
  ;; Leader Key
  (define-prefix-command 'spt/leader)

  ;; Switcher Keybinding
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

  ;; workflow
  (define-key spt/leader (kbd "RET") 'spt/import-unknown-class)

  ;; todo
  (define-key spt/leader (kbd "j") 'spt/company-jpa-backend)
  (define-key spt/leader (kbd "m") 'spt/jump-to-class-methods))
(global-set-key (kbd "M-[") 'spt/leader)


;; for debugging
(defun spt/t ()
  (jh/read-file-content (buffer-file-name)))

(provide 'init-springboot)
