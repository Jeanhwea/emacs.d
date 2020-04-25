(when (require 'yasnippet)
  (setq
    yas-snippet-dirs '("~/.emacs.d/snippets")
    ;; yas-visit-from-menu t
    yas-indent-line 'auto)

  (yas-global-mode 1))

;; -----------------------------------------------------------------------------
;;      _
;;     | | __ ___   ____ _
;;  _  | |/ _` \ \ / / _` |
;; | |_| | (_| |\ V / (_| |
;;  \___/ \__,_| \_/ \__,_|
;; -----------------------------------------------------------------------------

(defun jh/java-package-name (&optional file)
  "Return the package name for a java file."
  (let*
    ((file (or file (buffer-file-name)))
      (dir (jh/parent-dir file))
      (re ".*src\\(/\\(main\\|test\\)\\)?\\(/java\\)?"))
    (mapconcat 'identity
      (split-string
        (jh/re-replace re "" dir) "/" t) ".")))

(defun jh/java-class-name (&optional file)
  "Return the class name for java."
  (let*
    ((file (or file (buffer-file-name))))
    (jh/pascalcase (jh/file-base-name file))))

(defun jh/java-test-subjects (&optional file)
  "Genearate test subject names."
  (let*
    ((file (or file (buffer-file-name)))
      (srcfile (spt/coerce-to-srcfile file))
      (methods
        (spt/parse-java-class-methods
          (jh/read-file-content srcfile)))
      (mapfn
        #'(lambda (method)
            (concat
              "test"
              (jh/pascalcase (gethash 'funcname method))
              (format-time-string "%H%M%S"))))
      (fltfn #'(lambda (method) (string= "public" (gethash 'visibility method))))
      (subjects (mapcar mapfn (remove-if-not fltfn methods))))
    subjects))

(defun jh/java-coerce-to-entity (whatever)
  "Convert `*RepositoryImpl', `*Service' ... to `*'."
  (let
    ((re "\\(RepositoryImpl\\|ServiceImpl\\|Repository\\|Service\\|Controller\\)$"))
    (jh/pascalcase (jh/re-replace re "" whatever))))

(defun jh/java-ctrl-http-prefix (ctrl)
  "Return a url mapping from name."
  (let*
    ((entity (jh/java-coerce-to-entity ctrl))
      (words (split-string (jh/kebabcase entity) "-")))
    (concat "/" (mapconcat 'identity (mapcar #'jh/pluralize words) "/"))))

(defun jh/java-pluralize-entity (entity)
  "Convert the entity name to plural form."
  (let*
    ((words (split-string (jh/kebabcase entity) "-"))
      (head (butlast words))
      (tail (car (last words)))
      (words2
        (add-to-list 'head (jh/pluralize tail) t)))
    (jh/pascalcase (mapconcat 'identity words2 "-"))))

(defun jh/java-impl-to-iface (name)
  "Convert `*Impl' to `*'"
  (jh/pascalcase (jh/re-replace "Impl$" "" name)))

(defun jh/java-endpoint-uri ()
  "Return a full url, to put it as the header of the doc, like `GET /api/'."
  (spt/endpoint-uri (buffer-file-name)))

(defun jh/java-iface-method-sign ()
  "Complete the signature of interface's method."
  (save-some-buffers t)
  (let*
    ((file (buffer-file-name))
      (ifacefile (spt/find-iface-file file))
      (implmtds
        (spt/parse-java-class-methods (jh/read-file-content file)))
      (ifacemtds
        (spt/parse-java-iface-methods (jh/read-file-content ifacefile)))
      (mapfn
        #'(lambda (method)
            (format
              "%s %s(%s)"
              (gethash 'return method)
              (gethash 'funcname method)
              (gethash 'args method))))
      (implsigns (mapcar mapfn implmtds))
      (ifacesigns (mapcar mapfn ifacemtds))
      (todo (remove-if #'(lambda (x) (member x implsigns)) ifacesigns)))
    (if todo todo '("String toString()"))))

;; -----------------------------------------------------------------------------
;;  ____    _  _____  _    ____    _    ____  _____
;; |  _ \  / \|_   _|/ \  | __ )  / \  / ___|| ____|
;; | | | |/ _ \ | | / _ \ |  _ \ / _ \ \___ \|  _|
;; | |_| / ___ \| |/ ___ \| |_) / ___ \ ___) | |___
;; |____/_/   \_\_/_/   \_\____/_/   \_\____/|_____|
;; -----------------------------------------------------------------------------
(defun jh/sql-guess-tabname ()
  "Guess the table name"
  (or (spt/read-entity-tabname (jh/current-buffer))
    (completing-read "Choose a Table >> " (jh/sql-tabnames))))

(defun jh/sql-tables ()
  "Return all tables."
  (or (local-variable-p 'tables)
    (set (make-local-variable 'tables) (qy/read-tables-meta-data)))
  tables)

(defun jh/sql-columns ()
  "Read all columns with given TABLENAME."
  (or (local-variable-p 'columns)
    (set (make-local-variable 'columns)
      (qy/read-columns-meta-data (jh/sql-guess-tabname))))
  columns)

(defun jh/sql-tabnames ()
  "Return table names for current buffer."
  (mapcar #'(lambda (x) (gethash 'tabname x)) (jh/sql-tables)))

(defun jh/sql-colnames ()
  "Return columns names for current buffer."
  (mapcar #'(lambda (x) (gethash 'colname x)) (jh/sql-columns)))

(defun jh/sql-lookup-tables (tabname)
  "Lookup tables with tabname."
  (let
    ((cache (jh/sql-tables))
      (pred #'(lambda (x) (string= (gethash 'tabname x) tabname))))
    (car (remove-if-not pred cache))))

(defun jh/sql-lookup-columns (colname)
  "Lookup columns with colname."
  (let
    ((cache (jh/sql-columns))
      (pred #'(lambda (x) (string= (gethash 'colname x) colname))))
    (car (remove-if-not pred cache))))

(defun jh/java-source-colnames ()
  "Read colunm names in Java source code."
  (let ((trans #'(lambda (x) (gethash 'colname x))))
    (mapcar trans (spt/read-fields (jh/current-buffer)))))

(defun jh/java-column-names ()
  "Return all column name."
  (let
    ((all (jh/sql-colnames))
      (added (jh/java-source-colnames))
      (ignored '("SIGNED_CODE" "DATETIME" "VALIDATION" "MYID")))
    (remove-if #'(lambda (x) (or (member x added) (member x ignored))) all)))

(defun jh/java-type (coltype)
  "Transfer column type to java type."
  (cond
    ((string= coltype "BLOB") "byte[]")
    ((string= coltype "DATE") "Timestamp")
    ((string= coltype "NUMBER") "double")
    ((member coltype
       '("CHAR" "CLOB" "VARCHAR2" "VARCHAR" "NVARCHAR2"))
      "String")
    (t "void")))

(defun jh/java-column-args (colname)
  "Build the arguments in @Column(...)"
  (let
    ((column (jh/sql-lookup-columns colname))
      (args))
    (and column (gethash 'isnul column)
      (add-to-list 'args "nullable = false"))
    (and column (gethash 'isuniq column)
      (add-to-list 'args "unique = true"))
    (and column (string= "String" (jh/java-type (gethash 'coltype column)))
      (add-to-list 'args (format "length = %d" (nth 2 column))))
    (and column (string= "CLOB" (gethash 'coltype column))
      (add-to-list 'args "columnDefinition = \"CLOB\""))
    (and column (string= "BLOB" (gethash 'coltype column))
      (add-to-list 'args "columnDefinition = \"BLOB\""))
    ;; return args
    (and args (concat ", " (mapconcat #'identity args ", ")))))

(defun jh/java-column-header (colname)
  "Build additional header, like @Lob, @Basic(...) blabla."
  (let
    ((column (jh/sql-lookup-columns colname)))
    (cond
      ((member
         (and column (gethash 'coltype column))
         '("CLOB" "BLOB"))
        "@Lob\n  @Basic(fetch = FetchType.LAZY)\n")
      (t ""))))

(defun jh/java-column-type (colname)
  "Get field type."
  (let
    ((column (jh/sql-lookup-columns colname)))
    (jh/java-type (and column (gethash 'coltype column)))))

(defun jh/java-column-comments (colname)
  "Get field comments."
  (let*
    ((column (jh/sql-lookup-columns colname))
      (colcmt (and column (gethash 'colcmt column))))
    (if (> (length colcmt) 0) colcmt "TODO: Add Comment")))

(defun jh/java-column-field (colname)
  "Get field name."
  (and colname (jh/camelcase colname)))

;; -----------------------------------------------------------------------------
;;  _____                                _       _
;; |_   _|   _ _ __   ___  ___  ___ _ __(_)_ __ | |_
;;   | || | | | '_ \ / _ \/ __|/ __| '__| | '_ \| __|
;;   | || |_| | |_) |  __/\__ \ (__| |  | | |_) | |_
;;   |_| \__, | .__/ \___||___/\___|_|  |_| .__/ \__|
;;       |___/|_|                         |_|
;; -----------------------------------------------------------------------------

(defun jh/ts-console-prefix ()
  "Return class#func"
  (let
    ((clzname
       (gethash 'clzname
         (ng/parse-ts-frontinfo (jh/current-buffer))))
      (funcname (ng/current-method-name)))
    (concat clzname "/" funcname)))

;; -----------------------------------------------------------------------------
;;  ____        _   _
;; |  _ \ _   _| |_| |__   ___  _ __
;; | |_) | | | | __| '_ \ / _ \| '_ \
;; |  __/| |_| | |_| | | | (_) | | | |
;; |_|    \__, |\__|_| |_|\___/|_| |_|
;;        |___/
;; -----------------------------------------------------------------------------
(defun jh/python-class-name (&optional file)
  "Return the class name for python."
  (let ((file (or file (buffer-file-name))))
    (jh/pascalcase (jh/file-base-name file))))

(provide 'init-yasnippet)
