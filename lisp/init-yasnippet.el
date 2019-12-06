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
  (let
    ((dir (jh/parent-dir (or file (buffer-file-name))))
      (re ".*src\\(/\\(main\\|test\\)\\)?\\(/java\\)?"))
    (mapconcat 'identity
      (split-string
        (jh/re-replace re "" dir) "/" t) ".")))

(defun jh/java-class-name (&optional file)
  "Return the class name for java."
  (let ((file (or file (buffer-file-name))))
    (jh/pascalcase (jh/file-base-name file))))

(defun jh/java-test-subjects (file)
  "Genearate test subject names."
  (and (string-match-p "Test$" (jh/java-class-name file))
    (let*
      ((srcfile (spt/coerce-to-srcfile file))
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
      subjects)))

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

(defun jh/java-tabnames ()
  "Return all table name."
  (mapcar 'car (jh/oracle-list-tables)))

(defun jh/java-tabcols ()
  "Get table columns for current buffer."
  (or (local-variable-p 'tabcols)
    (let
      ((tabname
         (or (spt/read-entity-tabname (jh/current-buffer))
           (completing-read "Load Table >> " (jh/java-tabnames)))))
      (set (make-local-variable 'tabcols) (jh/oracle-list-columns tabname))))
  tabcols)

(defvar jh/java-ignored-colnames '("SIGNED_CODE" "DATETIME" "VALIDATION" "MYID")
  "Java entity ignored columns.")

(defun jh/java-column-names ()
  "Return all column name."
  (let*
    ((text (jh/current-buffer))
      (fcmap
        (mapcar
          #'(lambda (e)
              (cons (gethash 'name e) (gethash 'colname e)))
          (spt/read-column-infos text)))
      (fields
        (mapcar
          #'(lambda (f)
              (cdr (assoc (gethash 'name f) fcmap)))
          (spt/parse-java-fields text)))
      (colnames
        (mapcar #'car (jh/java-tabcols))))
    (remove-if
      #'(lambda (x)
          (or (member x fields) (member x jh/java-ignored-colnames)))
      colnames)))

(defvar jh/dbtype-fields-type-alist
  '(("BLOB" . "byte[]")
     ("CHAR" . "String")
     ("CLOB" . "String")
     ("NVARCHAR2" . "String")
     ("VARCHAR" . "String")
     ("VARCHAR2" . "String")
     ("DATE" . "Timestamp")
     ("NUMBER" . "double"))
  "Map dbtype to entity field type.")

(defun jh/java-column-args (colname)
  "Build the arguments in @Column(...)"
  (let*
    ((column (assoc colname (jh/java-tabcols)))
      (lookup (assoc (cadr column) jh/dbtype-fields-type-alist))
      (args))
    (and (string= "N" (nth 3 column))
      (add-to-list 'args "nullable = false"))
    (and (string= "U" (nth 4 column))
      (add-to-list 'args "unique = true"))
    (and lookup (string= "String" (cdr lookup))
      (add-to-list 'args (format "length = %d" (nth 2 column))))
    (and (string= "CLOB" (nth 1 column))
      (add-to-list 'args "columnDefinition = \"CLOB\""))
    (and (string= "BLOB" (nth 1 column))
      (add-to-list 'args "columnDefinition = \"BLOB\""))
    ;; return args
    (and args (concat ", " (mapconcat #'identity args ", ")))))

(defun jh/java-column-header (colname)
  "Build additional header, like @Lob, @Basic(...) blabla."
  (let*
    ((column (assoc colname (jh/java-tabcols)))
      (colname (nth 0 column))
      (dbtype (nth 1 column)))
    (cond
      ((member dbtype '("CLOB" "BLOB"))
        "@Lob\n  @Basic(fetch = FetchType.LAZY)\n")
      (t ""))))

(defun jh/java-column-type (colname)
  "Get field type."
  (let*
    ((column (assoc colname (jh/java-tabcols)))
      (lookup
        (and column
          (assoc (cadr column) jh/dbtype-fields-type-alist))))
    (if lookup (cdr lookup) "void")))

(defun jh/java-column-comments (colname)
  "Get field comments."
  (let
    ((column (assoc colname (jh/java-tabcols))))
    (if column
      (concat " // "(car (last column)))
      " // TODO: Add Comment")))

(defun jh/java-column-field (colname)
  "Get field name."
  (and colname (jh/camelcase colname)))

;; -----------------------------------------------------------------------------
;; python helper function for yasnippets
;; -----------------------------------------------------------------------------
(defun jh/python-class-name (&optional file)
  "Return the class name for python."
  (let ((file (or file (buffer-file-name))))
    (jh/pascalcase (jh/file-base-name file))))

(provide 'init-yasnippet)
