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
        (replace-regexp-in-string re "" dir) "/" t) ".")))

(defun jh/java-class-name (&optional file)
  "Return the class name for java."
  (let ((file (or file (buffer-file-name))))
    (jh/pascalcase (jh/filename-without-extension file))))

(defun jh/java-test-subjects (file)
  "Genearate test subject names."
  (and (string-match-p "Test$" (jh/java-class-name file))
    (let*
      ((srcfile (spt/coerce-to-srcfile file))
        (text (jh/read-file-content srcfile))
        (methods (spt/parse-java-class-methods text))
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
    (jh/pascalcase (replace-regexp-in-string re "" whatever))))

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
  (jh/pascalcase (replace-regexp-in-string "Impl$" "" name)))

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

(defun jh/java-tabnames ()
  "Return all table name."
  (mapcar #'car (spt/query-all-tables)))

(defun jh/java-tabcols ()
  "Get table columns for current buffer."
  (or (local-variable-p 'tabcols)
    (let
      ((tabname
         (or (spt/read-entity-tabname (jh/current-buffer))
           (completing-read "Load Table >> " (jh/java-tabnames)))))
      (set (make-local-variable 'tabcols) (spt/query-table-columns tabname))))
  tabcols)



(defun jh/java-column-names ()
  "Return all column name."
  (let* ((file (buffer-file-name))
          (tabcols (jh/java-tabcols))
          (origin (hash-table-keys tabcols))
          (common-filter '("SIGNED_CODE" "DATETIME" "VALIDATION" "MYID"))
          (file-filter (and (spt/entity? file)
                         (mapcar #'car
                           (hash-table-values (spt/cache-of-entity-fields file)))))
          (filter (append common-filter file-filter)))
    (and filter (remove-if (lambda (x) (member x filter)) origin))))

(defun jh/java-column-args (colname)
  "Build the arguments in @Column(...)"
  (let* ((tabcols (jh/java-tabcols))
          (col (gethash colname tabcols))
          (colname (nth 0 col))
          (dbtype (nth 1 col))
          (dblen (nth 2 col))
          (nullable (nth 3 col))
          (unique (nth 4 col))
          (nullable-arg
            (if (string= "N" nullable) ", nullable = false"  ""))
          (unique-arg
            (if (string= "U" unique) ", unique = true" ""))
          (length-arg
            (cond
              ((member dbtype '("CHAR" "NVARCHAR2" "VARCHAR" "VARCHAR2"))
                (concat ", length = " dblen))
              (t "")))
          (addition-arg
            (cond
              ((string= "BLOB" dbtype) ", columnDefinition = \"BLOB\"")
              ((string= "CLOB" dbtype) ", columnDefinition = \"CLOB\"")
              (t ""))))
    (concat nullable-arg unique-arg length-arg addition-arg)))

(defun jh/java-column-header (colname)
  "Build additional header, like @Lob, @Basic(...) blabla."
  (let* ((tabcols (jh/java-tabcols))
          (col (gethash colname tabcols))
          (colname (nth 0 col))
          (dbtype (nth 1 col)))
    (cond
      ((member dbtype '("CLOB" "BLOB"))
        "@Lob\n  @Basic(fetch = FetchType.LAZY)\n")
      (t ""))))

(defun jh/java-column-type (colname)
  "Get field type."
  (let* ((tabcols (jh/java-tabcols))
          (col (gethash colname tabcols)))
    (and col
      (let ((dbtype (nth 1 col)))
        (cond
          ((member dbtype '("BLOB")) "byte[]")
          ((member dbtype '("CHAR" "CLOB" "NVARCHAR2" "VARCHAR" "VARCHAR2")) "String")
          ((member dbtype '("DATE")) "Timestamp")
          ((member dbtype '("NUMBER")) "double")
          (t "void"))))))

(defun jh/java-column-comments (colname)
  "Get field comments."
  (let* ((tabcols (jh/java-tabcols))
          (col (gethash colname tabcols)))
    (and col
      (let ((comments (nth 5 col)))
        (if (string= comments "") ""
          (concat " // " comments))))))

(defun jh/java-column-field (colname)
  "Get field name."
  (and colname (jh/camelcase colname)))

;; -----------------------------------------------------------------------------
;; python helper function for yasnippets
;; -----------------------------------------------------------------------------
(defun jh/python-class-name (&optional file)
  "Return the class name for java."
  (let ((class (jh/filename-without-extension (or file (buffer-file-name)))))
    (jh/pascalcase class)))

(provide 'init-yasnippet)
