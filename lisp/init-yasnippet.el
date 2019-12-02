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

(defun jh/java-repository-name-list ()
  "Get all repository names in the project."
  (when (spt/maven-project?)
    (hash-table-keys (spt/cache-of-class-in-project-if 'spt/repository?))))

(defun jh/java-service-name-list ()
  "Get all service names in the project."
  (when (spt/maven-project?)
    (hash-table-keys (spt/cache-of-class-in-project-if 'spt/service?))))

(defun jh/java-controller-doc-url ()
  "Return a full url, to put it as the header of the doc, like `GET /api/'."
  (when (spt/maven-project?)
    (let* ((cache (spt/cache-of-all-controller-api))
            (path (jh/relative-path (buffer-file-name) (spt/doc-root)))
            (signature (gethash path cache)))
      (if (null signature) "" (car signature)))))

(defun jh/java-inter-method-signature ()
  "Complete the signature of interface's method."
  (save-some-buffers t)
  (if (spt/implement? (buffer-file-name))
    (let* ((file (buffer-file-name))
            (cache-impl (spt/cache-of-impl-override-method file))
            (cache-inter (spt/cache-of-inter-method (spt/trans-impl-and-inter file)))
            (todo))
      (maphash
        (lambda (k v)
          (let ((value (gethash k cache-impl)))
            (if (null value)
              (setq todo (cons v todo)))))
        cache-inter)
      (if todo
        (mapcar (lambda (sign) (apply 'format "%s %s(%s)" sign)) todo)
        '("String toString()")))
    '("String toString()")))

(defun jh/java-get-local-tabinfo ()
  "Get tabinfo in current buffer."
  (interactive)
  (if (local-variable-p 'tabinfo) tabinfo
    (let* ((table (spt/extract-java-entity-table (jh/current-buffer)))
            (tabname (if table table
                       (completing-read "Load Table >> " (jh/java-table-names))))
            (columns (spt/cache-of-table-columns tabname)))
      (set (make-local-variable 'tabinfo) columns)
      tabinfo)))

(defun jh/java-table-names ()
  "Return all table name."
  (mapcar #'car (spt/query-all-tables)))

(defun jh/java-column-names ()
  "Return all column name."
  (let* ((file (buffer-file-name))
          (tabinfo (jh/java-get-local-tabinfo))
          (origin (hash-table-keys tabinfo))
          (common-filter '("SIGNED_CODE" "DATETIME" "VALIDATION" "MYID"))
          (file-filter (and (spt/entity? file)
                         (mapcar #'car
                           (hash-table-values (spt/cache-of-entity-fields file)))))
          (filter (append common-filter file-filter)))
    (and filter (remove-if (lambda (x) (member x filter)) origin))))

(defun jh/java-column-args (colname)
  "Build the arguments in @Column(...)"
  (let* ((tabinfo (jh/java-get-local-tabinfo))
          (col (gethash colname tabinfo))
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
  (let* ((tabinfo (jh/java-get-local-tabinfo))
          (col (gethash colname tabinfo))
          (colname (nth 0 col))
          (dbtype (nth 1 col)))
    (cond
      ((member dbtype '("CLOB" "BLOB"))
        "@Lob\n  @Basic(fetch = FetchType.LAZY)\n")
      (t ""))))

(defun jh/java-column-type (colname)
  "Get field type."
  (let* ((tabinfo (jh/java-get-local-tabinfo))
          (col (gethash colname tabinfo)))
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
  (let* ((tabinfo (jh/java-get-local-tabinfo))
          (col (gethash colname tabinfo)))
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
