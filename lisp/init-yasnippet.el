(when (require 'yasnippet)
  (setq
    yas-snippet-dirs '("~/.emacs.d/snippets")
    ;; yas-visit-from-menu t
    yas-indent-line 'auto)

  (yas-global-mode 1))


;; -----------------------------------------------------------------------------
;; helper function for yasnippets
;; -----------------------------------------------------------------------------
(defun jh/java-package-name (&optional file)
  "Return the package name for a java file."
  (let ((dir (jh/parent-dir (or file (buffer-file-name)))))
    (mapconcat 'identity
      (split-string
        (replace-regexp-in-string
          ".*src\\(/\\(main\\|test\\)\\)?\\(/java\\)?"
          "" dir) "/" t) ".")))

(defun jh/java-class-name (&optional file)
  "Return the class name for java."
  (let ((class (jh/filename-without-extension (or file (buffer-file-name)))))
    (jh/pascalcase class)))

(defun jh/java-test-subject-names (file)
  "Genearate test subject names."
  (when (spt/testcase? file)
    (let ((text (jh/read-file-content (spt/trans-test-and-source file))))
      (remove-duplicates
        (mapcar #'cadddr (spt/extract-java-class-methods text))
        :test 'equal))))

(defun jh/java-test-case-func-names ()
  "Generate test case name list."
  (let ((subjects (jh/java-test-subject-names (buffer-file-name))))
    (mapcar
      (lambda (name)
        (concat
          "test"
          (jh/pascalcase name)
          (format-time-string "%H%M%S")))
      subjects)))

(defun jh/java-whatever-to-entity-name (whatever)
  "Convert `*RepositoryImpl', `*Service' ... to `*'."
  (jh/pascalcase
    (replace-regexp-in-string
      "\\(RepositoryImpl\\|ServiceImpl\\|Repository\\|Service\\|Controller\\)$"
      "" whatever)))

(defun jh/java-controller-router (ctrl)
  "Return a url mapping from name."
  (let ((entity (jh/java-whatever-to-entity-name ctrl)))
    (concat "/"
      (mapconcat 'identity
        (mapcar #'jh/pluralize
          (split-string (jh/kebabcase entity) "-")) "/"))))

(defun jh/java-implement-name-to-interface-name (name)
  "Convert `*Impl' to `*'"
  (jh/pascalcase
    (replace-regexp-in-string "Impl$" "" name)))

(defun jh/java-interface-name-to-implement-name (name)
  "Convert `*' to `*Impl'."
  (interactive)
  (jh/pascalcase
    (if (string-match-p "^.*Impl$" name) name
      (concat name "Impl"))))

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
          (length (nth 2 col))
          (nullable (nth 3 col))
          (unique (nth 4 col))
          (nullable-arg
            (if (string= "N" nullable) ", nullable = false"  ""))
          (unique-arg
            (if (string= "U" unique) ", unique = true" ""))
          (length-arg
            (cond
              ((member dbtype '("CHAR" "NVARCHAR2" "VARCHAR" "VARCHAR2"))
                (concat ", length = " length))
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
      ((string= "BLOB" dbtype)
        (concat
          "@JsonIgnore\n"
          "  @Lob\n"
          "  @Basic(fetch = FetchType.LAZY)\n"))
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
          ((member dbtype '("NUMBER")) "long")
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

(provide 'init-yasnippet)
