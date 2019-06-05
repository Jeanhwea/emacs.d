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
  (mapcar #'cadr (spt/query-all-table)))

(defun jh/java-column-names ()
  "Return all column name."
  (let ((tabinfo (jh/java-get-local-tabinfo)))
    (hash-table-keys tabinfo)))

(defun jh/java-column-args (colname)
  "Build the arguments in @Column(...)"
  (let* ((tabinfo (jh/java-get-local-tabinfo))
          (desc (gethash colname tabinfo))
          (type (nth 0 desc))
          (null (nth 3 desc))
          (length (nth 4 desc))
          (nullable-arg (if null (concat ", nullable = " null) ""))
          (length-arg
            (if length
              (cond
                ((string= "String" type) (concat ", length = " length))
                (t "")) ""))
          (addition-arg
            (cond
              ((string= "byte[]" type) ",columnDefinition = \"BLOB\"")
              (t ""))))
    (concat nullable-arg length-arg addition-arg)))

(defun jh/java-column-type (colname)
  "Get field type."
  (let* ((tabinfo (jh/java-get-local-tabinfo))
          (desc (gethash colname tabinfo)))
    (and desc (car desc))))

(defun jh/java-column-field (colname)
  "Get field name."
  (let* ((tabinfo (jh/java-get-local-tabinfo))
          (desc (gethash colname tabinfo)))
    (and desc (cadr desc))))

(provide 'init-yasnippet)
