(when (require 'yasnippet)
  (setq
    yas-snippet-dirs '("~/.emacs.d/snippets")
    ;; yas-visit-from-menu t
    yas-indent-line 'auto)

  (yas-global-mode 1))


;; -----------------------------------------------------------------------------
;; string helper, using string-inflection
;; -----------------------------------------------------------------------------
(defun jh/underscore (str)
  "convert string to `foo_bar' format"
  (string-inflection-underscore-function str))

(defun jh/pascalcase (str)
  "convert string to `FooBar' format"
  (string-inflection-pascal-case-function str))

(defun jh/camelcase (str)
  "convert string to `fooBar' format"
  (string-inflection-camelcase-function str))

(defun jh/upcase (str)
  "convert string to `FooBar' format"
  (string-inflection-upcase-function str))

(defun jh/kebabcase (str)
  "convert string to `foo-bar' format"
  (string-inflection-kebab-case-function str))

(defun jh/capital-underscore (str)
  "convert string to `Foo_Bar' format"
  (string-inflection-capital-underscore str))


;; -----------------------------------------------------------------------------
;; helper function for yasnippets
;; -----------------------------------------------------------------------------
(defun jh/file-name (&optional file)
  "Return the file name without extension."
  (setq file (or file (buffer-file-name)))
  (file-name-nondirectory (file-name-sans-extension file)))

(defun jh/parent-dir (file)
  "Return parent directory of the FILE."
  (unless (null file)
    (directory-file-name
      (file-name-directory
        (directory-file-name (expand-file-name file))))))

(defun jh/read-file-lines (file)
  "Read a file content, and put all into a list of lines."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun jh/java-project-source-files (&optional file)
  "Return a list of `*.java' files in the project which contains the FILE."
  (setq file (or file (buffer-file-name)))
  (let ((project-src-dir
          (when (string-match ".*src\\(/\\(main\\|test\\)\\)?\\(/java\\)?" file)
            (expand-file-name
              (replace-regexp-in-string
                "/\\(main\\|test\\)/java/.*$" "" file)))))
    (unless (null project-src-dir)
      (directory-files-recursively project-src-dir "^.*\.java$"))))

(defun jh/extract-java-package-class (line)
  "Extract package name and class name from line."
  (save-match-data
    (and (string-match "^import \\([^;]*\\)\\.\\([a-zA-Z0-9]*\\);$" line)
      (setq package (match-string 1 line)
        class (match-string 2 line))
      (list class package))))

(defun jh/read-java-imported-class (file)
  "Read imported class in the FILES, then put them into a cache."
  (let ((class-cache (make-hash-table :test 'equal)))
    (dolist (java-src-file (jh/java-project-source-files file))
      (puthash (jh/java-class-name file) (jh/java-package-name file) class-cache)
      (dolist (ele (mapcar #'jh/extract-java-package-class (jh/read-file-lines java-src-file)))
        (unless (null ele) (puthash (car ele) (cadr ele) class-cache))))
    class-cache))

(defun jh/java-class-need-imported-p (clz &optional file)
  "Return ture when the class is needed imported."
  (setq file (or file (buffer-file-name)))
  (let ((class-cache (make-hash-table :test 'equal)))
    (dolist (ele (mapcar #'jh/extract-java-package-class (jh/read-file-lines file)))
      (unless (null ele) (puthash (car ele) (cadr ele) class-cache)))
    (null (gethash clz class-cache))))

;; (remove-if 'null (mapcar #'jh/extract-java-package-class (jh/read-file-lines "/Users/hujinghui/Code/work/avic/skree/src/main/java/com/avic/mti/skree/user/controller/EmployeeController.java")))
;; (gethash "Employee" (jh/read-java-imported-class "/Users/hujinghui/Code/work/avic/skree/src/main/java/com/avic/mti/skree/user/controller/EmployeeController.java"))
;; (jh/java-class-need-imported-p "EmployeeType" "/Users/hujinghui/Code/work/avic/skree/src/main/java/com/avic/mti/skree/user/controller/EmployeeController.java")
;; (jh/java-class-need-imported-p "List" "/Users/hujinghui/Code/work/avic/skree/src/main/java/com/avic/mti/skree/user/controller/EmployeeController.java")
;; (jh/java-class-need-imported-p "Employee" "/Users/hujinghui/Code/work/avic/skree/src/main/java/com/avic/mti/skree/user/controller/EmployeeController.java")

(defun jh/java-package-name (&optional file)
  "Return the package name for a java file."
  (setq dir (jh/parent-dir (or file (buffer-file-name))))
  (mapconcat 'identity
    (split-string
      (replace-regexp-in-string
        ".*src\\(/\\(main\\|test\\)\\)?\\(/java\\)?"
        "" dir) "/" t) "."))

(defun jh/java-class-name (&optional file)
  "Return the class name for java."
  (interactive)
  (jh/pascalcase (jh/file-name file)))

(defun jh/java-import-package-statement (clz &optional file)
  "Return the importing java package statement."
  (setq file (or file (buffer-file-name)))
  (let ((package (gethash clz (jh/read-java-imported-class file))))
    (unless (null package) (format "import %s.%s;" package clz))))

(defun jh/java-try-import-class (&optional clz)
  "Try import class"
  (interactive)
  (setq clz (or clz (word-at-point)))
  (save-buffer)
  (let ((stmt (jh/java-import-package-statement clz)))
    (when (and (jh/java-class-need-imported-p clz) (not (null stmt)))
      (save-excursion
        (progn
          (beginning-of-buffer)
          (next-line)
          (newline)
          (insert stmt))))))
(add-hook 'java-mode-hook (lambda () (local-set-key (kbd "M-RET") 'jh/java-try-import-class)))

;; (jh/java-import-package-statement "EmployeeRepository" "/Users/hujinghui/Code/work/avic/skree/src/main/java/com/avic/mti/skree/user/controller/EmployeeController.java")
;; (jh/java-import-package-statement "Logger" "/Users/hujinghui/Code/work/avic/skree/src/main/java/com/avic/mti/skree/user/controller/EmployeeController.java")

(defun jh/java-test-case-name ()
  "Generate test case name with random time string."
  (interactive)
  (concat
    "test"
    (replace-regexp-in-string "Test$" "" (jh/java-class-name))
    (format-time-string "%H%M%S")))

(defun jh/java-whatever-to-entity-name (whatever)
  "Convert `*RepositoryImpl', `*Service' ... to `*'."
  (interactive)
  (jh/pascalcase
    (replace-regexp-in-string
      "\\(RepositoryImpl\\|ServiceImpl\\|Repository\\|Service\\|Controller\\)$"
      "" whatever)))

(defun jh/java-implement-name-to-interface-name (name)
  "Convert `*Impl' to `*'"
  (interactive)
  (jh/pascalcase
    (replace-regexp-in-string "Impl$" "" name)))

(defun jh/java-interface-name-to-implement-name (name)
  "Convert `*' to `*Impl'."
  (interactive)
  (jh/pascalcase
    (if (string-match-p "^.*Impl$" name) name
      (concat name "Impl"))))

(provide 'init-yasnippet)
