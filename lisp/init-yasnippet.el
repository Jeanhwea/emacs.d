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
  (setq dir (jh/parent-dir (or file (buffer-file-name))))
  (mapconcat 'identity
    (split-string
      (replace-regexp-in-string
        ".*src\\(/\\(main\\|test\\)\\)?\\(/java\\)?"
        "" dir) "/" t) "."))

(defun jh/java-class-name (&optional file)
  "Return the class name for java."
  (interactive)
  (jh/pascalcase (jh/filename-without-extension file)))

(defun jh/java-test-case-name ()
  "Generate test case name with random time string."
  (interactive)
  (concat
    "test"
    (replace-regexp-in-string "Test$" "" (jh/java-class-name))
    (format-time-string "%H%M%S")))

(defun jh/java-test-case-name-list ()
  "Generate test case name list."
  (interactive)
  (setq file (buffer-file-name))
  (when (spt/testcase? file)
    (let* ((source-file (spt/trans-test-and-source file))
            (methods (remove-if 'null (mapcar #'spt/extract-java-method (jh/read-file-content-as-lines source-file))))
            (methods-names (remove-duplicates (mapcar 'caddr methods) :test 'equal)))
      (mapcar #'(lambda (name) (concat "test" (jh/pascalcase name) (format-time-string "%H%M%S"))) methods-names))))

(defun jh/java-whatever-to-entity-name (whatever)
  "Convert `*RepositoryImpl', `*Service' ... to `*'."
  (interactive)
  (jh/pascalcase
    (replace-regexp-in-string
      "\\(RepositoryImpl\\|ServiceImpl\\|Repository\\|Service\\|Controller\\)$"
      "" whatever)))

(defun jh/java-controller-base-url-mapping (ctrl)
  "Return a url mapping from name."
  (let ((entity (jh/java-whatever-to-entity-name ctrl)))
    (concat "/"
      (mapconcat 'identity
        (mapcar #'jh/pluralize
          (split-string (jh/kebabcase entity) "-")) "/"))))

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
