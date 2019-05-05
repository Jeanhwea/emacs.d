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
(defun jh/java-package-name ()
  "Get the package name for java."
  (interactive)
  (mapconcat 'identity
    (split-string
      (replace-regexp-in-string
        ".*src\\(/\\(main\\|test\\)\\)?\\(/java\\)?"
        ""
        default-directory) "/" t) "."))

(defun jh/java-class-name ()
  "Get the class name for java."
  (interactive)
  (jh/pascalcase
    (file-name-nondirectory
      (file-name-sans-extension (buffer-file-name)))))

(defun jh/java-test-case-name ()
  "Generate test case name with random time string."
  (interactive)
  (concat
    "test"
    (replace-regexp-in-string "Test$" "" (jh/java-class-name))
    (format-time-string "%H%M%S")))

(defun jh/java-repo-to-entity (repo)
  "Guess entity name from repo"
  (interactive)
  (replace-regexp-in-string "Repository" "" repo))

(provide 'init-yasnippet)
