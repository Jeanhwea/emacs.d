;; -----------------------------------------------------------------------------
;; system predictor
;; -----------------------------------------------------------------------------
(defun jh/windows? ()
  "test if system-type is windows?"
  (string-equal "windows-nt" system-type))
(defun jh/mac? ()
  "test if system-type is mac?"
  (string-equal "darwin" system-type))

;; -----------------------------------------------------------------------------
;; String, convert shape
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
  "convert string to `FOO_BAR' format"
  (string-inflection-upcase-function str))

(defun jh/kebabcase (str)
  "convert string to `foo-bar' format"
  (string-inflection-kebab-case-function str))

(defun jh/capital-underscore (str)
  "convert string to `Foo_Bar' format"
  (string-inflection-capital-underscore-function str))

;; -----------------------------------------------------------------------------
;; String, dictionary related
;; -----------------------------------------------------------------------------
(defvar noun-cache (make-hash-table :test 'equal)
  "store all noun in english.")

(defun jh/noun-p (word)
  "Return ture if the WORD is a noun."
  (unless (gethash "apple" noun-cache)
    (let ((noun-file (expand-file-name "lang/nouns.txt" user-emacs-directory)))
      (dolist (line (jh/read-file-content-as-lines noun-file))
        (unless (null line)
          (puthash line 't noun-cache)))))
  (gethash word noun-cache))

(defvar pluralism-rule
  '(
     ;; irregular words
     ("child$" . "children")
     ("man$" . "men")
     ("person$" . "people")
     ("tooth$" . "teeth")
     ;; common rule
     ("ch$" . "ches")
     ("ief$" . "ieves")
     ("ife$" . "ives")
     ("lf$" . "lves")
     ("sh$" . "shes")
     ("ss$" . "sses")
     ("um" . "a")
     ("\\([^aeiou]\\)y$" . "\\1ies")
     ("[aeiou]y$" . "\\&s")
     ;; default append "s"
     ("[^s]$" . "\\&s"))
  "Define the rule of pluralizing noun.")

(defun jh/pluralize-cond-test (re str)
  (let ((is-lower (equal (downcase re) re)))
    (let ((case-fold-search is-lower))
      (string-match re str))))

(defun jh/pluralize (noun)
  "Return a plural of NOUN."
  (when (jh/noun-p noun)
    (let ((plural-replace
            (assoc-default noun pluralism-rule 'jh/pluralize-cond-test)))
      (if (null plural-replace) noun
        (replace-match plural-replace t nil noun)))))

;; (jh/pluralize "bus")
;; (jh/pluralize "city")
;; (jh/pluralize "boy")

;; -----------------------------------------------------------------------------
;; String, helper
;; -----------------------------------------------------------------------------
(defun jh/trim-blank (str)
  "Remove trailing spaces and tabs in a string."
  (and str (string-trim (replace-regexp-in-string "[ \t\n]+" " " str))))


;; -----------------------------------------------------------------------------
;; file and directory helper
;; -----------------------------------------------------------------------------
(defun jh/absolute-path (file)
  "Return absolute path of FILE."
  (let ((abs (expand-file-name file)))
    (cond
      ((file-directory-p abs)
        (file-name-as-directory abs))
      (t abs))))

(defun jh/relative-path (file dir)
  "Return a relative path of FILE to DIR."
  (let ((abs-file (jh/absolute-path file))
         (abs-dir (jh/absolute-path dir)))
    (when (file-in-directory-p abs-file abs-dir)
      (replace-regexp-in-string abs-dir "" abs-file))))

(defun jh/parent-dir (file)
  "Return the parent directory of FILE."
  (file-name-directory
    (file-name-directory
      (directory-file-name (expand-file-name file)))))

(defun jh/root-dir-p (dir)
  "Return ture if DIR is a root directory"
  (let ((path (jh/absolute-path dir)))
    (string-equal path (jh/parent-dir path))))

(defun jh/directory-sequence-recursively (dirs)
  "Return a list of dir, dir's parent, dir's great parent and more, which dir is the head of dirs."
  (let ((dir (car dirs)))
    (if (jh/root-dir-p dir) dirs
      (jh/directory-sequence-recursively (cons (jh/parent-dir dir) dirs)))))

(defun jh/directory-sequence (dir)
  "Return a list of dir, dir's parent, dir's great parent and more."
  (reverse
    (jh/directory-sequence-recursively
      (list (jh/absolute-path dir)))))

(defun jh/filename-without-extension (file)
  "Return the file name without extension."
  (file-name-nondirectory (file-name-sans-extension file)))

;; -----------------------------------------------------------------------------
;; file content and string reader
;; -----------------------------------------------------------------------------
(defun jh/read-file-content (file)
  "Read a file content, and put all into a list of lines."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun jh/read-file-content-as-lines (file)
  "Read a file content, and put all into a list of lines."
  (split-string (jh/read-file-content file) "\n" t))

(defun jh/current-line ()
  "Read content of current line."
  (string-trim (thing-at-point 'line t)))

(defun jh/current-buffer ()
  "Read content of current buffer."
  (jh/read-file-content (buffer-file-name)))

;; -----------------------------------------------------------------------------
;; setup timer
;; -----------------------------------------------------------------------------
(defun jh/run-at-time-if-possible (timestr func)
  "If timestr is not out of date, then setup a callback function"
  (let ((now (current-time))
         (start (date-to-time (format-time-string (concat "%Y-%m-%d " timestr)))))
    (when (time-less-p now start)
      (run-at-time timestr nil func))))

(provide 'init-util)
