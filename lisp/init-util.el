;; -----------------------------------------------------------------------------
;; system predictor
;; -----------------------------------------------------------------------------
(defun jh/windows? ()
  "test if system-type is windows?"
  (equal 'windows-nt system-type))

(defun jh/mac? ()
  "test if system-type is mac?"
  (equal 'darwin system-type))

(defun jh/linux? ()
  "test if system-type is linux?"
  (equal 'gnu/linux system-type))

(defun jh/bsd? ()
  "test if system-type is berkeley-unix?"
  (equal 'berkeley-unix system-type))

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

(defalias 'jh/re-replace 'replace-regexp-in-string)

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
(defun jh/strip (str)
  "Remove all trailing spaces and tabs inside STR."
  (and str (string-trim (jh/re-replace "[ \t\n]+" " " str))))

(defun jh/trim (str)
  "Remove trailing spaces and tabs at the begining and end of STR."
  (and str (string-trim str)))

(defun jh/unblank (str)
  "Remove spaces and tabs in STR."
  (and str (jh/re-replace "\s*" "" str)))

(defun jh/concat-lines (&rest lines)
  "Concatenate lines to a single string."
  (mapconcat 'identity lines "\n"))

;; -----------------------------------------------------------------------------
;; file and directory helper
;; -----------------------------------------------------------------------------
(defun jh/absolute-path (file)
  "Return absolute path of FILE."
  (let ((abs (expand-file-name file)))
    (if (file-directory-p abs) (file-name-as-directory abs) abs)))

(defun jh/relative-path (file dir)
  "Return a relative path of FILE to DIR."
  (let ((absfile (jh/absolute-path file)) (absdir (jh/absolute-path dir)))
    (or (file-directory-p dir) (error "DIR should be a directory"))
    (file-relative-name absfile absdir)))

(defun jh/parent-dir (file)
  "Return the parent directory of FILE."
  (file-name-directory
    (file-name-directory
      (directory-file-name (expand-file-name file)))))

(defun jh/root-dir-p (dir)
  "Return ture if DIR is a root directory"
  (let ((path (jh/absolute-path dir)))
    (string= path (jh/parent-dir path))))

(defun jh/list-files-in-directory (dir)
  "List all file in current directory."
  (remove-if #'file-directory-p
    (mapcar #'(lambda (f) (expand-file-name f dir)) (directory-files dir))))

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

(defun jh/file-base-name (file)
  "Return the file name without extension."
  (file-name-nondirectory (file-name-sans-extension file)))

(defun jh/cycle-next-file (prev files)
  "Cycle to next file"
  (let ((curr (car files)))
    (if (string= prev curr) curr
      (jh/cycle-next-file curr (cdr files)))))

(defun jh/cycle-buffer-next-file ()
  "Cycle to next file related current buffer."
  (let ((curr (buffer-file-name))
         (files (jh/list-files-in-directory default-directory)))
    (message (jh/cycle-next-file curr files))))

;; -----------------------------------------------------------------------------
;; File related helper
;; -----------------------------------------------------------------------------
(defun jh/line-number ()
  "Return current line number."
  (line-number-at-pos))

(defun jh/read-file-content (file)
  "Read a file content, and put all into a list of lines."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun jh/read-file-content-as-lines (file)
  "Read a file content, and put all into a list of lines."
  (split-string (jh/read-file-content file) "\n" t))

(defun jh/current-buffer ()
  "Read content of current buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun jh/current-buffer-lines ()
  "Read content of current buffer as line."
  (split-string (jh/current-buffer) "\n" t))

(defun jh/current-line ()
  "Read content of current line."
  (string-trim (thing-at-point 'line t)))

(defun jh/current-selection ()
  "Read current selected text."
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun jh/save-variable (var filename)
  "Save variable to a FILE."
  (let ((file (expand-file-name filename
                (expand-file-name "var" user-emacs-directory)))
         (newbuf (generate-new-buffer filename)))
    (with-current-buffer newbuf
      (prin1 var (current-buffer))
      (write-file file)
      (kill-buffer newbuf))))

;; useful helper function
(defun jh/symbol-at-point ()
  "Read symbol and selection at point."
  (if (use-region-p)
    (let
      ((beg (region-beginning)) (end (region-end)))
      (deactivate-mark)
      (buffer-substring-no-properties beg end))
    (let
      ((sym (symbol-at-point)))
      (and sym (symbol-name sym)))))

(defun jh/indent-current-buffer ()
  "Indent current buffer."
  (save-excursion (indent-region (point-min) (point-max))))

(defun jh/project-name ()
  "Return the project name."
  (let*
    ((dir default-directory)
      (root (directory-file-name (or (jh/git-root dir) dir)))
      (parent (regexp-quote (jh/parent-dir root))))
    (jh/re-replace parent "" root nil 'literal)))

(defun jh/shrimp-shell-name ()
  "Return the project name for shrimp shell name."
  (let*
    ((dir default-directory)
      (root (directory-file-name (or (jh/git-root dir) dir)))
      (parent (jh/parent-dir root))
      (parent2 (regexp-quote (jh/parent-dir parent))))
    (jh/re-replace parent2 "" root nil 'literal)))

;; -----------------------------------------------------------------------------
;; setup timer
;; -----------------------------------------------------------------------------
(defun jh/run-at-time-if-possible (timestr func)
  "If timestr is not out of date, then setup a callback function"
  (let ((now (current-time))
         (start (date-to-time (format-time-string (concat "%Y-%m-%d " timestr)))))
    (when (time-less-p now start)
      (run-at-time timestr nil func))))

;; -----------------------------------------------------------------------------
;; clipboard
;; -----------------------------------------------------------------------------
(defun jh/sent-to-clipboard (text)
  "Sent text to clipboard."
  (kill-new text))

;; -----------------------------------------------------------------------------
;; some random function
;; -----------------------------------------------------------------------------
(defvar jh/alphabet "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  "Random alphabet list.")

(defvar jh/alphabet-length (length jh/alphabet)
  "Length of alphabet list.")

(defun jh/shortid (len)
  "Generate a shortid with length of LEN."
  (let ((randfn #'(lambda (x) (random jh/alphabet-length)))
         (strfn #'(lambda (x) (substring jh/alphabet x (1+ x)))))
    (mapconcat strfn (mapcar randfn (number-sequence 1 len)) "")))

(defun jh/now ()
  "Now as date string."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time)))

(provide 'init-util)
