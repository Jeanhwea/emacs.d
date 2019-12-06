;; -----------------------------------------------------------------------------
;;  __  __      _        ___        __
;; |  \/  | ___| |_ __ _|_ _|_ __  / _| ___
;; | |\/| |/ _ \ __/ _` || || '_ \| |_ / _ \
;; | |  | |  __/ || (_| || || | | |  _| (_) |
;; |_|  |_|\___|\__\__,_|___|_| |_|_|  \___/
;; -----------------------------------------------------------------------------

(defun ng/project-root ()
  "Return current project root dir."
  (or (jh/git-root default-directory)
    (error "This file is not inside a GIT repository")))

(defun ng/src-root ()
  "Return current source root dir."
  (let
    ((dir
       (file-name-as-directory
         (expand-file-name "src" (ng/project-root)))))
    (or (file-exists-p dir)
      (error "Folder `src' is not exists!"))
    dir))

(defun ng/app-root (&optional entry)
  "Return current source root dir."
  (let
    ((dir
       (file-name-as-directory
         (expand-file-name "app" (ng/src-root)))))
    (or (file-exists-p dir)
      (error "Failed to get application root!"))
    dir))

;; -----------------------------------------------------------------------------
;;  ____                             _____ _ _
;; / ___|  ___  _   _ _ __ ___ ___  |  ___(_) | ___  ___
;; \___ \ / _ \| | | | '__/ __/ _ \ | |_  | | |/ _ \/ __|
;;  ___) | (_) | |_| | | | (_|  __/ |  _| | | |  __/\__ \
;; |____/ \___/ \__,_|_|  \___\___| |_|   |_|_|\___||___/
;; -----------------------------------------------------------------------------

(defun ng/coerce-to-prefix (file)
  "Force to convert to prefix."
  (let
    ((re "\\(\\.html\\|\\.less\\|\\.spec\\.ts\\|\\.ts\\)$"))
    (jh/re-replace re "" file)))

(defvar ng/filetype-suffix-alist
  '((model . ".ts")
     (view . ".html")
     (style . ".less")
     (test . ".spec.ts"))
  "File type suffix in angular project.")

(defvar ng/boi-list '(model view style)
  "Bundle of interest in angular project.")

(defun ng/current-filetype (file)
  "Return current filetype"
  (let
    ((lookup
       (remove-if-not
         #'(lambda (e)
             (string-match-p (concat (cdr e) "$") file))
         ng/filetype-suffix-alist)))
    (and lookup (caar lookup))))

(defun ng/find-alternative-file (filetype)
  "Find alternative filename with specific FILETYPE."
  (let
    ((prefix (ng/coerce-to-prefix (buffer-file-name)))
      (lookup (assoc filetype ng/filetype-suffix-alist)))
    (and lookup (concat prefix (cdr lookup)))))

(defun ng/switch-to (&optional filetype)
  "Switch to file type."
  (let
    ((alterfile (ng/find-alternative-file filetype)))
    (and (file-exists-p alterfile) (find-file alterfile))))

(defun ng/cycle-source-files ()
  "Cycling source files in file list."
  (interactive)
  (let*
    ((file (buffer-file-name))
      (current-filetype
        (ng/current-filetype file))
      (next-filetype
        (cadr (member current-filetype ng/boi-list))))
    (if current-filetype
      (if next-filetype
        (ng/switch-to next-filetype)
        (ng/switch-to (car ng/boi-list)))
      (error "Not a angular project type file: %s" file))))

(defun ng/source-files ()
  "List all typescript source files."
  (remove-if
    #'(lambda (f) (string-match-p "^.*\\.spec\\.ts$" f))
    (directory-files-recursively (ng/app-root) "^.*\\.ts$")))

(defun ng/find-source-file ()
  "Open source file."
  (interactive)
  (let*
    ((files (ng/source-files))
      (files-alist
        (mapcar
          #'(lambda (f)
              (cons (jh/file-base-name f) f))
          files))
      (lookup
        (completing-read
          "Goto source >> "
          (mapcar #'car files-alist)))
      (file (cdr (assoc lookup files-alist))))
    (progn
      (find-file file)
      (message (concat "Opened " file)))))

;; -----------------------------------------------------------------------------
;;  ____   _    ____  ____  _____ ____
;; |  _ \ / \  |  _ \/ ___|| ____|  _ \
;; | |_) / _ \ | |_) \___ \|  _| | |_) |
;; |  __/ ___ \|  _ < ___) | |___|  _ <
;; |_| /_/   \_\_| \_\____/|_____|_| \_\  for Typescript
;; -----------------------------------------------------------------------------

(defun ng/parse-ts-frontinfo (text)
  "Parse javascript front info. like `export class ClassName ...' "
  (let
    ((params (make-hash-table :test 'equal))
      (regexp
        (concat
          "^export\s+class\s+"
          "\\([_A-Za-z][_A-Za-z0-9]*\\)\s+"
          "\\(implements\\|\\)\s*"
          "\\([_A-Za-z][_A-Za-z0-9, ]*\\|\\)\s*"
          "{"))
      (addr 0))
    (save-match-data
      (setq addr (string-match regexp text addr))
      (and addr
        (and
          (let
            ((str1 (match-string 1 text))
              (str2 (match-string 2 text))
              (str3 (match-string 3 text)))
            (puthash 'clzname str1 params)
            (and (string= str2 "implements")
              (puthash 'implname
                (mapcar 'jh/trim (split-string str3 ",")) params))))))
    params))

(defun ng/parse-ts-class-methods (text)
  "Parse typescript class methods. like `public funcName(...) : rettype {' "
  (let
    ((regexp
       (concat
         "^  \\(public\\|private\\)\s*"
         "\\([_A-Za-z][_A-Za-z0-9]*\\)\s*"
         "(\\([^;{]*\\))\s*"
         "\\(:[^{]+\\|\\)"
         "{$"))
      (regexp2
        (concat
          "^  \\(ng[_A-Za-z]+\\|constructor\\)\s*"
          "(\\([^;{]*\\))\s*"
          "{"))
      (addr 0)
      (methods))
    (while addr
      (save-match-data
        (setq addr (string-match regexp text addr))
        (and addr
          ;; add a new method
          (let
            ((method (make-hash-table :test 'equal :size 10))
              (str1 (match-string 1 text))
              (str2 (match-string 2 text))
              (str3 (match-string 3 text))
              (str4 (match-string 4 text)))
            ;; put value
            (puthash 'visibility str1 method)
            (puthash 'funcname str2 method)
            (puthash 'args (jh/strip str3) method)
            (puthash 'return (jh/strip (jh/re-replace "^:" "" str4)) method)
            (puthash 'addr addr method)
            ;; append method to list
            (add-to-list 'methods method t))
          ;; next
          (setq addr (+ addr 1)))))
    (setq addr 0)
    (while addr
      (save-match-data
        (setq addr (string-match regexp2 text addr))
        (and addr
          ;; add a new method
          (let
            ((method (make-hash-table :test 'equal :size 10))
              (str1 (match-string 1 text))
              (str2 (match-string 2 text)))
            ;; put value
            (puthash 'funcname str1 method)
            (puthash 'args (jh/strip str2) method)
            (puthash 'addr addr method)
            ;; append method to list
            (add-to-list 'methods method t))
          ;; next
          (setq addr (+ addr 1)))))
    methods))

(defun ng/parse-ts-meta (text)
  "Parse typescript class meta info to hashtable."
  (let
    ((metainfo (ng/parse-ts-frontinfo text)))
    ;; parse class methods
    (puthash 'methods (ng/parse-ts-class-methods text) metainfo)
    metainfo))

(defun ng/current-method-name ()
  "Find current class method, the method name near cursor."
  (let*
    ((methods
       (sort
         (ng/parse-ts-class-methods (jh/current-buffer))
         #'(lambda (a b) (< (gethash 'addr a) (gethash 'addr b)))))
      (current-point (point))
      (lookup
        (remove-if
          #'(lambda (method) (<= current-point (gethash 'addr method)))
          methods)))
    (and lookup (gethash 'funcname (car (last lookup))))))

(provide 'init-angular)
