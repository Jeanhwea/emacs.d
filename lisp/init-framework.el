(defun fw/filename-without-extension ()
  "Return the file name without extension."
  (file-name-nondirectory
    (file-name-sans-extension
      (buffer-file-name))))

(defun fw/expand-directory (path)
  "Return absolute path of PATH."
  (directory-file-name (expand-file-name path)))

(defun fw/parent-dir (path)
  "Return the parent directory of PATH."
  (directory-file-name (file-name-directory (directory-file-name (expand-file-name path)))))

(defun fw/root-dir-p (path)
  "Return ture if PATH is a root directory"
  (let ((dir (fw/expand-directory path)))
    (string-equal dir (fw/parent-dir dir))))

(defun fw/git-project-root-dir (path)
  "Return the git root dir of PATH."
  (if (fw/git-project-root-p path) path
    (if (fw/root-dir-p path) nil
      (fw/git-project-root-dir
        (fw/parent-dir path)))))

(defun fw/git-project-root-p (dir)
  "Return ture if DIR is contains `.git'."
  (file-directory-p
    (fw/expand-directory
      (expand-file-name ".git" dir))))

(defun fw/project-root (&optional directory)
  "Return current project root dir."
  (setq directory (or directory default-directory))
  (fw/git-project-root-dir directory))

(defun fw/file-relative-name (filename &optional directory)
  "Convert FILENAME to be relative to DIRECTORY (default: `default-directory')."
  (setq dir (expand-file-name (or directory default-directory)))
  (setq name (expand-file-name filename))
  (replace-regexp-in-string "^/" ""
    (replace-regexp-in-string dir "" name)))

(defun fw/flatten-dirs-list (dirs-list)
  "flatten a nested list."
  (let (value)
    (dolist (elt dirs-list value)
      (setq value (concatenate 'list value elt)))))

(defun fw/java-files (dir)
  "Return a list of `*.java' files."
  (directory-files-recursively dir "^.*\.java$"))

(defun fw/directory-files (dirs)
  "Return a list of interested files."
  (fw/flatten-dirs-list (mapcar #'fw/java-files dirs)))

(defun fw/file-name-to-class-name (file)
  "Return the class name from a file name."
  (file-name-nondirectory (file-name-sans-extension file)))

(defun fw/file-name-to-entity-name (file)
  "Return the entity name from a file name"
  (when (string-match-p ".*entity/.*\\.java" file)
    (jh/pascalcase
      (replace-regexp-in-string
        "\\(RepositoryImpl\\|ServiceImpl\\|Repository\\|Service\\|Controller\\)$"
        "" (fw/file-name-to-class-name file)))))

(defvar fw/all-entities-cache (make-hash-table :test 'equal)
  "Cached all entities in this project.")

(defun fw/scan-all-entities (dirs)
  "Return a list that contains all entities in the project."
  (progn
    (clrhash fw/all-entities-cache)
    (dolist (file (fw/directory-files dirs))
      (let ((entity (fw/file-name-to-entity-name file)))
        (unless (null entity)
          (puthash entity file fw/all-entities-cache))))))


;; (mapcar #'fw/file-name-to-class-name (fw/directory-files '("~/Code/work/avic/skree/src")))
;; (mapcar #'fw/file-name-to-entity-name (fw/directory-files '("~/Code/work/avic/skree/src")))
;; (fw/file-name-to-entity-name (car (fw/directory-files '("~/Code/work/avic/skree/src"))))
;; (fw/scan-all-entities '("~/Code/work/avic/skree/src"))
;; (fw/scan-all-entities nil)
;; (gethash "Employee" fw/all-entities-cache)
;; (print fw/all-entities-cache)

(defun fw/maven-project-source-dirs ()
  "Return the maven project source folder."
  (setq srcdir (expand-file-name "src" (fw/project-root)))
  (when (file-directory-p srcdir) (cons srcdir nil)))

;; (fw/maven-project-source-dirs)

(provide 'init-framework)
