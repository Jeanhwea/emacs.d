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

(defun fw/find-file (file)
  "Open a FILE."
  (find-file file))

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
  "Return the entity name from a file name."
  (when (string-match-p ".*\\(controller\\|entity\\|repo\\|service\\)/.*\\.java" file)
    (jh/pascalcase
      (replace-regexp-in-string
        "\\(RepositoryImpl\\|ServiceImpl\\|Repository\\|Service\\|Controller\\)$"
        "" (fw/file-name-to-class-name file)))))

(defun fw/file-name-to-implement-name (file)
  "Return the implement name from a file name."
  (let ((class (fw/file-name-to-class-name file)))
    (when (string-match-p "^.*\\(Repository\\|Service\\)$" class)
      (concat class "Impl"))))

(defvar fw/all-components-cache (make-hash-table :test 'equal)
  "Cached all components in this project.")

(defvar fw/all-implements-cache (make-hash-table :test 'equal)
  "Cached all implements in this project.")

(defun fw/scan-all-components (dirs)
  "Return a list that contains all components in the project."
  (progn
    (clrhash fw/all-components-cache)
    (dolist (file (fw/directory-files dirs))
      (let ((class (fw/file-name-to-class-name file)))
        (unless (null (fw/file-name-to-entity-name file))
          (puthash class file fw/all-components-cache))))))

(defun fw/scan-all-implements (dirs)
  "Return a list that contains all implements in the project."
  (progn
    (clrhash fw/all-implements-cache)
    (dolist (file (fw/directory-files dirs))
      (let ((class (fw/file-name-to-class-name file)))
        (when (string-match-p "^.*Impl$" class)
          (puthash class file fw/all-implements-cache))))))

(defun fw/trans-file-name (file path formula)
  "Transfer file to given relative path, with a formula, like `%sRepository.java'."
  (let ((entity (fw/file-name-to-entity-name file))
         (folder (expand-file-name path (fw/parent-dir file))))
    (expand-file-name (replace-regexp-in-string "%s" entity formula) folder)))

(defun fw/switch-to-component-file (path formula)
  "Switch to a component file in the project."
  (let ((entity (fw/file-name-to-entity-name (buffer-file-name))))
    (progn
      (fw/scan-all-components (fw/maven-project-source-dirs))
      (unless (null entity)
        (fw/find-file
          (fw/trans-file-name
            (gethash entity fw/all-components-cache) path formula))))))

(defun fw/switch-to-entity-file ()
  "Switch to entity file."
  (interactive)
  (let ((entity (fw/file-name-to-entity-name (buffer-file-name))))
    (progn
      (fw/scan-all-components (fw/maven-project-source-dirs))
      (unless (null entity)
        (fw/find-file (gethash entity fw/all-components-cache))))))

(defun fw/switch-to-repository-file ()
  "Switch to repository file."
  (interactive)
  (fw/switch-to-component-file "../repo" "%sRepository.java"))

(defun fw/switch-to-service-file ()
  "Switch to service file."
  (interactive)
  (fw/switch-to-component-file "../../service" "%sService.java"))

(defun fw/switch-to-controller-file ()
  "Switch to controller file."
  (interactive)
  (fw/switch-to-component-file "../../controller" "%sController.java"))

(defun fw/swap-interface-and-implemention (file)
  "Swap interface and implemention name."
  (let ((class (fw/file-name-to-class-name file)))
    (cond
      ((string-match-p "^.*Impl$" class)
        (expand-file-name
          (concat (replace-regexp-in-string "Impl$" "" class) ".java")
          (fw/parent-dir (fw/parent-dir file))))
      ((string-match-p "^.*\\(Repository\\|Service\\)$" class)
        (expand-file-name
          (concat class "Impl.java")
          (expand-file-name "impl" (fw/parent-dir file))))
      (t nil))))

(defun fw/toggle-interface-and-implemention ()
  "Toggle interface file and implemention."
  (interactive)
  (fw/find-file (fw/swap-interface-and-implemention (buffer-file-name))))

;; (mapcar #'fw/file-name-to-class-name (fw/directory-files '("~/Code/work/avic/skree/src")))
;; (mapcar #'fw/file-name-to-entity-name (fw/directory-files '("~/Code/work/avic/skree/src")))
;; (fw/file-name-to-entity-name (car (fw/directory-files '("~/Code/work/avic/skree/src"))))
;; (fw/scan-all-components '("~/Code/work/avic/skree/src"))
;; (fw/scan-all-implements '("~/Code/work/avic/skree/src"))
;; (fw/scan-all-components nil)
;; (gethash "Employee" fw/all-components-cache)
;; (find-file (gethash "Employee" fw/all-components-cache))
;; (fw/trans-file-name (gethash "Employee" fw/all-components-cache) "../repo" "%sRepository.java")
;; (print fw/all-components-cache)
;; (print fw/all-implements-cache)

(defun fw/maven-project-source-dirs ()
  "Return the maven project source folder."
  (setq srcdir (expand-file-name "src" (fw/project-root)))
  (when (file-directory-p srcdir) (cons srcdir nil)))

;; (fw/maven-project-source-dirs)

;; -----------------------------------------------------------------------------
;; key bindings
;; -----------------------------------------------------------------------------
(progn
  (define-prefix-command 'fw/leader-key-map)
  (define-key fw/leader-key-map (kbd "c") 'fw/switch-to-controller-file)
  (define-key fw/leader-key-map (kbd "i") 'fw/toggle-interface-and-implemention)
  (define-key fw/leader-key-map (kbd "r") 'fw/switch-to-repository-file)
  (define-key fw/leader-key-map (kbd "s") 'fw/switch-to-service-file)
  (define-key fw/leader-key-map (kbd "t") 'projectile-toggle-between-implementation-and-test)
  (define-key fw/leader-key-map (kbd "RET") 'jh/java-try-import-class)
  (define-key fw/leader-key-map (kbd "e") 'fw/switch-to-entity-file))
(global-set-key (kbd "M-RET") 'fw/leader-key-map)

(provide 'init-framework)
