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
  (directory-file-name
    (file-name-directory
      (directory-file-name
        (expand-file-name path)))))

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

(defun fw/project-root ()
  "Return current project root dir."
  (fw/git-project-root-dir default-directory))

(defun fw/file-relative-name (filename &optional directory)
  "Convert FILENAME to be relative to DIRECTORY (default: `default-directory')."
  (setq dir (expand-file-name (or directory default-directory)))
  (setq name (expand-file-name filename))
  (replace-regexp-in-string "^/" ""
    (replace-regexp-in-string dir "" name)))

(defun fw/maven-project-source-dirs ()
  "Return the maven project source folder."
  (setq srcdir (expand-file-name "src" (fw/project-root)))
  (if (file-directory-p srcdir) (cons srcdir nil) nil))

(defun fw/interested-files (dirs pat)
  "Return a list of interested files."
  (mapcar
    (lambda (dir)
      (mapcar
        (lambda (file) (fw/file-relative-name file (fw/project-root)))
        (directory-files-recursively dir pat)))
    dirs))

(provide 'init-framework)
