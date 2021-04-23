(add-hook 'sql-mode-hook
  #'(lambda()
      (setq
        show-trailing-whitespace t
        indent-tabs-mode nil
        tab-width 2)
      (setq abbrev-mode nil)
      (sqlind-minor-mode 1)
      (hl-line-mode 1)))

(when (jh/windows?)
  (setq sql-mysql-program "mysql")
  (setq sql-mysql-options '("-C" "-f" "-t" "-n" "--default-character-set=utf8mb4")))

;; oracle sqlplus environments
(setenv "NLS_LANG" "AMERICAN_AMERICA.AL32UTF8")
(setenv "NLS_DATE_FORMAT" "YYYY-MM-DD HH24:MI:SS")

(defconst pgformat-name
  (if (jh/windows?)  "C:/Local/pgFormatter-5.0/pg_format" (executable-find "pg_format"))
  "Location of pg_format program")

(defconst pgformat-prog (if (jh/windows?) "perl pg_format" "LC_ALL=C pg_format")
  "pg_format command.")

(defconst pgformat-func-dict
  (expand-file-name "lang/oracle-func-name.txt" user-emacs-directory)
  "Location of pg_format extra function names.")

;; http://sqlformat.darold.net/
;; https://github.com/darold/pgFormatter
(defconst pgformat-command
  (format
    "%s -p '\\?[0-9]+' -f 2 -u 2 -U 2 -s 2 -w 80 --extra-function \"%s\" -"
    pgformat-prog pgformat-func-dict)
  "pg_format command.")

;; pip install sqlparse
(defconst sqlformat-command
  "sqlformat - -k upper -i upper -s -a --indent_width 2 --wrap_after 80"
  "sqlformat command.")

(defconst sql-format-command
  (format "%s | %s" sqlformat-command pgformat-command)
  "format sql command.")

(defun jh/format-sql-source (&optional file)
  "Format sql source code."
  (let
    ((file (or file (buffer-file-name)))
      (current-point (point)) (beg) (end))
    ;; get a paragraph
    (progn
      (backward-paragraph)
      (when (> (point) (point-min)) (forward-char))
      (setq beg (point))
      (forward-paragraph)
      ;; (when (< (point) (point-max)) (backward-char))
      (setq end (point)))
    ;; execute commands
    (if (file-exists-p pgformat-name)
      (let ((default-directory (file-name-directory pgformat-name)))
        (shell-command-on-region beg end sql-format-command nil t))
      (shell-command-on-region beg end sqlformat-command nil t))
    ;; goto previous place
    (when (<= current-point (point-max)) (goto-char current-point))))

(defun jh/format-sql-file (&optional file)
  "Format sql file code."
  (let
    ((file (or file (buffer-file-name)))
      (current-point (point)) (beg) (end))
    ;; get a paragraph
    (progn
      (beginning-of-buffer)
      (next-line)
      (when (> (point) (point-min)) (forward-char))
      (setq beg (point))
      (end-of-buffer)
      ;; (when (< (point) (point-max)) (backward-char))
      (setq end (point)))
    ;; execute commands
    (if (file-exists-p pgformat-name)
      (let ((default-directory (file-name-directory pgformat-name)))
        (shell-command-on-region beg end (concat sql-format-command " -N") nil t))
      (shell-command-on-region beg end sqlformat-command nil t))
    ;; goto previous place
    (when (<= current-point (point-max)) (goto-char current-point))))

;; https://www.emacswiki.org/emacs/SqlMode
(defun jh/sql-handle-prompt (output)
  "Handle prompt on windows."
  (cond
    ((eq sql-product 'mysql)
      (if (jh/windows?) (concat "\n" output "\nmysql> ") (concat "\n" output)))
    (t output)))

(add-hook 'sql-interactive-mode-hook
  #'(lambda ()
      (add-hook 'comint-preoutput-filter-functions 'jh/sql-handle-prompt)))

(provide 'init-sql)
