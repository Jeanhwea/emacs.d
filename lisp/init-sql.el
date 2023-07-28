(add-hook 'sql-mode-hook
  #'(lambda()
      (setq
        show-trailing-whitespace t
        indent-tabs-mode nil
        tab-width 4)
      (setq abbrev-mode nil)
      (sqlind-minor-mode 1)
      (highlight-current-line)))

(when (jh/windows?)
  (setq sql-mysql-program "mysql")
  (setq sql-mysql-options '("-C" "-f" "-t" "-n" "--default-character-set=utf8mb4")))

(when (jh/mac?)
  (setq sql-mysql-program "mysql")
  (setq sql-mysql-options '("-C" "-f" "-t" "-n")))

;; oracle sqlplus environments
(setenv "NLS_LANG" "AMERICAN_AMERICA.AL32UTF8")
(setenv "NLS_DATE_FORMAT" "YYYY-MM-DD HH24:MI:SS")

(defconst pgformat-name
  (if (jh/windows?)  "C:/Local/pgFormatter-5.0/pg_format" (executable-find "pg_format"))
  "Location of pg_format program")

(defconst pgformat-prog (if (jh/windows?) "perl pg_format" "LC_ALL=C pg_format")
  "pg_format command.")

(defconst pgformat-func-dict
  (expand-file-name "lang/sql-func-name.txt" user-emacs-directory)
  "Location of pg_format extra function names.")

;; http://sqlformat.darold.net/
;; https://github.com/darold/pgFormatter
(defconst pgformat-command-lower
  (format
    "%s -p '\\$\\{[_0-9a-zA-Z:]+\\}' -f 1 -u 1 -U 1 -s 2 -w 120 --extra-function \"%s\" -"
    pgformat-prog pgformat-func-dict)
  "pg_format command.")

(defconst pgformat-command-upper
  (format
    "%s -p '\\$\\{[_0-9a-zA-Z:]+\\}' -f 2 -u 2 -U 2 -s 2 -w 120 --extra-function \"%s\" -"
    pgformat-prog pgformat-func-dict)
  "pg_format command.")

;; pip install sqlparse
(defconst sqlformat-command-lower
  "sqlformat - -k lower -i lower -s -a --indent_width 2 --wrap_after 120"
  "sqlformat command.")

(defconst sqlformat-command-upper
  "sqlformat - -k upper -i upper -s -a --indent_width 2 --wrap_after 120"
  "sqlformat command.")

;; (defconst sql-format-command
;;   (format "%s | %s" sqlformat-command-lower pgformat-command-lower)
;;   "format sql command.")

(defconst sql-format-command
  (format "%s" pgformat-command-lower)
  "format sql command.")

(defun jh/format-sql-source (&optional file)
  "Format sql source code."
  (let
    ((file (or file (buffer-file-name)))
      (current-point (point)) (pt1) (pt2))
    ;; setup begin and end
    (if (use-region-p)
      (and (setq pt1 (region-beginning)) (setq pt2 (region-end)))
      ;; get a paragraph
      (progn
        (backward-paragraph)
        (when (> (point) (point-min)) (forward-char))
        (setq pt1 (point))
        (forward-paragraph)
        (backward-char)
        (end-of-line)
        (setq pt2 (point))))
    ;; execute commands
    (progn
      (shell-command-on-region pt1 pt2 pgformat-command-lower nil t)
      (goto-char (min current-point (point-max))))))

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
