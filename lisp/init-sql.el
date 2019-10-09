(add-hook 'sql-mode-hook
  (lambda()
    (setq
      show-trailing-whitespace t
      indent-tabs-mode nil
      tab-width 2)
    (sqlind-minor-mode 1)
    (hl-line-mode 1)))

(when (jh/windows?)
  (setq sql-mysql-program "mysql")
  (setq sql-mysql-options '("-C" "-f" "-t" "-n" "--default-character-set=utf8mb4")))

;; https://www.emacswiki.org/emacs/SqlMode
(defun jh/sql-handle-prompt (output)
  "handle prompt on windows."
  (cond
    ((eq sql-product 'mysql)
      (if (jh/windows?) (concat "\n" output "\nmysql> ") (concat "\n" output)))
    (t output)))

(defun jh/sql-interactive-hook ()
  "Add hooks to `sql-interactive-mode-hook'."
  (add-hook 'comint-preoutput-filter-functions 'jh/sql-handle-prompt))

(add-hook 'sql-interactive-mode-hook #'jh/sql-interactive-hook)

;; -----------------------------------------------------------------------------
;; sql helper
;; -----------------------------------------------------------------------------
(defun jh/sql-execute (query)
  "Execute a query."
  (let ((sqlbuf (sql-find-sqli-buffer))
         (outbuf "*SQL RESULT SET*")
         (result))
    (unless sqlbuf
      (user-error "No SQL interactive buffer found"))
    (progn
      (switch-to-buffer outbuf)
      (sql-execute sqlbuf outbuf query nil nil)
      (setq result (buffer-string))
      (kill-buffer outbuf))
    result))

(defun jh/sql-escape-string (str)
  "escape string in sql text."
  (let* ((str (replace-regexp-in-string "\\\\" "\\\\" str nil t))
          (str (replace-regexp-in-string "\"" "\\\"" str nil t)))
    str))

;; -----------------------------------------------------------------------------
;; oracle
;; -----------------------------------------------------------------------------
(defun jh/oracle-gen-list-table-query (&optional separator)
  "generate list table query."
  (let ((sep (if separator separator ",")))
    (jh/concat-lines "SELECT"
      (format "  utbs.TABLE_NAME || '%s' ||" sep)
      "  ("
      "    SELECT REPLACE(REPLACE(utbc.COMMENTS, CHR(13), ''), CHR(10), '\\n')"
      "      FROM USER_TAB_COMMENTS utbc"
      "     WHERE utbc.TABLE_NAME = utbs.TABLE_NAME AND ROWNUM <= 1"
      "  )"
      "  FROM USER_TABLES utbs"
      " ORDER BY utbs.TABLE_NAME;")))

(defun jh/extract-table-in-oracle (line)
  "Extract table name, when using oracle database."
  (let ((regexp
          (concat
            "[ \t]*\\([_A-Za-z0-9]+\\),"
            "\\(TABLE\\|VIEW\\),"
            "[ \t]*\\(.*\\)$"))
         (table))
    (and (save-match-data (string-match regexp line)
         (setq
           tabname (match-string 1 line)
           comments (match-string 3 line))
         (and tabname
           (setq table (list tabname comments)))))
    table))

(defun jh/extract-table-column-in-oracle (line)
  "Extract table column information, when using oracle database."
  (let ((regexp
          (concat
            "[ \t]*\\([_A-Za-z0-9]+\\),"
            "[ \t]*\\([_A-Za-z0-9]+\\),"
            "[ \t]*\\([_A-Za-z0-9]*\\),"
            "[ \t]*\\([_A-Za-z0-9]*\\),"
            "[ \t]*\\([_A-Za-z0-9]*\\),"
            "[ \t]*\\(.*\\)$"))
         (col))
    (and
      (save-match-data (string-match regexp line)
        (setq
          colname (match-string 1 line)
          dbtype (match-string 2 line)
          dblen (match-string 3 line)
          nullable (match-string 4 line)
          unique (match-string 5 line)
          comments (match-string 6 line))
        (and colname
          (setq col (list colname dbtype dblen nullable unique comments)))))
    col))

(defun jh/query-all-tables-in-oracle ()
  "Get all table information using oracle database."
  (let* ((query
           (concat
             "SELECT tc.TABLE_NAME ||','|| tc.TABLE_TYPE ||','|| "
             "  REPLACE(REPLACE(tc.COMMENTS, CHR(13), ''), CHR(10), '_r_n')"
             "  FROM USER_TAB_COMMENTS tc"
             " WHERE REGEXP_LIKE(tc.TABLE_NAME, '^[0-9A-Za-z][_0-9A-Za-z]*$')"
             " ORDER BY tc.TABLE_NAME;"))
          (lines (split-string (jh/sql-execute query) "\n")))
    (remove-if 'null (mapcar #'jh/extract-table-in-oracle lines))))

(defun jh/query-table-columns-in-oracle (tabname)
  "Query columns of a table using oracle database."
  (let* ((query
           (concat
             "SELECT TAB.COLUMN_NAME ||"
             "         ',' || TAB.DATA_TYPE ||"
             "         ',' || TAB.DATA_LENGTH ||"
             "         ',' || TAB.NULLABLE ||"
             "         ',' || CONS.CONSTRAINT_TYPE ||"
             "         ',' || REPLACE(CMT.COMMENTS, CHR(13) || CHR(10), '')"
             "  FROM USER_TAB_COLUMNS TAB"
             "         LEFT JOIN"
             "         USER_CONS_COLUMNS CONS_NAME"
             "             ON TAB.TABLE_NAME = CONS_NAME.TABLE_NAME AND TAB.COLUMN_NAME = CONS_NAME.COLUMN_NAME"
             "         LEFT JOIN"
             "         USER_CONSTRAINTS CONS"
             "             ON CONS_NAME.CONSTRAINT_NAME = CONS.CONSTRAINT_NAME"
             "         LEFT JOIN"
             "         USER_COL_COMMENTS CMT"
             "             ON TAB.TABLE_NAME = CMT.TABLE_NAME AND TAB.COLUMN_NAME = CMT.COLUMN_NAME"
             " WHERE TAB.TABLE_NAME = '" tabname "'"
             "    ORDER BY TAB.COLUMN_ID, CONS.CONSTRAINT_TYPE;"))
          (lines (split-string (jh/sql-execute query) "\n")))
    (remove-if 'null (mapcar #'jh/extract-table-column-in-oracle lines))))

(defun jh/dump-oracle-tables ()
  "Dump talbe name and comments."
  (interactive)
  (let ((tables (jh/query-all-tables-in-oracle)))
    (progn
      (switch-to-buffer "tables.txt")
      (dolist (table tables)
        (let ((colname (nth 0 table))
               (comments (jh/strip (nth 1 table))))
          (insert (format "%s %s\n" colname comments))))
      (goto-char (point-min)))))

(defvar jh/oracle-string-datatype '("CHAR" "NVARCHAR2" "VARCHAR" "VARCHAR2")
  "Oracle string datatype list")

(defvar jh/oracle-lob-datatype '("CLOB" "BLOB")
  "Oracle string datatype list")

(defun jh/format-oracle-select-column(column)
  "Format oracle column placeholder at the select place."
  (let ((colname (nth 0 column))
         (dbtype (nth 1 column))
         (dblen (nth 2 column)))
    (cond
      ((member dbtype jh/oracle-string-datatype)
        (format "REPLACE(REPLACE(NVL(t.%s,'#il'), CHR(13),''), CHR(10), '#ew')" colname))
      ((member dbtype jh/oracle-lob-datatype)
        (format "NVL(TO_CHAR(LENGTH(t.%s)),'#il')" colname))
      ((string= dbtype "DATE")
        (format "TO_CHAR(t.%s, 'yyyy-mm-dd hh24:mi:ss')" colname))
      (t (format "t.%s" colname)))))

(defun jh/gen-oracle-select-query (tabname columns &optional limit)
  "Generate Oracle query string."
  (let ((limit (or limit 1000))
         (sltcol (mapconcat #'jh/format-oracle-select-column columns " ||'$ep'||\n")))
    (concat
      "SELECT 'li#e'|| " sltcol
      "\nFROM " tabname " t"
      "\nWHERE ROWNUM < " (int-to-string limit) ";")))

(defun jh/stringify-oracle-column-value (column coldata)
  "Make column data to a readable string."
  (let* ((colname (nth 0 column))
          (dbtype (nth 1 column))
          (nullable (nth 3 column))
          (star (if (string= nullable "N") "*" ""))
          (colvalue
            (cond
              ((and (string= dbtype "NUMBER") (string= coldata "")) "null")
              ((and (member dbtype jh/oracle-string-datatype) )
                (cond
                  ((string-match-p "#ew" coldata)
                    (concat "|\n    " (replace-regexp-in-string "#ew" "\n    " coldata)))
                  ((> (length coldata) 80)
                    (concat ">\n    " coldata))
                  (t (if (string= coldata "#il") "null"
                       (concat "\"" (jh/sql-escape-string coldata) "\"")))))
              ((member dbtype jh/oracle-lob-datatype)
                (if (string= coldata "#il") (format "### %s(null) ###" dbtype)
                  (format "### %s(%s) ###" dbtype
                    (file-size-human-readable (string-to-number coldata)))))
              (t (if (string= coldata "#il") "null" coldata)))))
    (format "  %s%s: %s\n" star colname colvalue)))

(defun jh/guess-table-name ()
  "Guess table name."
  (let ((tabnames (jh/java-table-names))
         (anno (and (buffer-file-name)
                 (spt/extract-java-entity-table (jh/current-buffer))))
         (name (file-name-sans-extension (buffer-name)))
         (symb (symbol-name (symbol-at-point))))
    (cond
      ((member anno tabnames) anno)
      ((member name tabnames) name)
      ((member symb tabnames) symb)
      (t (completing-read "Dump Table >> " tabnames)))))

(defun jh/dump-oracle-table-columns ()
  "Dump table data."
  (interactive)
  (let* ((tabname (jh/guess-table-name))
          (columns (hash-table-values (spt/cache-of-table-columns tabname)))
          (query (jh/gen-oracle-select-query tabname columns))
          (rawlist (split-string (jh/sql-execute query) "\n"))
          (lines (mapcar (lambda (line) (replace-regexp-in-string "^li#e" "" line))
                   (remove-if-not (lambda (line) (string-match-p "^li#e" line)) rawlist))))
    (progn
      (switch-to-buffer (concat tabname ".yml"))
      (or (eq major-mode 'yaml-mode) (yaml-mode))
      (kill-region (point-min) (point-max))
      ;; insert rows data
      (setq i 1)
      (dolist (line lines)
        ;; insert row counter
        (insert (format "- ### Row %d of %s ###\n" i tabname))
        (setq j 0)
        ;; insert columns
        (dolist (coldata (split-string line "$ep"))
          (insert (jh/stringify-oracle-column-value (nth j columns) coldata))
          (setq j (+ j 1)))
        (insert "\n")
        (setq i (+ i 1)))
      ;; go to the beigining
      (goto-char (point-min)))))

;; -----------------------------------------------------------------------------
;; postgres
;; -----------------------------------------------------------------------------


(provide 'init-sql)
