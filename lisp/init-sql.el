(add-hook 'sql-mode-hook
  (lambda()
    (setq
      show-trailing-whitespace t
      indent-tabs-mode nil
      tab-width 2)
    (sqlind-minor-mode 1)
    (hl-line-mode 1)))

(when (jh/windows?)
  (setq sql-mysql-options '("-C" "-f" "-t" "-n")))

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

;; -----------------------------------------------------------------------------
;; oracle
;; -----------------------------------------------------------------------------
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
             "  REPLACE(REPLACE(tc.COMMENTS, TO_CHAR(CHR(13)), ''), TO_CHAR(CHR(10)), '_r_n')"
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
             "         ',' || REPLACE(CMT.COMMENTS, TO_CHAR(CHR(13)) || TO_CHAR(CHR(10)), '')"
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

(defun jh/gen-oracle-select-query (tabname columns)
  "Generate Oracle query string."
  (let* ((selected
           (mapconcat
             (lambda (column)
               (let ((colname (nth 0 column))
                      (dbtype (nth 1 column))
                      (dblen (nth 2 column)))
                 (cond
                   ((member dbtype jh/oracle-string-datatype)
                     (format "REPLACE(REPLACE(NVL(t.%s,'#il'), TO_CHAR(CHR(13)),''), TO_CHAR(CHR(10)), '#ew')" colname))
                   ((string= dbtype "DATE")
                     (format "TO_CHAR(t.%s, 'YYYY-MM-DD HH:MM:SS')" colname))
                   (t (format "t.%s" colname)))))
             columns " ||'$ep'||\n"))
          (query (concat
                   "SELECT 'li#e'|| " selected
                   "\nFROM " tabname " t"
                   "\nWHERE ROWNUM < " (int-to-string limit) ";")))
    query))

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
                       (concat "\""
                         (replace-regexp-in-string "\"" "\\\"" coldata)
                         "\"")))))
              (t (if (string= coldata "#il") "null" coldata)))))
    (format "  %s%s: %s\n" star colname colvalue)))

(defun jh/dump-oracle-table-columns ()
  "Dump table data."
  (interactive)
  (let* ((limit 1000)
          (table (and (buffer-file-name) (spt/extract-java-entity-table (jh/current-buffer))))
          (tabname (if table table (completing-read "Dump Table >> " (jh/java-table-names))))
          (columns (hash-table-values (spt/cache-of-table-columns tabname)))
          (visible-dbtypes '("CHAR" "NVARCHAR2" "VARCHAR" "VARCHAR2" "NUMBER" "DATE"))
          (visible-columns (remove-if-not (lambda (column) (member (cadr column) visible-dbtypes)) columns))
          (invisible-columns (remove-if (lambda (column) (member (cadr column) visible-dbtypes)) columns))
          (query (jh/gen-oracle-select-query tabname visible-columns))
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
        (insert (format "- ### Row %d ###\n" i))
        (setq j 0)
        ;; insert visibale columns
        (dolist (coldata (split-string line "$ep"))
          (insert (jh/stringify-oracle-column-value (nth j visible-columns) coldata))
          (setq j (+ j 1)))
        ;; insert invisible columns
        (dolist (column invisible-columns)
          (let* ((colname (nth 0 column))
                  (dbtype (nth 1 column))
                  (nullable (nth 3 (nth j visible-columns)))
                  (star (if (string= nullable "N") "*" "")))
            (insert (format "  %s%s: ##<%s>##\n" star colname dbtype))))
        ;; skip a blank line
        (insert "\n")
        (setq i (+ i 1)))
      ;; go to the beigining
      (goto-char (point-min)))))

;; -----------------------------------------------------------------------------
;; postgres
;; -----------------------------------------------------------------------------


(provide 'init-sql)
