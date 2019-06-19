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
          length (match-string 3 line)
          nullable (match-string 4 line)
          unique (match-string 5 line)
          comments (match-string 6 line))
        (and colname
          (setq col (list colname dbtype length nullable unique comments)))))
    col))

(defun jh/query-all-tables-in-oracle ()
  "Get all table information using oracle database."
  (let* ((query
           (concat
             "SELECT USER_TAB_COMMENTS.TABLE_NAME ||"
             "         ',' || USER_TAB_COMMENTS.TABLE_TYPE ||"
             "         ',' || USER_TAB_COMMENTS.COMMENTS"
             "  FROM USER_TAB_COMMENTS"
             " ORDER BY USER_TAB_COMMENTS.TABLE_NAME;"))
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
        (insert (apply 'format "%s %s\n" table)))
      (goto-char (point-min)))))

(defun jh/dump-oracle-table-columns ()
  "Dump table data."
  (interactive)
  (let* ((limit 1000)
          (table (spt/extract-java-entity-table (jh/current-buffer)))
          (tabname (if table table
                     (completing-read "Dump Table >> " (jh/java-table-names))))
          (columns (hash-table-values (spt/cache-of-table-columns tabname)))
          (visiable-dbtypes '("CHAR" "NVARCHAR2" "VARCHAR" "VARCHAR2" "NUMBER" "DATE"))
          (visiable-columns
            (remove-if-not
              (lambda (column)
                (let ((dbtype (cadr column)))
                  (member dbtype visiable-dbtypes)))
              columns))
          (invisiable-columns
            (remove-if
              (lambda (column)
                (let ((dbtype (cadr column)))
                  (member dbtype visiable-dbtypes)))
              columns))
          (seleted-columns
            (mapconcat
              (lambda (column)
                (let ((colname (car column))
                       (dbtype (cadr column))
                       (length (caddr column)))
                  (cond
                    ((member dbtype '("CHAR" "NVARCHAR2" "VARCHAR" "VARCHAR2"))
                      (format "REPLACE(REPLACE(NVL(t.%s,'NULL'),TO_CHAR(CHR(13)),'\\r'),TO_CHAR(CHR(10)),'\\n')" colname))
                    ((string= dbtype "DATE")
                      (format "TO_CHAR(t.%s,'YYYY-MM-DD HH:MM:SS')" colname))
                    (t (format "t.%s" colname)))))
              visiable-columns "  ||'$ep'||\n"))
          (query (concat
                   "SELECT " seleted-columns " AS HEADER\n"
                   " FROM " tabname " t"
                   " WHERE ROWNUM < " (int-to-string limit) ";"))
          (lines (cdr (remove-if
                        (lambda (line)
                          (or (string= "HEADER" line)
                            (string-match-p "^-*$" line)
                            (string-match-p "^[0-9]+ rows selected.$" line)))
                        (split-string (jh/sql-execute query) "\n")))))
    (progn
      (switch-to-buffer (concat tabname ".yml"))
      (or (eq major-mode 'yaml-mode) (yaml-mode))
      (kill-region (point-min) (point-max))

      ;; insert rows data
      (setq i 1)
      (dolist (line lines)
        ;; insert row counter
        (insert
          (concat
            "##############################" (format " Row %d " i)
            "##############################" "\n"))
        (setq j 0)

        ;; insert visibale columns
        (dolist (coldata (split-string line "$ep"))
          (let* ((colname (car (nth j visiable-columns)))
                  (dbtype (cadr (nth j visiable-columns)))
                  (nullable (nth 3 (nth j visiable-columns)))
                  (star (if (string= nullable "N") "*" ""))
                  (colvalue
                    (if (and (string= dbtype "NUMBER") (string= coldata "")) "NULL" coldata)))
            (insert (format "%s%s: %s\n" star colname colvalue)))
          (setq j (+ j 1)))

        ;; insert invisiable columns
        (dolist (column invisiable-columns)
          (let* ((colname (car column))
                  (dbtype (cadr column))
                  (nullable (nth 3 (nth j visiable-columns)))
                  (star (if (string= nullable "N") "*" "")))
            (insert (format "%s%s: <<%s>>\n" star colname dbtype))))

        ;; skip a blank line
        (insert "\n")
        (setq i (+ i 1)))
      ;; go to the beigining
      (goto-char (point-min)))))

;; -----------------------------------------------------------------------------
;; postgres
;; -----------------------------------------------------------------------------


(provide 'init-sql)
