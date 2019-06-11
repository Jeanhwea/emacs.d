(add-hook 'sql-mode-hook
  (lambda()
    (setq
      show-trailing-whitespace t
      indent-tabs-mode nil
      tab-width 2)
    (sqlind-minor-mode 1)
    (hl-line-mode 1)))

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
            tabtype (match-string 2 line)
            comments (match-string 3 line))
          (and tabname
            (setq table (list tabname tabtype comments)))))
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

(defun jh/dump-oracle-table-colums ()
  "Dump table data."
  (interactive)
  (let* ((limit 100)
          (table (spt/extract-java-entity-table (jh/current-buffer)))
          (tabname (if table table
                     (completing-read "Dump Table >> " (jh/java-table-names))))
          (columns (spt/query-table-columns tabname))
          (visiable-dbtypes '("CHAR" "NVARCHAR2" "VARCHAR" "VARCHAR2" "NUMBER" "CLOB" "DATE"))
          (display-columns
            (delete-duplicates
              (remove-if 'null
                (mapcar
                  (lambda (col)
                    (let ((colname (car col)) (dbtype (cadr col)))
                      (cond
                        ((member dbtype visiable-dbtypes) colname)
                        (t nil))))
                  columns)) :test 'equal))
          (seleted-columns
            (mapconcat
              (lambda (colname) (format "t.%s" colname))
              display-columns " || 's-e-p-a-r-a-t-o-r' || "))
          (query (concat
                   "SELECT " seleted-columns " AS HEADER"
                   " FROM " tabname " t"
                   " WHERE ROWNUM < " (int-to-string limit) ";"))
          (lines (remove-if
                   (lambda (line)
                     (or (string= "HEADER" line)
                       (string-match-p "^-*$" line)
                       (string-match-p "^[0-9]+ rows selected.$" line)))
                   (split-string (jh/sql-execute query) "\n"))))
    (progn
      (switch-to-buffer (concat tabname ".txt"))
      (kill-region (point-min) (point-max))
      (setq i 1)
      (dolist
        (line lines)
        (insert (concat
                  "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
                  " Row " (int-to-string i) " "
                  ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                  "\n"))
        (setq j 0)
        (dolist (coldata (split-string line "s-e-p-a-r-a-t-o-r"))
          (insert (concat (nth j display-columns) ": " coldata "\n"))
          (setq j (+ j 1)))
        (dolist (column columns)
          (if (not (member (cadr column) visiable-dbtypes))
            (insert (concat (car column) ": <<" (cadr column) ">>\n"))))
        (setq i (+ i 1))
        (insert "\n"))
      (goto-char (point-min)))))

;; -----------------------------------------------------------------------------
;; postgres
;; -----------------------------------------------------------------------------


(provide 'init-sql)
