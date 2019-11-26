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
;; SQL Helper
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ___  ____      _    ____ _     _____
;;  / _ \|  _ \    / \  / ___| |   | ____|
;; | | | | |_) |  / _ \| |   | |   |  _|
;; | |_| |  _ <  / ___ \ |___| |___| |___
;;  \___/|_| \_\/_/   \_\____|_____|_____|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -----------------------------------------------------------------------------
;; Variables
;; -----------------------------------------------------------------------------

;; datatypes
(defvar jh/oracle-string-datatype '("CHAR" "NVARCHAR2" "VARCHAR" "VARCHAR2")
  "Oracle string datatype list")
(defvar jh/oracle-lob-datatype '("CLOB" "BLOB")
  "Oracle string datatype list")
;; separators
(defvar jh/oracle-lsep "#ew" "Oracle newline separator")
(defvar jh/oracle-nsep "#il" "Oracle null separator")
(defvar jh/oracle-fsep "$ep" "Oracle field separator")
(defvar jh/oracle-lpre "li#e" "Oracle line prefix")

;; -----------------------------------------------------------------------------
;; Query Generator
;; -----------------------------------------------------------------------------

(defun jh/oracle-gen-list-table-query (&optional separator)
  "Generate list table query."
  (let ((sep (or separator ",")))
    (jh/concat-lines
      "SELECT"
      (format "  utbs.TABLE_NAME || '%s' ||" sep)
      "  ("
      "    SELECT REPLACE(REPLACE(utbc.COMMENTS, CHR(13), ''), CHR(10), '\\n')"
      "      FROM USER_TAB_COMMENTS utbc"
      "     WHERE utbc.TABLE_NAME = utbs.TABLE_NAME AND ROWNUM <= 1"
      "  ) AS CONTENT"
      "  FROM USER_TABLES utbs"
      " ORDER BY utbs.TABLE_NAME;")))

(defun jh/oracle-gen-list-table-column-query (tabname &optional separator)
  "Generate list table columns query."
  (let ((sep (or separator ",")))
    (concat
      "SELECT"
      (format "  utbc.COLUMN_NAME || '%s' ||" sep)
      (format "  utbc.DATA_TYPE || '%s' ||" sep)
      (format "  utbc.DATA_LENGTH || '%s' ||" sep)
      (format "  DECODE(utbc.NULLABLE, 'N', 'N', '') || '%s' ||" sep)
      "  (SELECT 'U'"
      "     FROM USER_CONS_COLUMNS uccl, USER_CONSTRAINTS ucst"
      "    WHERE ucst.CONSTRAINT_NAME = uccl.CONSTRAINT_NAME AND"
      "          utbc.COLUMN_NAME = uccl.COLUMN_NAME AND"
      "          utbc.TABLE_NAME = uccl.TABLE_NAME AND"
      "          ucst.CONSTRAINT_TYPE = 'U'"
      "    GROUP BY ucst.CONSTRAINT_NAME"
      (format "   HAVING COUNT(1) = 1) || '%s' ||" sep)
      "  (SELECT 'P'"
      "     FROM USER_CONS_COLUMNS uccl, USER_CONSTRAINTS ucst"
      "    WHERE ucst.CONSTRAINT_NAME = uccl.CONSTRAINT_NAME AND"
      "          utbc.COLUMN_NAME = uccl.COLUMN_NAME AND"
      "          utbc.TABLE_NAME = uccl.TABLE_NAME AND"
      "          ucst.CONSTRAINT_TYPE = 'P' AND"
      (format "          ROWNUM <= 1) || '%s' ||" sep)
      (format "  utbc.DATA_PRECISION || '%s' ||" sep)
      "  ("
      "    SELECT REPLACE(REPLACE(uclc.COMMENTS, CHR(13), ''), CHR(10), '\n')"
      "      FROM USER_COL_COMMENTS uclc"
      "     WHERE uclc.COLUMN_NAME = utbc.COLUMN_NAME AND"
      "           uclc.TABLE_NAME = utbc.TABLE_NAME AND"
      "           ROWNUM <= 1"
      "  ) AS CONTENT"
      "  FROM USER_TAB_COLUMNS utbc"
      (format " WHERE UPPER(utbc.TABLE_NAME) = '%s';" tabname))))

(defun jh/oracle-gen-select-query (tabname colinfos &optional limit)
  "Generate SELECT query."
  (let ((limit (or limit 100)) (fsep ",\n"))
    (jh/concat-lines
      "SELECT"
      (mapconcat #'(lambda (colinfo) (format "  %s" (car colinfo))) colinfos fsep)
      "FROM" (format "  %s" tabname)
      "WHERE" (format "  ROWNUM < %d;" limit))))

;; -----------------------------------------------------------------------------
;; Normalization Helper
;; -----------------------------------------------------------------------------

(defun jh/oracle-uniform-column (colinfo)
  "uniform oracle column select string."
  (let ((colname (nth 0 colinfo))
         (dbtype (nth 1 colinfo)))
    (cond
      ((member dbtype jh/oracle-string-datatype)
        (format
          "REPLACE(REPLACE(NVL(t.%s,'%s'),CHR(13),''),CHR(10),'%s')"
          colname jh/oracle-lsep jh/oracle-lsep))
      ((member dbtype jh/oracle-lob-datatype)
        (format
          "NVL(TO_CHAR(LENGTH(t.%s)),'%s')"
          colname jh/oracle-nsep))
      ((string= dbtype "DATE")
        (format
          "TO_CHAR(t.%s, 'yyyy-mm-dd hh24:mi:ss')"
          colname))
      (t (format "t.%s" colname)))))

(defun jh/oracle-gen-uniform-select-query (tabname colinfos &optional limit)
  "generate SELECT query with uniformed column select string."
  (let ((limit (or limit 100)))
    (jh/concat-lines
      (format "SELECT '%s'||" jh/oracle-lpre)
      (mapconcat
        #'(lambda (colinfo)
            (format "  %s" (jh/oracle-uniform-column colinfo)))
        colinfos (format "||'%s'||\n" jh/oracle-fsep))
      "AS CONTENT"
      "FROM" (format "  %s t" tabname)
      "WHERE" (format "  ROWNUM < %d;" limit))))

;; -----------------------------------------------------------------------------
;; Regexp and Line Parser
;; -----------------------------------------------------------------------------

(defun jh/oracle-parse-table-info (line)
  "Convert oracle line string to (tabname, tabcmt), otherwise return nil."
  (let ((regexp
          (concat
            "[ \t]*\\([_A-Za-z0-9]+\\),"
            "[ \t]*\\(.*\\)$"))
         (tabinfo))
    (and (save-match-data (string-match regexp line)
         (setq
           tabname (match-string 1 line)
           tabcmt (match-string 2 line))
         (and tabname
           (setq tabinfo (list tabname tabcmt)))))
    tabinfo))

(defun jh/oracle-parse-table-columns-info (line)
  "Convert oracle line string to (colname dbtype dblen nullable unique colcmt)."
  (let ((regexp
          (concat
            "[ \t]*\\([_A-Za-z0-9]+\\),"
            "[ \t]*\\([_A-Za-z0-9]+\\),"
            "[ \t]*\\([_A-Za-z0-9]*\\),"
            "[ \t]*\\([_A-Za-z0-9]*\\),"
            "[ \t]*\\([_A-Za-z0-9]*\\),"
            "[ \t]*\\([_A-Za-z0-9]*\\),"
            "[ \t]*\\([_A-Za-z0-9]*\\),"
            "[ \t]*\\(.*\\)$"))
         (colinfo))
    (and
      (save-match-data (string-match regexp line)
        (setq
          colname (match-string 1 line)
          dbtype (match-string 2 line)
          dblen (match-string 3 line)
          nullable (match-string 4 line)
          unique (match-string 5 line)
          pk (match-string 6 line)
          precision (match-string 7 line)
          colcmt (match-string 8 line))
        (and colname
          (setq colinfo
            (list colname dbtype dblen nullable unique pk precision colcmt)))))
    colinfo))


;; -----------------------------------------------------------------------------
;; SQL-level Helper
;; -----------------------------------------------------------------------------

(defun jh/oracle-list-tables ()
  "List all tables in database."
  (let* ((query (jh/oracle-gen-list-table-query))
          (lines (split-string (jh/sql-execute query) "\n")))
    (remove-if 'null (mapcar #'jh/oracle-parse-table-info lines))))

(defun jh/oracle-list-table-columns (tabname)
  "List all columns in a table with given TABNAME."
  (let* ((query (jh/oracle-gen-list-table-column-query tabname))
          (lines (split-string (jh/sql-execute query) "\n")))
    (remove-if 'null (mapcar #'jh/oracle-parse-table-columns-info lines))))


;; -----------------------------------------------------------------------------
;; Result Set Helper
;; -----------------------------------------------------------------------------

(defun jh/oracle-stringify-result-data (cell dbtype)
  "Make coldate to a string according to dbtype."
  (cond
    ((and (string= dbtype "NUMBER") (string= cell "")) "null")
    ((member dbtype jh/oracle-string-datatype)
      (cond
        ;; multi-line string
        ((string-match-p jh/oracle-lsep cell)
          (jh/concat-lines "|"
            (mapconcat #'(lambda (line) (concat "    " line))
              (split-string cell jh/oracle-lsep) "\n")))
        ;; long line string
        ((> (length cell) 120) (concat ">\n    " cell))
        ;; null string
        ((string= cell jh/oracle-nsep) "null")
        ;; default string
        (t (concat "\"" (jh/sql-escape-string cell) "\""))))
    ((member dbtype jh/oracle-lob-datatype)
      (cond
        ;; null lob
        ((string= cell jh/oracle-nsep)
          (format "### %s(null) ###" dbtype))
        ;; default lob, just display size
        (t (format "### %s(%s) ###" dbtype
             (file-size-human-readable (string-to-number cell))))))
    ;; default return value
    (t (if (string= cell jh/oracle-nsep) "null" cell))))

(defun jh/oracle-stringify-result-cell (cell colinfo)
  "Convert result cell to a readable string."
  (let ((colname (nth 0 colinfo))
         (star (if (string= (nth 3 colinfo) "N") "*" ""))
         (colvalue (jh/oracle-stringify-result-data cell (nth 1 colinfo))))
    (format "  %s%s: %s" star colname colvalue)))

(defun jh/oracle-stringify-result-row (index row colinfos)
  "Convert nth row line string to YAML file block."
  (let ((res (format "- ### Row %d ###" index)))
    (setq j 0)
    (dolist (cell row)
      (setq res
        (jh/concat-lines res
          (jh/oracle-stringify-result-cell cell (nth j colinfos))))
      (setq j (+ j 1)))
    res))

(defun jh/oracle-stringify-result-set (rows colinfos)
  "Convert result set to YAML file content."
  (let ((res))
    (setq i 1)
    (dolist (row rows)
      (setq res
        (jh/concat-lines res ""
          (jh/oracle-stringify-result-row i row colinfos)))
      (setq i (+ i 1)))
    res))

(defun jh/oracle-fetch-result-set (tabname)
  "Fetch oracle result set."
  (let ((colinfos (jh/oracle-list-table-columns tabname)))
    (mapcar
      #'(lambda (line)
          (split-string
            (replace-regexp-in-string
              (concat "^" jh/oracle-lpre) "" line)
            jh/oracle-fsep))
      (remove-if-not
        #'(lambda (line)
            (string-match-p (concat "^" jh/oracle-lpre) line))
        (split-string
          (jh/sql-execute
            (jh/oracle-gen-uniform-select-query tabname colinfos)) "\n")))))

;; -----------------------------------------------------------------------------
;; Interactive Commands
;; -----------------------------------------------------------------------------

(defun jh/oracle-dump-tables ()
  "Dump talbe name and comments."
  (interactive)
  (let ((tables (jh/oracle-list-tables)))
    (progn
      (switch-to-buffer "tables.txt")
      (dolist (table tables)
        (let ((colname (nth 0 table))
               (comments (jh/strip (nth 1 table))))
          (insert (format "%s %s\n" colname comments))))
      (goto-char (point-min)))))

(defun jh/oracle-dump-rows ()
  "Dump table row data."
  (interactive)
  (let ((tabname (jh/guess-table-name)))
    (progn
      (switch-to-buffer (concat tabname ".yml"))
      (or (eq major-mode 'yaml-mode) (yaml-mode))
      (kill-region (point-min) (point-max))
      ;; insert title
      (insert (format "### Dump rows of %s ###" tabname))
      (insert (jh/oracle-stringify-result-set
                (jh/oracle-fetch-result-set tabname)
                (jh/oracle-list-table-columns tabname)))
      ;; go to the beigining
      (goto-char (point-min)))))

(defun jh/oracle-copy-select-query ()
  "Copy select query to clipboard."
  (interactive)
  (let*
    ((tabnames (mapcar #'car (jh/oracle-list-tables)))
      (tabname (completing-read "Dump Table >> " tabnames)))
    (jh/sent-to-clipboard
      (jh/oracle-gen-select-query tabname
        (jh/oracle-list-table-columns tabname)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (let ((limit (or limit 100))
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
                  ((string-match-p jh/oracle-lsep coldata)
                    (concat "|\n    " (replace-regexp-in-string jh/oracle-lsep "\n    " coldata)))
                  ((> (length coldata) 80)
                    (concat ">\n    " coldata))
                  (t (if (string= coldata jh/oracle-nsep) "null"
                       (concat "\"" (jh/sql-escape-string coldata) "\"")))))
              ((member dbtype jh/oracle-lob-datatype)
                (if (string= coldata jh/oracle-nsep) (format "### %s(null) ###" dbtype)
                  (format "### %s(%s) ###" dbtype
                    (file-size-human-readable (string-to-number coldata)))))
              (t (if (string= coldata jh/oracle-nsep) "null" coldata)))))
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
;;  ____   ___  ____ _____ ____ ____  _____ ____
;; |  _ \ / _ \/ ___|_   _/ ___|  _ \| ____/ ___|
;; | |_) | | | \___ \ | || |  _| |_) |  _| \___ \
;; |  __/| |_| |___) || || |_| |  _ <| |___ ___) |
;; |_|    \___/|____/ |_| \____|_| \_\_____|____/
;; -----------------------------------------------------------------------------


(provide 'init-sql)
