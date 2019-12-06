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
  "Handle prompt on windows."
  (cond
    ((eq sql-product 'mysql)
      (if (jh/windows?) (concat "\n" output "\nmysql> ") (concat "\n" output)))
    (t output)))

(defun jh/sql-interactive-hook ()
  "Add hooks to `sql-interactive-mode-hook'."
  (add-hook 'comint-preoutput-filter-functions 'jh/sql-handle-prompt))

(add-hook 'sql-interactive-mode-hook #'jh/sql-interactive-hook)

;; -----------------------------------------------------------------------------
;;
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
  "Escape string in sql text."
  (let* ((str (jh/re-replace "\\\\" "\\\\" str nil t))
          (str (jh/re-replace "\"" "\\\"" str nil t)))
    str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ___  ____      _    ____ _     _____
;;  / _ \|  _ \    / \  / ___| |   | ____|
;; | | | | |_) |  / _ \| |   | |   |  _|
;; | |_| |  _ <  / ___ \ |___| |___| |___
;;  \___/|_| \_\/_/   \_\____|_____|_____|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -----------------------------------------------------------------------------
;;
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
(defvar jh/oracle-lpre ":) " "Oracle line prefix")

;; parameter
(defvar jh/oracle-row-limit 200 "Oracle row limit")

;; -----------------------------------------------------------------------------
;;
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

(defun jh/oracle-gen-list-column-query (tabname &optional separator)
  "Generate list table columns query."
  (let ((sep (or separator ",")))
    (jh/concat-lines
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
      "    SELECT REPLACE(REPLACE(uclc.COMMENTS, CHR(13), ''), CHR(10), '\\n')"
      "      FROM USER_COL_COMMENTS uclc"
      "     WHERE uclc.COLUMN_NAME = utbc.COLUMN_NAME AND"
      "           uclc.TABLE_NAME = utbc.TABLE_NAME AND"
      "           ROWNUM <= 1"
      "  ) AS CONTENT"
      "  FROM USER_TAB_COLUMNS utbc"
      (format " WHERE UPPER(utbc.TABLE_NAME) = '%s';" tabname))))

(defun jh/oracle-gen-select-query (tabname colinfos &optional limit)
  "Generate SELECT query."
  (let ((limit (or limit jh/oracle-row-limit)) (fsep ",\n"))
    (jh/concat-lines
      "SELECT"
      (mapconcat #'(lambda (colinfo) (format "  %s" (car colinfo))) colinfos fsep)
      "FROM" (format "  %s" tabname)
      "WHERE" (format "  ROWNUM < %d;" limit))))

;; -----------------------------------------------------------------------------
;;
;; Machine-level SELECT Query Builder
;; -----------------------------------------------------------------------------

(defun jh/oracle-uniform-column (colinfo)
  "uniform oracle column select string."
  (let ((colname (nth 0 colinfo))
         (dbtype (nth 1 colinfo)))
    (cond
      ((member dbtype jh/oracle-string-datatype)
        (format
          "REPLACE(REPLACE(NVL(t.%s,'%s'),CHR(13),''),CHR(10),'%s')"
          colname jh/oracle-nsep jh/oracle-lsep))
      ((member dbtype jh/oracle-lob-datatype)
        (format
          "NVL(TO_CHAR(LENGTH(t.%s)),'%s')"
          colname jh/oracle-nsep))
      ((string= dbtype "DATE")
        (format
          "NVL(TO_CHAR(t.%s,'yyyy-mm-dd hh24:mi:ss'),'%s')"
          colname jh/oracle-nsep))
      (t (format "t.%s" colname)))))

(defun jh/oracle-gen-uniform-select-query (tabname colinfos &optional limit)
  "generate SELECT query with uniformed column select string."
  (let ((limit (or limit jh/oracle-row-limit)))
    (jh/concat-lines
      (format "SELECT '%s'||" jh/oracle-lpre)
      (mapconcat
        #'(lambda (colinfo)
            (format "  %s" (jh/oracle-uniform-column colinfo)))
        colinfos (format "||'%s'||\n" jh/oracle-fsep))
      "AS" "  CONTENT"
      "FROM" (format "  %s t" tabname)
      "WHERE" (format "  ROWNUM < %d;" limit))))

(defun jh/oracle-gen-uniform-count-query (tabname)
  "generate COUNT query."
  (format "SELECT '%s'|| COUNT(1) AS TOTAL FROM %s;" jh/oracle-lpre tabname))

;; -----------------------------------------------------------------------------
;;
;; Regexp and Line Parser
;; -----------------------------------------------------------------------------

(defun jh/oracle-parse-table-info (line)
  "Convert oracle line string to (tabname, tabcmt), otherwise return nil."
  (let
    ((regexp
       (concat
         "^\\([_A-Za-z0-9]+\\),"
         "\\(.*\\)$"))
      (tabinfo))
    (save-match-data
      (and (string-match regexp line)
        (setq
          tabname (match-string 1 line)
          tabcmt (match-string 2 line))
        (setq tabinfo (list tabname tabcmt))))
    tabinfo))

(defun jh/oracle-parse-columns-info (line)
  "Convert oracle line string to (colname dbtype dblen nullable unique colcmt)."
  (let
    ((regexp
       (concat
         "^[ \t]*\\([_A-Za-z0-9]+\\),"
         "[ \t]*\\([_A-Za-z0-9]+\\),"
         "[ \t]*\\([_A-Za-z0-9]*\\),"
         "[ \t]*\\([_A-Za-z0-9]*\\),"
         "[ \t]*\\([_A-Za-z0-9]*\\),"
         "[ \t]*\\([_A-Za-z0-9]*\\),"
         "[ \t]*\\([_A-Za-z0-9]*\\),"
         "[ \t]*\\(.*\\)$"))
      (colinfo))
    (save-match-data
      (and (string-match regexp line)
        (setq
          colname (match-string 1 line)
          dbtype (match-string 2 line)
          dblen (match-string 3 line)
          nullable (match-string 4 line)
          unique (match-string 5 line)
          pk (match-string 6 line)
          precision (match-string 7 line)
          colcmt (match-string 8 line))
        (setq colinfo
          (list
            colname dbtype (string-to-number dblen)
            nullable unique pk precision colcmt))))
    colinfo))

;; -----------------------------------------------------------------------------
;;
;; SQL-level Helper
;; -----------------------------------------------------------------------------

(defun jh/oracle-list-tables ()
  "List all tables in database."
  (let* ((query (jh/oracle-gen-list-table-query))
          (lines (split-string (jh/sql-execute query) "\n")))
    (remove-if 'null (mapcar #'jh/oracle-parse-table-info lines))))

(defun jh/oracle-list-columns (tabname)
  "List all columns in a table with given TABNAME."
  (let* ((query (jh/oracle-gen-list-column-query tabname))
          (lines (split-string (jh/sql-execute query) "\n")))
    (remove-if 'null (mapcar #'jh/oracle-parse-columns-info lines))))

;; -----------------------------------------------------------------------------
;;
;; Frontend Helper
;; -----------------------------------------------------------------------------

(defun jh/oracle-guess-tabname ()
  "Guess table name."
  (let
    ((tabnames (jh/java-tabnames))
      (anno (spt/read-entity-tabname (jh/current-buffer)))
      (name (jh/file-base-name (buffer-name)))
      (symb (thing-at-point 'symbol t)))
    (cond
      ((member anno tabnames) anno)
      ((member name tabnames) name)
      ((member symb tabnames) symb)
      (t (completing-read "Dump Table >> " tabnames)))))

;; -----------------------------------------------------------------------------
;;
;; YAML
;; -----------------------------------------------------------------------------

(defun jh/oracle-yamlfy-result-cell-value (value dbtype)
  "Make coldate to a string according to dbtype."
  (cond
    ((and (string= dbtype "NUMBER") (string= value "")) "null")
    ((member dbtype jh/oracle-string-datatype)
      (cond
        ;; multiple line string
        ((string-match-p (format ".*%s.*" jh/oracle-lsep) value)
          (jh/concat-lines "|"
            (mapconcat #'(lambda (line) (concat "    " line))
              (split-string value jh/oracle-lsep) "\n")))
        ;; long line string, which length is greater than 40
        ((> (length value) 40) (concat ">\n    " value))
        ;; null string
        ((string= value jh/oracle-nsep) "null")
        ;; default string
        (t (concat "\"" (jh/sql-escape-string value) "\""))))
    ((member dbtype jh/oracle-lob-datatype)
      (cond
        ;; null lob
        ((string= value jh/oracle-nsep)
          (format "# %s(null) #" dbtype))
        ;; default lob, just display size
        (t (format "# %s(%s) #" dbtype
             (file-size-human-readable (string-to-number value))))))
    ;; default return value
    (t (if (string= value jh/oracle-nsep) "null" value))))

(defun jh/oracle-yamlfy-result-cell (cell colinfo)
  "Convert result cell to a readable string."
  (let ((colname (nth 0 colinfo))
         (star (if (string= (nth 3 colinfo) "N") "*" ""))
         (colvalue (jh/oracle-yamlfy-result-cell-value cell (nth 1 colinfo))))
    (format "  %s%s: %s" star colname colvalue)))

(defun jh/oracle-yamlfy-result-row (index row colinfos)
  "Convert nth row line string to YAML file block."
  (let ((res (format "- ### Row %d ###" index)))
    (setq j 0)
    (dolist (cell row)
      (setq res
        (jh/concat-lines res
          (jh/oracle-yamlfy-result-cell cell (nth j colinfos))))
      (setq j (+ j 1)))
    res))

(defun jh/oracle-yamlfy-result-set (rows colinfos)
  "Convert result set to YAML file content."
  (let ((res))
    (setq i 1)
    (dolist (row rows)
      (setq res
        (jh/concat-lines
          (if (eq i 1) res (concat res "\n"))
          (jh/oracle-yamlfy-result-row i row colinfos)))
      (setq i (+ i 1)))
    res))

;; -----------------------------------------------------------------------------
;;
;; CSV
;; -----------------------------------------------------------------------------

(defun jh/oracle-csvfy-result-cell-value (value dbtype)
  "Make coldate to a string according to dbtype."
  (cond
    ((and (string= dbtype "NUMBER") (string= value "")) "null")
    ((member dbtype jh/oracle-string-datatype)
      (cond
        ;; null string
        ((string= value jh/oracle-nsep) "null")
        ;; default string
        (t (concat "\"" (jh/sql-escape-string value) "\""))))
    ((member dbtype jh/oracle-lob-datatype)
      (cond
        ;; null lob
        ((string= value jh/oracle-nsep)
          (format "%s(null)" dbtype))
        ;; default lob, just display size
        (t (format "%s(%s)" dbtype
             (file-size-human-readable (string-to-number value))))))
    ;; default return value
    (t (if (string= value jh/oracle-nsep) "null" value))))

(defun jh/oracle-csvfy-result-cell (cell colinfo)
  "Convert result cell to a readable string."
  (jh/oracle-csvfy-result-cell-value cell (nth 1 colinfo)))

(defun jh/oracle-csvfy-result-row (row colinfos separator)
  "Convert nth row line string to CSV file block."
  (let ((res))
    (setq j 0)
    (dolist (cell row)
      (setq res
        (concat (if (eq j 0) res (concat res separator))
          (jh/oracle-csvfy-result-cell cell (nth j colinfos))))
      (setq j (+ j 1)))
    res))

(defun jh/oracle-csvfy-result-set (rows colinfos &optional separator)
  "Convert result set to CSV file content."
  (let* ((sep (or separator ",")) (res (mapconcat #'car colinfos sep)))
    (dolist (row rows)
      (setq res
        (jh/concat-lines res
          (jh/oracle-csvfy-result-row row colinfos sep))))
    res))

;; -----------------------------------------------------------------------------
;;
;; Result Set Helper
;; -----------------------------------------------------------------------------

(defun jh/oracle-fetch-result-set (tabname)
  "Fetch oracle result set."
  (let ((colinfos (jh/oracle-list-columns tabname)))
    (mapcar
      #'(lambda (line)
          (split-string
            (jh/re-replace
              (concat "^" jh/oracle-lpre) "" line)
            jh/oracle-fsep))
      (remove-if-not
        #'(lambda (line)
            (string-match-p (concat "^" jh/oracle-lpre) line))
        (split-string
          (jh/sql-execute
            (jh/oracle-gen-uniform-select-query tabname colinfos)) "\n")))))

(defun jh/oracle-fetch-result-count (tabname)
  "Fetch total count."
  (string-to-number
    (jh/re-replace
      (concat "^" jh/oracle-lpre) ""
      (car
        (remove-if-not
          #'(lambda (line)
              (string-match-p (concat "^" jh/oracle-lpre) line))
          (split-string
            (jh/sql-execute
              (jh/oracle-gen-uniform-count-query tabname)) "\n"))))))

;; -----------------------------------------------------------------------------
;;
;; Interactive Commands
;; -----------------------------------------------------------------------------

(defun jh/oracle-dump-tables ()
  "Dump talbe name and comments."
  (interactive)
  (let ((tables (jh/oracle-list-tables)))
    (progn
      (switch-to-buffer "tables.txt")
      (kill-region (point-min) (point-max))
      (dolist (table tables)
        (let ((colname (nth 0 table))
               (comments (jh/strip (nth 1 table))))
          (insert (format "%s %s\n" colname comments))))
      (goto-char (point-min)))))

(defun jh/oracle-dump-rows-as-yaml ()
  "Dump table row data."
  (interactive)
  (let ((tabname (jh/oracle-guess-tabname)))
    (progn
      (switch-to-buffer (concat tabname ".yml"))
      (or (eq major-mode 'yaml-mode) (yaml-mode))
      (kill-region (point-min) (point-max))
      ;; insert title
      (insert (format "### Dump rows of %s ###" tabname))
      (insert (jh/oracle-yamlfy-result-set
                (jh/oracle-fetch-result-set tabname)
                (jh/oracle-list-columns tabname)))
      ;; go to the beigining
      (goto-char (point-min)))))

(defun jh/oracle-dump-rows-as-csv ()
  "Dump table row data."
  (interactive)
  (let ((tabname (jh/oracle-guess-tabname)))
    (progn
      (switch-to-buffer (concat tabname ".csv"))
      (or (eq major-mode 'csv-mode) (csv-mode))
      (kill-region (point-min) (point-max))
      ;; csv content
      (insert (jh/oracle-csvfy-result-set
                (jh/oracle-fetch-result-set tabname)
                (jh/oracle-list-columns tabname)))
      ;; (csv-align-fields t (point-min) (point-max))
      ;; go to the beigining
      (goto-char (point-min)))))

(defun jh/oracle-copy-list-table-query ()
  "Copy list table query to clipboard"
  (interactive)
  (jh/sent-to-clipboard (jh/oracle-gen-list-table-query)))

(defun jh/oracle-copy-list-column-query ()
  "Copy select query to clipboard."
  (interactive)
  (let*
    ((tabnames (mapcar #'car (jh/oracle-list-tables)))
      (tabname (completing-read "Dump Table >> " tabnames)))
    (jh/sent-to-clipboard (jh/oracle-gen-list-column-query tabname))))

(defun jh/oracle-copy-select-query ()
  "Copy select query to clipboard."
  (interactive)
  (let*
    ((tabnames (mapcar #'car (jh/oracle-list-tables)))
      (tabname (completing-read "Dump Table >> " tabnames)))
    (jh/sent-to-clipboard
      (jh/oracle-gen-select-query tabname (jh/oracle-list-columns tabname)))))

;; -----------------------------------------------------------------------------
;;  __  __       ____   ___  _
;; |  \/  |_   _/ ___| / _ \| |
;; | |\/| | | | \___ \| | | | |
;; | |  | | |_| |___) | |_| | |___
;; |_|  |_|\__, |____/ \__\_\_____|
;;         |___/
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;;  ____   ___  ____ _____ ____ ____  _____ ____
;; |  _ \ / _ \/ ___|_   _/ ___|  _ \| ____/ ___|
;; | |_) | | | \___ \ | || |  _| |_) |  _| \___ \
;; |  __/| |_| |___) || || |_| |  _ <| |___ ___) |
;; |_|    \___/|____/ |_| \____|_| \_\_____|____/
;; -----------------------------------------------------------------------------

(provide 'init-sql)
