(add-hook 'sql-mode-hook
  #'(lambda()
      (setq
        show-trailing-whitespace t
        indent-tabs-mode nil
        tab-width 2)
      ;; for sqlformat
      ;; npm install --global poor-mans-t-sql-formatter-cli
      (if (jh/windows?)
        (setq
          sqlformat-command 'sqlformat
          sqlformat-args
          '("-k" "upper" "-i" "upper" "-s" "--indent_width" "2" "-a"))
        (setq
          sqlformat-command 'pgformatter
          sqlformat-args '("-u" "2" "-s" "2" "-w" "80")))
      (sqlind-minor-mode 1)
      (hl-line-mode 1)))

(when (jh/windows?)
  (setq sql-mysql-program "mysql")
  (setq sql-mysql-options '("-C" "-f" "-t" "-n" "--default-character-set=utf8mb4")))

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
      (backward-char)
      (setq end (point)))
    ;; execute commands
    ;; (shell-command-on-region beg end "sqlformat -s \"pl/sql\"" nil t)
    (shell-command-on-region beg end
      "sqlformat - -k upper -i upper -a --indent_width 2 --wrap_after 80" nil t)
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

;; -----------------------------------------------------------------------------
;;  ____   ___  _       _   _      _
;; / ___| / _ \| |     | | | | ___| |_ __   ___ _ __
;; \___ \| | | | |     | |_| |/ _ \ | '_ \ / _ \ '__|
;;  ___) | |_| | |___  |  _  |  __/ | |_) |  __/ |
;; |____/ \__\_\_____| |_| |_|\___|_| .__/ \___|_|
;;                                  |_|
;; -----------------------------------------------------------------------------
(defun jh/sql-redirect (query)
  "Redirect a query."
  (let
    ((sqlbuf (sql-find-sqli-buffer)))
    (unless sqlbuf
      (error "No SQL interactive buffer found"))
    (sql-redirect sqlbuf query nil nil)))

(defun jh/sql-execute (query)
  "Execute a query."
  (let
    ((sqlbuf (sql-find-sqli-buffer))
      (outbuf "*SQL RESULT SET*")
      (result))
    (unless sqlbuf
      (error "No SQL interactive buffer found"))
    (progn
      (switch-to-buffer outbuf)
      (sql-execute sqlbuf outbuf query nil nil)
      (setq result (buffer-string))
      (kill-buffer outbuf))
    result))

(defun jh/sql-escape-string (str)
  "Escape string in sql text."
  (let*
    ((str (jh/re-replace "\\\\" "\\\\" str nil t))
      (str (jh/re-replace "\"" "\\\"" str nil t)))
    str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ___  ____      _    ____ _     _____
;;  / _ \|  _ \    / \  / ___| |   | ____|
;; | | | | |_) |  / _ \| |   | |   |  _|
;; | |_| |  _ <  / ___ \ |___| |___| |___
;;  \___/|_| \_\/_/   \_\____|_____|_____|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pagenation control
(defconst jh/database-pagesize 100 "Database output result set page size")

;; datatypes
(defconst jh/oracle-string-datatype '("CHAR" "NVARCHAR2" "VARCHAR" "VARCHAR2")
  "Oracle string datatype list")
(defconst jh/oracle-lob-datatype '("CLOB" "BLOB")
  "Oracle string datatype list")

;; separators
(defconst jh/oracle-lsep "#ew" "Oracle newline separator")
(defconst jh/oracle-nsep "#il" "Oracle null separator")
(defconst jh/oracle-fsep "$ep" "Oracle field separator")
(defconst jh/oracle-lpre ":) " "Oracle line prefix")

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
      "  ) AS ROWDATA"
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
      "  ) AS ROWDATA"
      "  FROM USER_TAB_COLUMNS utbc"
      (format " WHERE UPPER(utbc.TABLE_NAME) = '%s';" tabname))))

(defun jh/oracle-gen-select-columns (colinfos &optional fsep)
  "Generate oracle select columns string."
  (let
    ((trfn #'(lambda (colinfo) (format "  %s" (car colinfo))))
      (fsep (or fsep ",\n")))
    (mapconcat trfn colinfos fsep)))

(defun jh/oracle-init-buffer-params ()
  "Initialize local parameters."
  (progn
    (set
      (make-local-variable 'query-pagination-params)
      (make-hash-table :test 'equal))
    (puthash 'page-number 1 query-pagination-params)
    (puthash 'total 0 query-pagination-params)
    (puthash 'count 0 query-pagination-params)))

(defun jh/oracle-gen-pagination-condition ()
  "Generate oracle pagination where condition."
  (if (local-variable-p 'query-pagination-params)
    (let*
      ((ps jh/database-pagesize)
        (pn (or (gethash 'page-number query-pagination-params) 1))
        (rmin (* (- pn 1) ps))
        (rmax (* pn ps))
        (pagenation (format "ROWIDX > %d AND ROWIDX <= %d" rmin rmax)))
      pagenation)
    (format "ROWNUM <= %d" jh/database-pagesize)))

(defun jh/oracle-gen-select-query (tabname colinfos)
  "Generate SELECT query."
  (jh/concat-lines "SELECT"
    (jh/oracle-gen-select-columns colinfos)
    (format "FROM %s" tabname)
    (format "WHERE %s;" (jh/oracle-gen-pagination-condition))))

(defun jh/oracle-init-sqlplus ()
  "Initialize sqlplus parameters."
  (jh/sql-redirect
    (jh/concat-lines
      "SET LINESIZE 32767;")))

;; -----------------------------------------------------------------------------
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

(defun jh/oracle-gen-uniform-select-query (tabname colinfos)
  "generate SELECT query with uniformed column select string."
  (let
    ((colstr
       (mapconcat
         #'(lambda (colinfo)
             (format "    %s" (jh/oracle-uniform-column colinfo)))
         colinfos (format "||'%s'||\n" jh/oracle-fsep)))
      (where
        (if
          (local-variable-p 'query-where-params)
          query-where-params "1 = 1")))
    (jh/concat-lines
      "SELECT ROWDATA FROM ("
      "  SELECT ROWNUM AS ROWIDX,"
      (format "    '%s'||" jh/oracle-lpre)
      colstr
      "  AS ROWDATA"
      (format "  FROM %s t" tabname)
      (format "  WHERE %s" where)
      (format ") WHERE %s;" (jh/oracle-gen-pagination-condition)))))

(defun jh/oracle-gen-uniform-count-query (tabname)
  "generate COUNT query."
  (let
    ((where
       (if
         (local-variable-p 'query-where-params)
         query-where-params "1 = 1")))
    (format
      "SELECT '%s' || COUNT(1) AS TOTAL FROM %s WHERE %s;"
      jh/oracle-lpre
      tabname
      where)))

;; -----------------------------------------------------------------------------
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
         "^\s*\\([_A-Za-z0-9]+\\),"
         "\s*\\([_A-Za-z0-9]+\\),"
         "\s*\\([_A-Za-z0-9]*\\),"
         "\s*\\([_A-Za-z0-9]*\\),"
         "\s*\\([_A-Za-z0-9]*\\),"
         "\s*\\([_A-Za-z0-9]*\\),"
         "\s*\\([_A-Za-z0-9]*\\),"
         "\s*\\(.*\\)$"))
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
;; SQL-level Helper
;; -----------------------------------------------------------------------------

(defun jh/oracle-list-tables ()
  "List all tables in database."
  (jh/oracle-init-sqlplus)
  (let*
    ((query (jh/oracle-gen-list-table-query))
      (lines (split-string (jh/sql-execute query) "\n")))
    (remove-if 'null (mapcar #'jh/oracle-parse-table-info lines))))

(defun jh/oracle-list-columns (tabname)
  "List all columns in a table with given TABNAME."
  (jh/oracle-init-sqlplus)
  (let*
    ((query
       (jh/oracle-gen-list-column-query tabname))
      (lines (split-string (jh/sql-execute query) "\n")))
    (remove-if 'null (mapcar #'jh/oracle-parse-columns-info lines))))

;; -----------------------------------------------------------------------------
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
  (let*
    ((pagination (local-variable-p 'query-pagination-params))
      (pn
        (and pagination
          (gethash 'page-number query-pagination-params)))
      (cnt
        (and pagination
          (gethash 'count query-pagination-params)))
      (idx
        (and pagination
          (+ (* (1- pn) jh/database-pagesize) index)))
      (res
        (if pagination
          (format "- ### Row %d of %d in Page %d ###" idx cnt pn)
          (format "- ### Row %d ###" index))))
    (setq j 0)
    (dolist (cell row)
      (setq res
        (jh/concat-lines res
          (jh/oracle-yamlfy-result-cell cell (nth j colinfos))))
      (setq j (1+ j)))
    res))

(defun jh/oracle-yamlfy-resultset (rows colinfos)
  "Convert result set to YAML file content."
  (let ((res))
    (setq i 1)
    (dolist (row rows)
      (setq res
        (jh/concat-lines
          (if (eq i 1) res (concat res "\n"))
          (jh/oracle-yamlfy-result-row i row colinfos)))
      (setq i (1+ i)))
    res))

(defun jh/oracle-update-yaml-resultset (rows tabname colinfos)
  "Update yaml result set."
  (let*
    ((pagination (local-variable-p 'query-pagination-params))
      (ttl
        (and pagination
          (gethash 'total query-pagination-params)))
      (cnt
        (and pagination
          (gethash 'count query-pagination-params)))
      (header
        (if rows
          (if pagination
            (format "### Fetch %d rows in %d page of %s ###" cnt ttl tabname)
            (format "### Result set of %s ###" tabname))
          "Empty")))
    (progn
      (switch-to-buffer (concat tabname ".yml"))
      (or (eq major-mode 'yaml-mode) (yaml-mode))
      (kill-region (point-min) (point-max))
      ;; insert title
      (insert (jh/concat-lines header ""))
      ;; insert content
      (and rows (insert (jh/oracle-yamlfy-resultset rows colinfos)))
      ;; go to the beigining
      (goto-char (point-min)))))

;; -----------------------------------------------------------------------------
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
      (setq j (1+ j)))
    res))

(defun jh/oracle-csvfy-resultset (rows colinfos &optional separator)
  "Convert result set to CSV file content."
  (let* ((sep (or separator ",")) (res (mapconcat #'car colinfos sep)))
    (dolist (row rows)
      (setq res
        (jh/concat-lines res
          (jh/oracle-csvfy-result-row row colinfos sep))))
    res))

(defun jh/oracle-update-csv-resultset (rows tabname colinfos)
  "Dump table row data."
  (progn
    (switch-to-buffer (concat tabname ".csv"))
    (or (eq major-mode 'csv-mode) (csv-mode))
    (kill-region (point-min) (point-max))
    ;; csv content
    (insert (jh/oracle-csvfy-resultset rows colinfos))
    ;; align fields
    (csv-align-fields t (point-min) (point-max))
    ;; go to the beigining
    (goto-char (point-min))))

;; -----------------------------------------------------------------------------
;; Result Set Helper
;; -----------------------------------------------------------------------------

(defun jh/oracle-fetch-resultset (tabname &optional colinfos)
  "Fetch oracle result set."
  (jh/oracle-init-sqlplus)
  (let*
    ((colinfos (or colinfos (jh/oracle-list-columns tabname)))
      (query (jh/oracle-gen-uniform-select-query tabname colinfos)))
    (mapcar
      #'(lambda (line)
          (split-string
            (jh/re-replace
              (concat "^" jh/oracle-lpre) "" line)
            jh/oracle-fsep))
      (remove-if-not
        #'(lambda (line)
            (string-match-p (concat "^" jh/oracle-lpre) line))
        (split-string (jh/sql-execute query) "\n")))))

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

(defun jh/oracle-paginate-resultset (tabname &optional action goto-page)
  "Return a result set according to action."
  (and action
    (or
      (local-variable-p 'query-pagination-params)
      (jh/oracle-init-buffer-params))
    (let*
      ((count (jh/oracle-fetch-result-count tabname))
        (total (ceiling count jh/database-pagesize))
        (pn (gethash 'page-number query-pagination-params)))
      (puthash 'count count query-pagination-params)
      (puthash 'total total query-pagination-params)
      (cond
        ((equal 'first action)
          (puthash 'page-number 1 query-pagination-params))
        ((equal 'last action)
          (puthash 'page-number total query-pagination-params))
        ((equal 'next action)
          (and (< pn total)
            (puthash 'page-number (1+ pn) query-pagination-params)))
        ((equal 'prev action)
          (and (> pn 1)
            (puthash 'page-number (- pn 1) query-pagination-params)))
        ((equal 'goto action)
          (if
            (and
              (number-or-marker-p goto-page)
              (>= goto-page 1)
              (<= goto-page total))
            (puthash 'page-number goto-page query-pagination-params)
            (error "Page number should in range %d~%d" 1 total)))
        ((equal 'refresh action) (message "Refresh result set."))
        (t (user-error "Ops, unknown pagination action!")))))
  (jh/oracle-fetch-resultset tabname))

;; -----------------------------------------------------------------------------
;; Frontend Helper
;; -----------------------------------------------------------------------------

(defun jh/oracle-read-tabname ()
  "Guess table name."
  (let
    ((tabnames (mapcar #'car (jh/oracle-list-tables)))
      (name (jh/file-base-name (buffer-name)))
      (symb (thing-at-point 'symbol t)))
    (cond
      ((member name tabnames) name)
      ((member symb tabnames) symb)
      (t (completing-read "Dump Table >> " tabnames)))))

(defun jh/oracle-read-render-format ()
  "Get result set type."
  (let
    ((file (buffer-name)) (types '(csv yaml)))
    (cond
      ((string-match-p "\\.csv$" file) "csv")
      ((string-match-p "\\.yml$" file) "yaml")
      (t (completing-read "Result Set Type >> " types)))))

(defun jh/oracle-render-rows (rows tabname colinfos)
  "Render rows to file content."
  (let
    ((render-format (jh/oracle-read-render-format)))
    (cond
      ((string= render-format "csv")
        (jh/oracle-update-csv-resultset rows tabname colinfos))
      ((string= render-format "yaml")
        (jh/oracle-update-yaml-resultset rows tabname colinfos))
      (t (user-error "Ops, unknown file format to render.")))))

(defun jh/oracle-table-do (action &optional goto-page)
  "dump table row data."
  (let*
    ((tabname (jh/oracle-read-tabname))
      (colinfos (jh/oracle-list-columns tabname))
      (rows))
    (cond
      ((equal action 'refresh)
        (setq rows (jh/oracle-paginate-resultset tabname action)))
      ((member action '(first last next prev))
        (setq rows (jh/oracle-paginate-resultset tabname action)))
      ((equal action 'goto)
        (setq rows (jh/oracle-paginate-resultset tabname action goto-page)))
      (t (user-error "Ops, unknown database table action.")))
    (jh/oracle-render-rows rows tabname colinfos)))

;; -----------------------------------------------------------------------------
;; Interactive Commands
;; -----------------------------------------------------------------------------

(defun jh/oracle-tables-list ()
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

(defun jh/oracle-table-refresh ()
  "First page of oracle table."
  (interactive)
  (jh/oracle-table-do 'refresh))

(defun jh/oracle-table-first ()
  "First page of oracle table."
  (interactive)
  (jh/oracle-table-do 'first))

(defun jh/oracle-table-last ()
  "Last page of oracle talbe."
  (interactive)
  (jh/oracle-table-do 'last))

(defun jh/oracle-table-next ()
  "Next page of oracle talbe."
  (interactive)
  (jh/oracle-table-do 'next))

(defun jh/oracle-table-prev ()
  "Previous page of oracle talbe."
  (interactive)
  (jh/oracle-table-do 'prev))

(defun jh/oracle-table-goto ()
  "Previous page of oracle talbe."
  (interactive)
  (let
    ((page (read-number "Goto page >> ")))
    (jh/oracle-table-do 'goto page)))

(defun jh/oracle-table-where ()
  "Previous page of oracle talbe."
  (interactive)
  (let*
    ((wherep (local-variable-p 'query-where-params))
      (init-where (if wherep query-where-params ""))
      (where (read-string "WHERE >> " init-where)))
    (if wherep
      (setq query-where-params where)
      (set (make-local-variable 'query-where-params) where))))

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
