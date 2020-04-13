;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   _____                    _                 _
;;  / ____|                  | |               | |
;; | |      ___   _ __   ___ | |_  __ _  _ __  | |_  ___
;; | |     / _ \ | '_ \ / __|| __|/ _` || '_ \ | __|/ __|
;; | |____| (_) || | | |\__ \| |_| (_| || | | || |_ \__ \
;;  \_____|\___/ |_| |_||___/ \__|\__,_||_| |_| \__||___/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst qy/snippets-dir (expand-file-name "query" user-emacs-directory)
  "The Directory that stores all query files.")

(defconst qy/dump-tables-file (expand-file-name "dump-tables.sql" qy/snippets-dir)
  "Dump table SQL script file name.")

(defconst qy/dump-columns-file (expand-file-name "dump-columns.sql" qy/snippets-dir)
  "Dump columns of a table SQL script file name.")

;; separators
(defconst qy/fsep "$ep" "Oracle field separator")
(defconst qy/lsep "#ew" "Oracle newline separator")
(defconst qy/nsep "#il" "Oracle null separator")
(defconst qy/lpre ":) " "Oracle line prefix")

;; process
(defconst qy/daemon-name "query-daemon")
(defconst qy/daemon-buffer "*query-buffer*")
(defconst qy/daemon-prog "sqlplus")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   _____                                 _
;;  / ____|                               | |
;; | |  __   ___  _ __    ___  _ __  __ _ | |_  ___   _ __
;; | | |_ | / _ \| '_ \  / _ \| '__|/ _` || __|/ _ \ | '__|
;; | |__| ||  __/| | | ||  __/| |  | (_| || |_| (_) || |
;;  \_____| \___||_| |_| \___||_|   \__,_| \__|\___/ |_|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun qy/replace-pipe (str &optional fsep lsep nsep)
  "Replace common placeholder in STR."
  (let*
    ((str (jh/re-replace "&lpre" qy/lpre str nil t))
      (str (jh/re-replace "&fsep" (or fsep qy/fsep) str nil t))
      (str (jh/re-replace "&lsep" (or fsep qy/lsep) str nil t))
      (str (jh/re-replace "&nsep" (or fsep qy/nsep) str nil t)))
    str))

(defun qy/gen-list-table-query ()
  "Generate list table query."
  (qy/replace-pipe (jh/read-file-content qy/dump-tables-file)))

(defun qy/gen-list-column-query (tabname)
  "Generate list table columns query."
  (jh/re-replace "&tablename" tabname
    (qy/replace-pipe (jh/read-file-content qy/dump-columns-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  _____
;; |  __ \
;; | |  | |  __ _   ___  _ __ ___    ___   _ __
;; | |  | | / _` | / _ \| '_ ` _ \  / _ \ | '_ \
;; | |__| || (_| ||  __/| | | | | || (_) || | | |
;; |_____/  \__,_| \___||_| |_| |_| \___/ |_| |_|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun qy/daemon-fork (user pass sid host port)
  "Start the query daemon."
  (start-process
    qy/daemon-name qy/daemon-buffer qy/daemon-prog
    (format "%s/%s@%s:%d/%s" user pass host port sid)))

(defun qy/daemon-eval (str)
  "Send STR to query daemon."
  (process-send-string qy/daemon-name str))

(defun qy/daemon-init ()
  "Initialize daemon client."
  (qy/daemon-eval "set linesize 30000\n")
  (qy/daemon-eval "set pagesize 9999\n")
  (qy/daemon-eval "set feedback off\n"))

(defun qy/start-client (user pass sid &optional host port)
  "Start a client as the query daemon."
  (interactive)
  (let
    ((host (or host "localhost")) (port (or port 1521)))
    (progn
      (qy/daemon-fork user pass sid host port)
      (qy/daemon-init))))

(defun qy/daemon-execute (query)
  "Execute a query and get output as string."
  (let ((oldbuf (current-buffer)) (result))
    (with-current-buffer qy/daemon-buffer
      (kill-region  (point-min) (point-max))
      (qy/daemon-eval query)
      ;; sqlplus needs sleep a while for responsing
      (sleep-for 1)
      (setq result
        (buffer-substring-no-properties (point-min) (point-max))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  _____
;; |  __ \
;; | |__) |__ _  _ __  ___   ___  _ __
;; |  ___// _` || '__|/ __| / _ \| '__|
;; | |   | (_| || |   \__ \|  __/| |
;; |_|    \__,_||_|   |___/ \___||_|
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun qy/sql-execute (query)
  "Execute a QUERY and get the results as list."
  (let
    ((sqlbuf (sql-find-sqli-buffer))
      (outbuf "*SQL RESULT SET*")
      (result))
    (unless sqlbuf
      (error "No SQL interactive buffer found"))
    ;; execute a query, and get raw results
    (progn
      (switch-to-buffer outbuf)
      (sql-redirect sqlbuf "SET LINESIZE 32767;" nil nil)
      (sql-redirect sqlbuf "SET PAGESIZE 9999;" nil nil)
      (sql-execute sqlbuf outbuf query nil nil)
      (setq result (buffer-string))
      (kill-buffer outbuf))
    ;; extract results line, and return a list
    (let
      ((presym (concat "^" qy/lpre)))
      (mapcar
        #'(lambda (line) (jh/re-replace presym "" line))
        (remove-if-not
          #'(lambda (line) (string-match-p presym line))
          (split-string result "\n"))))))

(defun qy/parse-table-row-data (str)
  "Parse the table row data."
  (let
    ((fields (split-string str qy/fsep)))
    `((tabname . ,(nth 0 fields))
       (tabcmt . ,(nth 1 fields)))))

(defun qy/parse-column-row-data (str)
  "Parse the table column row data."
  (let
    ((fields (split-string str qy/fsep)))
    `((ispk . ,(> (length (nth 0 fields)) 0))
       (isuniq . ,(> (length (nth 1 fields)) 0))
       (isnul . ,(> (length (nth 2 fields)) 0))
       (colname . ,(nth 3 fields))
       (coltype . ,(nth 4 fields))
       (collen . ,(string-to-number (nth 5 fields)))
       (colpcs . ,(string-to-number (nth 6 fields)))
       (colcmt . ,(nth 7 fields)))))

(defun qy/read-tables-meta-data ()
  "Read all tables meta data in a database."
  (mapcar #'qy/parse-table-row-data
    (qy/sql-execute (qy/gen-list-table-query))))

(defun qy/read-columns-meta-data (tabname)
  "Read all columns meta data of a table TABNAME."
  (mapcar #'qy/parse-column-row-data
    (qy/sql-execute (qy/gen-list-column-query tabname))))

(provide 'init-query)
