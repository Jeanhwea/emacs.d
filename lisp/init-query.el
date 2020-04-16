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

(defconst qy/sppass-file (expand-file-name "~/.sppass")
  "Default sqlplus configuration file")

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
(defun qy/daemon-fork (file)
  "Start the query daemon with give configuration FILE."
  (let
    ((conn-str (car (jh/read-file-content-as-lines qy/sppass-file))))
    (start-process qy/daemon-name qy/daemon-buffer qy/daemon-prog "-s" conn-str)))

(defun qy/daemon-eval (str)
  "Send STR to query daemon."
  (or
    (get-buffer qy/daemon-buffer)
    (error "Query Daemon haven't start"))
  (process-send-string qy/daemon-name str))

(defun qy/daemon-init ()
  "Initialize daemon client."
  (qy/daemon-eval "set linesize 32767\n")
  (qy/daemon-eval "set pagesize 9999\n")
  (qy/daemon-eval "set feedback off\n"))

(defun qy/daemon-start ()
  "Start a client as the query daemon."
  (interactive)
  (or (file-exists-p qy/sppass-file)
    (error "Need configuration file: %s" qy/sppass-file))
  (and (get-buffer qy/daemon-buffer)
    (error "Query daemon already started!"))
  (progn
    (qy/daemon-fork qy/sppass-file) (qy/daemon-init)))

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

(defun qy/sql-execute (query)
  "Execute a QUERY and get the results as list."
  (unless qy/daemon-buffer (error "No SQL interactive buffer found"))
  (let
    ((presym (concat "^" qy/lpre)))
    (mapcar
      #'(lambda (line) (jh/re-replace presym "" line))
      (remove-if-not
        #'(lambda (line) (string-match-p presym line))
        (split-string (qy/daemon-execute query) "\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  _____
;; |  __ \
;; | |__) |__ _  _ __  ___   ___  _ __
;; |  ___// _` || '__|/ __| / _ \| '__|
;; | |   | (_| || |   \__ \|  __/| |
;; |_|    \__,_||_|   |___/ \___||_|
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun qy/parse-table-row-data (str)
  "Parse the table row data."
  (let
    ((fields (split-string str qy/fsep))
      (tabinfo (make-hash-table :test 'equal :size 2)))
    (puthash 'tabname (nth 0 fields) tabinfo)
    (puthash 'tabcmt (nth 1 fields) tabinfo)
    tabinfo))

(defun qy/parse-column-row-data (str)
  "Parse the table column row data."
  (let
    ((fields (split-string str qy/fsep))
      (colinfo (make-hash-table :test 'equal :size 8)))
    (puthash 'ispk  (> (length (nth 0 fields)) 0) colinfo)
    (puthash 'isuniq (> (length (nth 1 fields)) 0) colinfo)
    (puthash 'isnul (> (length (nth 2 fields)) 0) colinfo)
    (puthash 'colname (nth 3 fields) colinfo)
    (puthash 'coltype (nth 4 fields) colinfo)
    (puthash 'collen (string-to-number (nth 5 fields)) colinfo)
    (puthash 'colpcs (string-to-number (nth 6 fields)) colinfo)
    (puthash 'colcmt (nth 7 fields) colinfo)
    colinfo))

(defun qy/read-tables-meta-data ()
  "Read all tables meta data in a database."
  (mapcar #'qy/parse-table-row-data
    (qy/sql-execute (qy/gen-list-table-query))))

(defun qy/read-columns-meta-data (tabname)
  "Read all columns meta data of a table TABNAME."
  (mapcar #'qy/parse-column-row-data
    (qy/sql-execute (qy/gen-list-column-query tabname))))

(provide 'init-query)
