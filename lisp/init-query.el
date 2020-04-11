;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ____                _              _
;;  / ___|___  _ __  ___| |_ __ _ _ __ | |_ ___
;; | |   / _ \| '_ \/ __| __/ _` | '_ \| __/ __|
;; | |__| (_) | | | \__ \ || (_| | | | | |_\__ \
;;  \____\___/|_| |_|___/\__\__,_|_| |_|\__|___/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst qy/snippets-dir (expand-file-name "query" user-emacs-directory)
  "The Directory that stores all query files.")

(defconst qy/dump-tables-file (expand-file-name "tables.sql" qy/snippets-dir)
  "Dump table SQL script file name.")

(defconst qy/dump-columns-file (expand-file-name "columns.sql" qy/snippets-dir)
  "Dump columns of a table SQL script file name.")

;; separators
(defconst qy/fsep "$ep" "Oracle field separator")
(defconst qy/lsep "#ew" "Oracle newline separator")
(defconst qy/nsep "#il" "Oracle null separator")
(defconst qy/lpre ":) " "Oracle line prefix")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ____                           _
;;  / ___| ___ _ __   ___ _ __ __ _| |_ ___  _ __
;; | |  _ / _ \ '_ \ / _ \ '__/ _` | __/ _ \| '__|
;; | |_| |  __/ | | |  __/ | | (_| | || (_) | |
;;  \____|\___|_| |_|\___|_|  \__,_|\__\___/|_|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun qy/replace-placeholder (str &optional fsep lsep nsep)
  "Replace common placeholder in STR."
  (jh/re-replace "&fsep" (or fsep qy/fsep)
    (jh/re-replace "&lsep" (or lsep qy/lsep)
      (jh/re-replace "&nsep" (or nsep qy/nsep)
        (jh/re-replace "&lpre" qy/lpre str)))))

(defun qy/gen-list-table-query ()
  "Generate list table query."
  (qy/replace-placeholder
    (jh/read-file-content qy/dump-tables-file) "," " "))

(defun qy/gen-list-column-query (tabname)
  "Generate list table columns query."
  (jh/re-replace "&tablename" tabname
    (qy/replace-placeholder
      (jh/read-file-content qy/dump-columns-file) "," " ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ____
;; |  _ \ __ _ _ __ ___  ___ _ __
;; | |_) / _` | '__/ __|/ _ \ '__|
;; |  __/ (_| | |  \__ \  __/ |
;; |_|   \__,_|_|  |___/\___|_|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  _____                _                 _
;; |  ___| __ ___  _ __ | |_ ___ _ __   __| |
;; | |_ | '__/ _ \| '_ \| __/ _ \ '_ \ / _` |
;; |  _|| | | (_) | | | | ||  __/ | | | (_| |
;; |_|  |_|  \___/|_| |_|\__\___|_| |_|\__,_|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-query)
