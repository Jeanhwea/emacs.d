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
(defconst qy/lpre ":) " "Oracle line prefix")
(defconst qy/nsep "#il" "Oracle null separator")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ____                           _
;;  / ___| ___ _ __   ___ _ __ __ _| |_ ___  _ __
;; | |  _ / _ \ '_ \ / _ \ '__/ _` | __/ _ \| '__|
;; | |_| |  __/ | | |  __/ | | (_| | || (_) | |
;;  \____|\___|_| |_|\___|_|  \__,_|\__\___/|_|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun qy/replace-placeholder (str &optional fsep lsep lpre nsep)
  "Replace common placeholder in STR."
  (jh/re-replace "&fsep" (or fsep qy/fsep)
    (jh/re-replace "&lsep" (or lsep qy/lsep)
      (jh/re-replace "&lpre" (or lpre qy/lpre)
        (jh/re-replace "&nsep" (or nsep qy/nsep) str)))))

(defun qy/gen-list-table-query ()
  "Generate list table query."
  (qy/replace-placeholder
    (jh/read-file-content qy/dump-tables-file) "," " " ""))

(defun qy/gen-list-column-query (tabname)
  "Generate list table columns query."
  (jh/re-replace "&tablename" tabname
    (qy/replace-placeholder
      (jh/read-file-content qy/dump-columns-file) "," " " "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ____
;; |  _ \ __ _ _ __ ___  ___ _ __
;; | |_) / _` | '__/ __|/ _ \ '__|
;; |  __/ (_| | |  \__ \  __/ |
;; |_|   \__,_|_|  |___/\___|_|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  _____                _                 _
;; |  ___| __ ___  _ __ | |_ ___ _ __   __| |
;; | |_ | '__/ _ \| '_ \| __/ _ \ '_ \ / _` |
;; |  _|| | | (_) | | | | ||  __/ | | | (_| |
;; |_|  |_|  \___/|_| |_|\__\___|_| |_|\__,_|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-query)
