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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ____                           _
;;  / ___| ___ _ __   ___ _ __ __ _| |_ ___  _ __
;; | |  _ / _ \ '_ \ / _ \ '__/ _` | __/ _ \| '__|
;; | |_| |  __/ | | |  __/ | | (_| | || (_) | |
;;  \____|\___|_| |_|\___|_|  \__,_|\__\___/|_|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun qy/gen-list-table-query (&optional separator)
  "Generate list table query."
  (let
    ((sep (or separator ","))
      (raw-query (jh/read-file-content qy/dump-tables-file)))
    (jh/re-replace "&fsep" sep raw-query)))

(defun qy/gen-list-column-query (tabname &optional separator)
  "Generate list table columns query."
  (let ((sep (or separator ","))
         (raw-query (jh/read-file-content qy/dump-columns-file)))
    (jh/re-replace "&tablename" tabname
      (jh/re-replace "&fsep" sep raw-query))))

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
