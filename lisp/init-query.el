;; const
(defconst qy/snippets-dir (expand-file-name "query" user-emacs-directory)
  "The Directory that stores all query files.")

(defconst qy/dump-tables-file (expand-file-name "tables.sql" qy/snippets-dir)
  "Dump table SQL script file name.")

(defconst qy/dump-columns-file (expand-file-name "columns.sql" qy/snippets-dir)
  "Dump columns of a table SQL script file name.")

(defun qy/oracle-gen-list-table-query (&optional separator)
  "Generate list table query."
  (let
    ((sep (or separator ","))
      (raw-query (qy/read-file-content qy/dump-tables-file)))
    (qy/re-replace "&fsep" sep raw-query)))

(defun qy/oracle-gen-list-column-query (tabname &optional separator)
  "Generate list table columns query."
  (let ((sep (or separator ","))
         (raw-query (qy/read-file-content qy/dump-columns-file)))
    (qy/re-replace "&tablename" tabname
      (qy/re-replace "&fsep" sep raw-query))))

(provide 'init-query)
