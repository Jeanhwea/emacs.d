(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(
     ;; ------------------------------------------------------------------------
     ("abz" "abcdefghijklmnopqrstuvwxyz")
     ("aqf" "A quick brown fox jumps over the lazy dog")
     ("eml" "hujinghui@buaa.edu.cn")
     ;; miss spelling
     ("atfer" "after")
     ("tabel" "table")
     ("envir" "environment")
     ("sceince" "science")
     ("scince" "science")
     ("feild" "field")
     ;; ------------------------------------------------------------------------
     ))


(set-default 'abbrev-mode t)
(setq save-abbrevs nil)

(provide 'init-abbrev)
