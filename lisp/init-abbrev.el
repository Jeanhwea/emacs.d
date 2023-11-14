(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(
     ;; ------------------------------------------------------------------------
     ("abz" "abcdefghijklmnopqrstuvwxyz")
     ("aqf" "A quick brown fox jumps over the lazy dog")
     ("jhh" "hujinghui@buaa.edu.cn")
     ;; miss spelling
     ("atfer" "after")
     ("enviroment" "environment")
     ("feild" "field")
     ("goverment" "government")
     ("retrun" "return")
     ("sceince" "science")
     ("scince" "science")
     ("slef" "self")
     ("tihs" "this")
     ("thsi" "this")
     ("tshi" "this")
     ("tabel" "table")
     ("udpate" "update")
     ;; abbr
     ;; ("re" "return")
     ("ev" "environment")
     ;; ------------------------------------------------------------------------
     ))

(set-default 'abbrev-mode t)
(setq save-abbrevs nil)

(provide 'init-abbrev)
