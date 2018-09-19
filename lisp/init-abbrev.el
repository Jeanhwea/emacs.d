(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(
     ;; ----------------------------------------------------------------------
     ("abz" "abcdefghijklmnopqrstuvwxyz")
     ("aqbf" "A quick brown fox jumps over the lazy dog")
     ("eml" "hujinghui@buaa.edu.cn")
     ;; ----------------------------------------------------------------------
     ))


(set-default 'abbrev-mode t)
(setq save-abbrevs nil)

(provide 'init-abbrev)
