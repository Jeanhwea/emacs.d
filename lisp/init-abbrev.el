(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(
     ;; ------------------------------------------------------------------------
     ("abz" "abcdefghijklmnopqrstuvwxyz")
     ("aqf" "A quick brown fox jumps over the lazy dog")
     ("atfer" "after")
     ("eml" "email")
     ;; ------------------------------------------------------------------------
     ))


(set-default 'abbrev-mode t)
(setq save-abbrevs nil)

(provide 'init-abbrev)
