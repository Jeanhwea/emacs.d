(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(
    
    ;;
    ("abz" "abcdefghijklmnopqrstuvwxyz")
    ("utf8" "-*- coding: utf-8 -*-" )
    ("eml" "hujinghui@buaa.edu.cn")

    ;;
    ))


(set-default 'abbrev-mode t)
(setq save-abbrevs nil)

(provide 'init-abbrev)
