(when (require 'csv-mode)
  (add-hook 'csv-mode-hook #'(lambda () (csv-header-line))))

(provide 'init-csv)
