(require 'company)

(add-hook 'after-init-hook 'global-company-mode)
(company-mode 1)

(provide 'init-company)
