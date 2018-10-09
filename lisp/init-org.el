;; -----------------------------------------------------------------------------
;; org-mode
;; -----------------------------------------------------------------------------
(when (require 'org)
  (define-key global-map (kbd "C-c l") 'org-store-link)
  (define-key global-map (kbd "C-c a") 'org-agenda)

  (setq
    org-link-file-path-type 'relative
    org-log-done t)

  (cond
    ((string-equal "windows-nt" system-type)
      (setq org-agenda-files
        (list "e:/Codes/common/jesenia/todo/avic.org")))
    ((string-equal "darwin" system-type)
      (setq org-agenda-files
        (list "~/Codes/common/jesenia/todo/avic.org"))))

  (add-hook 'org-mode-hook
    (lambda ()
      (org-babel-do-load-languages
        'org-babel-load-languages
        '((js . t)
           (python . t))))))

(provide 'init-org)
