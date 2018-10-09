;; -----------------------------------------------------------------------------
;; org-mode
;; -----------------------------------------------------------------------------
(when (require 'org)

  (defvar jesenia-path
    (if (string-equal "windows-nt" system-type)
      "e:/Codes/common/jesenia"
      "~/Codes/common/jesenia"))

  (define-key global-map (kbd "C-c l") 'org-store-link)
  (define-key global-map (kbd "C-c a") 'org-agenda)

  (setq
    org-link-file-path-type 'relative
    org-todo-keywords
      '((sequence "TODO" "WAITING" "|" "DONE" "CANCLE"))
    org-agenda-files
      (list
        (concat jesenia-path "/todo/avic.org")
        (concat jesenia-path "/todo/learn.org")
        (concat jesenia-path "/todo/misc.org"))
    org-log-done t)


  (add-hook 'org-mode-hook
    (lambda ()
      (org-babel-do-load-languages
        'org-babel-load-languages
        '((js . t)
           (python . t))))))

(provide 'init-org)
