;; -----------------------------------------------------------------------------
;; org-mode
;; -----------------------------------------------------------------------------
(when (require 'org)

  (defvar jesenia-path
    (if (string-equal "windows-nt" system-type)
      "e:/Codes/common/jesenia"
      "~/Codes/common/jesenia"))

  (setq
    org-link-file-path-type 'relative
    org-directory (concat jesenia-path "/agenda")
    org-agenda-files
      (list (concat jesenia-path "/agenda/"))
    org-todo-keywords
      '((sequence "TODO" "WAITING" "|" "DONE" "CANCLE"))
    org-default-priority ?B
    org-highest-priority ?A
    org-lowest-priority ?C
    org-priority-faces
      '((?A . (:foreground "#d33682" :weight bold))
        (?B . (:foreground "#c065db" :weight bold))
         (?C . (:foreground "#268bd2")))
    org-capture-templates
      '(("t" "capture some tasks." entry
          (file+headline "tasks.org" "Tasks")
          "** TODO %?\n  %i\n  %a"))
    org-log-done t)


  (define-key global-map (kbd "C-c l") 'org-store-link)

  (defun jh/org-capture-task ()
    "Capture a task with default template in org-mode"
    (interactive)
    (org-capture nil "t"))
  (define-key global-map (kbd "C-c t") 'jh/org-capture-task)

  (defun jh/pop-agenda-list-and-close-other-windows (split)
    "Popup org agenda list and close other windows"
    (interactive "P")
    (org-agenda nil "n")
    (when (not split)
      (delete-other-windows)))
  (define-key global-map (kbd "C-c a") 'jh/pop-agenda-list-and-close-other-windows)

  (add-hook 'org-mode-hook
    (lambda ()
      (org-babel-do-load-languages
        'org-babel-load-languages
        '((js . t)
           (python . t))))))

(provide 'init-org)
