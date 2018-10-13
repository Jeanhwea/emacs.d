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
    org-startup-with-inline-images t
    org-directory (concat jesenia-path "/agenda")
    org-agenda-files
      (list (concat jesenia-path "/agenda"))
    org-todo-keywords
      '((sequence "TODO" "DOING" "|" "WAITING" "DONE" "CANCLE"))
    org-todo-keyword-faces
      '(("TODO" :foreground "#d33682" :weight bold)
        ("DOING" :foreground "#dc322f" :weight bold)
        ("WAITING" :foreground "#d33682")
        ("DONE" :foreground "#859900" :weight bold)
        ("CANCEL" :foreground "#2aa198"))
    org-default-priority ?C
    org-highest-priority ?A
    org-lowest-priority ?D
    org-priority-faces
      '((?A . (:foreground "#dc322f"))
        (?B . (:foreground "#b58900"))
        (?C . (:foreground "#268bd2"))
        (?D . (:foreground "#859900")))
    org-capture-templates
      '(("t" "capture some todos." entry
          (file+headline "todos.org" "TODOs")
          "** TODO %?\n   %a"))
    org-enforce-todo-dependencies t
    org-log-redeadline 'time
    org-log-done 'time)

  (define-key global-map (kbd "C-c l") 'org-store-link)

  (defun jh/org-capture-task ()
    "Capture a task with default template in org-mode"
    (interactive)
    (org-capture nil "t"))
  (define-key global-map (kbd "<f6>") 'jh/org-capture-task)
  (define-key global-map (kbd "C-c t") 'jh/org-capture-task)

  (defun jh/pop-agenda-list-and-close-other-windows (split)
    "Popup org agenda list and close other windows"
    (interactive "P")
    (org-agenda nil "n")
    (when (not split)
      (delete-other-windows)))
  (define-key global-map (kbd "C-c a") 'jh/pop-agenda-list-and-close-other-windows)
  (define-key global-map (kbd "<f5>") 'jh/pop-agenda-list-and-close-other-windows)

  (add-hook 'org-mode-hook
    (lambda ()
      (org-babel-do-load-languages
        'org-babel-load-languages
        '((js . t)
           (python . t))))))

(provide 'init-org)
