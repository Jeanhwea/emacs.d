;; -----------------------------------------------------------------------------
;; org-mode
;; -----------------------------------------------------------------------------
(when (require 'org)

  (defvar jesenia-dir
    (if (jh/windows?)
      "e:/Code/avic/notifly"
      "~/Documents/archive/dropbox/jesenia"))

  (setq
    org-link-file-path-type 'relative
    org-html-validation-link nil
    org-startup-with-inline-images t
    org-directory (and (file-directory-p jesenia-dir) jesenia-dir)
    org-agenda-files (and (file-directory-p jesenia-dir) (list jesenia-dir))
    org-todo-keywords
    '((sequence "TODO" "DOING" "|" "WAITING" "DONE" "CANCELED"))
    org-todo-keyword-faces
    '(("TODO" :foreground "#d33682" :weight bold)
       ("DOING" :foreground "#dc322f" :weight bold)
       ("WAITING" :foreground "#d33682")
       ("DONE" :foreground "#859900" :weight bold)
       ("CANCELED" :foreground "#2aa198"))
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
    org-confirm-babel-evaluate nil
    org-log-redeadline 'time
    org-log-done 'time
    org-html-postamble t
    org-html-postamble-format
    '(("en" "<p class=\"author\">Last Updated %T. Created by %a at %d.</p>")))


  (define-key global-map (kbd "C-c l") 'org-store-link)

  (defun jh/org-capture-task ()
    "Capture a task with default template in org-mode"
    (interactive)
    (org-capture nil "t"))
  (define-key global-map (kbd "<f9>") 'jh/org-capture-task)
  (define-key global-map (kbd "C-c t") 'jh/org-capture-task)

  (defun jh/pop-agenda-list-and-close-other-windows (split)
    "Popup org agenda list and close other windows"
    (interactive "P")
    (org-agenda nil "n")
    (when (not split)
      (delete-other-windows)))
  (define-key global-map (kbd "C-c a") 'jh/pop-agenda-list-and-close-other-windows)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; verb needs eval after org-mode
  ;;   https://github.com/federicotdn/verb
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (require 'verb)

  (add-hook 'org-mode-hook
    #'(lambda ()
        (setq
          pangu-spacing-real-insert-separtor t
          org-babel-python-command "python3")
        (pangu-spacing-mode 1)
        (org-babel-do-load-languages
          'org-babel-load-languages
          '(
             ;; ------------------------------------------------------------------
             (emacs-lisp . t)
             (java . t)
             (js . t)
             (python . t)
             (shell . t)
             (sql . t)
             ;; ------------------------------------------------------------------
             ))
        (unless (jh/windows?) (org-bullets-mode 1)))))

(provide 'init-org)
