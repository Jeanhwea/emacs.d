;; -----------------------------------------------------------------------------
;; org-mode
;; -----------------------------------------------------------------------------
(when (require 'org)

  ;; git clone  https://github.com/fniessen/org-html-themes.git ~/.emacs.d/site-lisp/org-html-themes
  (defvar agenda-dir
    (if (jh/windows?) "e:/Code/avic/notifly" "~/agenda"))

  (setq
    org-link-file-path-type 'relative
    org-html-validation-link nil
    org-startup-with-inline-images t
    org-directory (and (file-directory-p agenda-dir) agenda-dir)
    org-agenda-start-with-log-mode t
    ;; 双周报格式, 其实时间当前往前推 7 天
    org-agenda-span 'fortnight
    org-agenda-start-day "-7d"
    org-agenda-files (and (file-directory-p agenda-dir) (list agenda-dir))
    org-todo-keywords
    ;; '((sequence "TODO" "DOING" "|" "WAITING" "DONE" "CANCELED"))
    '((sequence "TODO" "WAIT" "|" "DONE" "DROP"))
    org-todo-keyword-faces
    '(("TODO" :foreground "#d33682" :weight bold)
       ("WAIT" :foreground "#d33682")
       ("DONE" :foreground "#859900" :weight bold)
       ("DROP" :foreground "#2aa198"))
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

  ;; export latex by xelatex
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
      '("ctexart"
         "\\documentclass[11pt]{ctexart}"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

  (require 'ox-beamer)

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

  (setq local-python-file "/usr/local/python3/bin/python3")

  (add-hook 'org-mode-hook
    #'(lambda ()
        (setq
          org-babel-python-command
          (if (file-exists-p local-python-file) local-python-file "python3")
          pangu-spacing-real-insert-separtor t)
        (pangu-spacing-mode 1)
        (org-babel-do-load-languages
          'org-babel-load-languages
          '(
             ;; ------------------------------------------------------------------
             (emacs-lisp . t)
             (C . t)
             (dot . t)
             (java . t)
             (js . t)
             (go . t)
             (tmux . t)
             (python . t)
             (perl . t)
             (ruby . t)
             (shell . t)
             (sql . t)
             (verb . t)
             ;; ------------------------------------------------------------------
             ))
        (when (jh/mac?) (org-bullets-mode 1))
        (add-to-list 'org-babel-default-header-args:python '(:preamble  . "# -*- coding: utf-8 -*-"))
        (add-to-list 'org-babel-default-header-args '(:eval . "never-export")))))

(provide 'init-org)
