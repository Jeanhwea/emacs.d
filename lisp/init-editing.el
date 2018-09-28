;; key bindings
(global-set-key (kbd "C-,") 'set-mark-command)
(global-set-key (kbd "C-x C-,") 'pop-global-mark)
(global-set-key (kbd "C-j") 'join-line)


;; Tab, Space, indentation setup
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-width 4)

;; Language-independent indentation
(add-hook 'mhtml-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq html-indent-level 2)
    (setq tab-width 2)))

(add-hook 'js-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq js-indent-level 2)
    (setq tab-width 2)))

(add-hook 'typescript-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq typescript-indent-level 2)
    (setq tab-width 2)))

(add-hook 'css-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq css-indent-offset 2)
    (setq tab-width 2)))

(add-hook 'emacs-lisp-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq lisp-indent-offset 2)
    (setq tab-width 2)))

(add-hook 'sh-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq sh-basic-offset 2)
    (setq tab-width 2)))

(add-hook 'java-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq c-basic-offset 2)
    (setq tab-width 2)))

;; C-o to open next lines
(defun jh/open-next-line (N)
  "Open N lines next the cursor"
  (interactive "P")
  (save-excursion
    (move-end-of-line 1)
    (open-line N)))
(global-set-key (kbd "C-o") 'jh/open-next-line)

;; C-S-o to open previous lines
(defun jh/open-previous-line (N)
  "Open N lines before the cursor"
  (interactive "P")
  (save-excursion
    (move-beginning-of-line 1)
    (newline N)))
(global-set-key (kbd "C-S-o") 'jh/open-previous-line)


;; (defun jh/temporary-buffer ()
;;   "Create a temporary buffer"
;;   (interactive)
;;   (switch-to-buffer (make-temp-name "scratch+")))


(provide 'init-editing)
