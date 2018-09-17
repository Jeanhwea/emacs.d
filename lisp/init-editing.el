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


(defun jh/temporary-buffer ()
  "Create a temporary buffer"
  (interactive)
  (switch-to-buffer (make-temp-name "scratch+")))


(provide 'init-editing)
