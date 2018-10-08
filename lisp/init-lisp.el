(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq lisp-indent-offset 2)
    (setq tab-width 2)))

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (setq prettify-symbols-alist
      (append prettify-symbols-alist
        '(
           ;; ("defun" . 119917)             ; 
           )))))


(provide 'init-lisp)
