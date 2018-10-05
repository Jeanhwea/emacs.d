(add-hook 'emacs-lisp-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq lisp-indent-offset 2)
    (setq tab-width 2)
    (setq prettify-symbols-alist
      '(
         ("lambda" . 955)               ; λ
         ("defun" . 119917)             ; 𝑭
         ))))

(provide 'init-lisp)
