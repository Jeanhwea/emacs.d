
(add-hook 'python-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)))

(when (jh/windows?)
  (add-hook 'python-mode-hook
    (lambda()
      (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i"))))

(provide 'init-python)
