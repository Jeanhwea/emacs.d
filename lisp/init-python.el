
(add-hook 'python-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)))

(when (string-equal "windows-nt" system-type)
  (add-hook 'python-mode-hook
    (lambda()
      (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i"))))

(provide 'init-python)
