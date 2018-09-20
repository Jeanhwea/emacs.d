;; this file add some unstable experimental feature


;; when compact large fonts cause lots of resources, the editor will be very slow
;; so just inhibit compacting, when using chinese font
(when (string-equal "windows-nt" system-type)
  (setq inhibit-compacting-font-caches t))


(setq-default
  recentf-max-saved-items 1000
  recentf-exclude '("/tmp/" "/ssh:"))


(when (string-equal "windows-nt" system-type)
  (add-hook 'python-mode-hook
    (lambda()
      (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i"))))


(provide 'init-experimental)
