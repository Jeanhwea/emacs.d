;; this file add some unstable experimental feature


;; when compact large fonts cause lots of resources, the editor will be very slow
;; so just inhibit compacting, when using chinese font
(when (string-equal "windows-nt" system-type)
  (setq inhibit-compacting-font-caches t))


(setq-default
  recentf-max-saved-items 1000
  recentf-exclude '("/tmp/" "/ssh:"))


(provide 'init-experimental)
