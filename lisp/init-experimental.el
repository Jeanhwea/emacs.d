;; this file add some unstable experimental feature


;; when compact large fonts cause lots of resources, the editor will be very slow
;; so just inhibit compacting, when using chinese font
(when (string-equal "windows-nt" system-type)
  (setq inhibit-compacting-font-caches t))


(provide 'init-experimental)
