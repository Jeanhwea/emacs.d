(require 'color-theme-sanityinc-solarized)

;; default use solarized dark theme
(load-theme 'sanityinc-solarized-dark t)


;; --------------------------------------------------------------------------
;; Add helper command
;; --------------------------------------------------------------------------
(defun light()
  "Activate a light color theme"
  (interactive)
  (load-theme 'sanityinc-solarized-light t)
  )

(defun dark()
  "Activate a light color theme"
  (interactive)
  (load-theme 'sanityinc-solarized-dark t)
  )


(provide 'init-themes)
