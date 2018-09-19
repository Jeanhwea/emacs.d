(when (require 'color-theme-sanityinc-solarized nil t)

  ;; default use solarized dark theme
  (load-theme 'sanityinc-solarized-light t)

  ;; Add helper command to make changing color theme more faster
  (defun light()
    "Activate a light color theme"
    (interactive)
    (load-theme 'sanityinc-solarized-light t))

  (defun dark()
    "Activate a light color theme"
    (interactive)
    (load-theme 'sanityinc-solarized-dark t)))

(provide 'init-themes)
