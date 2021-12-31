(add-hook 'json-mode-hook
  #'(lambda()
      (evil-define-key '(normal visual) 'local (kbd "<tab>") 'json-tab-action)
      (evil-define-key '(normal visual) 'local (kbd "S-<tab>") 'json-shift-tab-action)
      (highlight-current-line)
      (rainbow-delimiters-mode 1)))

(defun json-tab-action ()
  "Default <tab> key action for golang."
  (interactive)
  (jh/hs-toggle-hideshow))

(defun json-shift-tab-action ()
  "Default Shift-<tab> key action for golang."
  (interactive)
  (workflow-inflect-string))


(provide 'init-json)
