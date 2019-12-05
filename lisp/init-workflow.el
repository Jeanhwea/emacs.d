(defun wf/symbol-or-selection-at-point ()
  "Read symbol and selection at point."
  (if (use-region-p)
    (let
      ((beg (region-beginning)) (end (region-end)))
      (deactivate-mark)
      (buffer-substring-no-properties beg end))
    (symbol-name (symbol-at-point))))

(defun workflow-replace ()
  "Better workflow for query-replace."
  (interactive)
  (let*
    ((old-text
       (read-string "Replace: "
         (wf/symbol-or-selection-at-point)))
      (new-text
        (read-string (format "Replace %s with: " old-text)
          old-text)))
    (query-replace old-text new-text)))

(provide 'init-workflow)
