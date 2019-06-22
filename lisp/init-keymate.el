(defun km/format-source-codes ()
  "Format codes."
  (interactive)
  (cond
    ((eq major-mode 'java-mode) (spt/format-java-source-code))
    ((eq major-mode 'python-mode) (elpy-format-code))
    (t (message "Ops, no format backend!"))))

(progn
  (define-prefix-command 'km/leader-key-map)
  (define-key km/leader-key-map (kbd "c") 'ct/expand-command)
  (define-key km/leader-key-map (kbd "f") 'km/format-source-codes))
(global-set-key (kbd "M-k") 'km/leader-key-map)

(provide 'init-keymate)
