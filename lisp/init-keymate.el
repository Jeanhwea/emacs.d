(defun km/format-source-codes ()
  "Format codes."
  (interactive)
  (cond
    ((eq major-mode 'java-mode) (spt/format-java-source-code))
    ((eq major-mode 'python-mode) (elpy-format-code))
    (t (message "Ops, no format backend!"))))

(defun km/M-x ()
  "Start a command M-x with prefix `^jh/'"
  (interactive)
  (counsel-M-x "jh/"))

(progn
  (define-prefix-command 'km/leader-key-map)
  (define-key km/leader-key-map (kbd "c") 'ct/expand-command)
  (define-key km/leader-key-map (kbd "f") 'km/format-source-codes)
  (define-key km/leader-key-map (kbd "x") 'km/M-x))
(global-set-key (kbd "M-k") 'km/leader-key-map)

(provide 'init-keymate)
