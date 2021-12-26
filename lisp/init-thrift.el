(add-hook 'thrift-mode-hook
  #'(lambda()
      (setq thrift-indent-level 4)
      ;; thrift tab action
      (evil-define-key '(normal visual) 'local (kbd "TAB") 'thrift-tab-action)))

(defun jh/format-thrift-source (&optional file)
  "Format thrift source code."
  (let
    ((file (or file (buffer-file-name))))
    (progn
      (save-buffer)
      ;; format buffer
      (shell-command (format "sed -i 's/  */ /g;s/  *,/,/g' \"%s\"" file))
      ;; reload buffer
      (revert-buffer nil t)
      (jh/indent-current-buffer)
      ;; leave a messge
      (message (format "Formatted t %s" file)))))

(defun thrift-tab-action ()
  "Default <tab> key action for thrift."
  (interactive)
  (jh/tab-dwim))

(provide 'init-thrift)
