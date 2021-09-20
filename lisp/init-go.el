(add-hook 'go-mode-hook
  #'(lambda()
      (setq
        ;; go get -u -v golang.org/x/tools/cmd/goimports
        gofmt-command "goimports"
        show-trailing-whitespace t
        indent-tabs-mode nil)
      ;; (flyspell-mode 1)
      (hl-line-mode 1)
      (rainbow-delimiters-mode 1)))

(defun jh/run-go-scratch (&optional file)
  "Run go scratch source code."
  (let*
    ((sbufname "*go-scratch-buffer*")
      (file (or file (buffer-file-name)))
      (filename file))
    (setq cmd (format "go run %s" file))
    (if (string-match-p ".*\\.go$" filename)
      (progn
        (save-buffer)
        (if (get-buffer sbufname)
          (setq sbuf (get-buffer sbufname))
          (setq sbuf (generate-new-buffer sbufname)))
        (shell-command cmd sbuf sbuf)
        (display-buffer sbuf)
        (message (format "Run %s" file)))
      (user-error (format "Not a valid go sratch file: %s" file)))))


(provide 'init-go)
