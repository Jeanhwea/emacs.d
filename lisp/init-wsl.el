;; browse url inside WSL
(when
  (and (eq system-type 'gnu/linux)
    (string-match "Linux.*Microsoft.*Linux" (shell-command-to-string "uname -a")))
  (setq
    browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
    browse-url-generic-args     '("/c" "start")
    browse-url-browser-function #'browse-url-generic))

(defun wsl-copy (start end)
  (interactive "r")
  (shell-command-on-region start end "clip.exe")
  (deactivate-mark))

(defun wsl-paste ()
  (interactive)
  (let ((clipboard (shell-command-to-string "powershell.exe -command 'Get-Clipboard' 2> /dev/null")))
    ;; Remove Windows ^M characters
    (setq clipboard (replace-regexp-in-string "\r" "" clipboard))
    ;; Remove newline added by Powershell
    (setq clipboard (substring clipboard 0 -1))
    (insert clipboard)))

(provide 'init-wsl)
