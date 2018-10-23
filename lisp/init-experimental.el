(defun jh/open-shrimp-shell-as-temporary-shell ()
  "open a eshell as a temporary shell, and rename the buffer to `shrimp'."
  (interactive)
  (let ((shrimp-shell-name "shrimp"))
    (progn
      (when (get-buffer shrimp-shell-name)
        (kill-buffer shrimp-shell-name))
      (eshell)
      (rename-buffer shrimp-shell-name))))
(global-set-key (kbd "C-c s") 'jh/open-shrimp-shell-as-temporary-shell)
(global-set-key (kbd "<f8>") 'jh/open-shrimp-shell-as-temporary-shell)

;; -----------------------------------------------------------------------------
;; browse-at-remote
;; -----------------------------------------------------------------------------
(when (require 'browse-at-remote)
  (global-set-key (kbd "C-c b") 'bar-browse)
  (global-set-key (kbd "<f4>") 'bar-browse))


;; -----------------------------------------------------------------------------
;; engine-mode
;; -----------------------------------------------------------------------------
(when (require 'engine-mode)

  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine baidu
    "https://www.baidu.com/s?wd=%s"
    :keybinding "b")

  (defengine npm
    "https://www.npmjs.com/search?q=%s"
    :keybinding "n")

  (engine-mode t))


;; -----------------------------------------------------------------------------
;; clipboard-image
;; -----------------------------------------------------------------------------
(defun jh/git-root-dir ()
  "Get the root directory of a Git repository"
  (expand-file-name
    (shell-command-to-string
      "git rev-parse --show-cdup")))


(require 'htmlize)

(provide 'init-experimental)
