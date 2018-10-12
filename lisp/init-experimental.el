(defun jh/open-shrimp-shell-as-temporary-shell ()
  "open a eshell as a temporary shell, and rename the buffer to `shrimp'."
  (interactive)
  (let ((shrimp-shell-name "shrimp-shell"))
    (when (get-buffer shrimp-shell-name)
      (kill-buffer shrimp-shell-name))
    (eshell)
    (rename-buffer shrimp-shell-name)))
(global-set-key (kbd "<f8>") 'jh/open-shrimp-shell-as-temporary-shell)

(defun jh/toggle-frame-fullscreen-mode()
  "toggle fulscreen, and toggle menu bar mode"
  (interactive)
  (if (string-equal "windows-nt" system-type)
    (toggle-frame-maximized)
    (toggle-frame-maximized)))
(global-set-key (kbd "<f11>") 'jh/toggle-frame-fullscreen-mode)

;; -----------------------------------------------------------------------------
;; browse-at-remote
;; -----------------------------------------------------------------------------
(when (require 'browse-at-remote)
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
(defun jh/git-home-directory (imagename)
  "Get the git home directory of current file."
  (interactive "sImage name: ")
  (let ((githome
          (file-name-directory (buffer-file-name))))
    (message githome)))

(require 'htmlize)

(provide 'init-experimental)
