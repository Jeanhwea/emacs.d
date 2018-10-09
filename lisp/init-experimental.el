;; -----------------------------------------------------------------------------
;; browse-at-remote
;; -----------------------------------------------------------------------------
;; (when (require 'browse-at-remote)
;;   (defalias 'open 'browse-url))


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

(provide 'init-experimental)
