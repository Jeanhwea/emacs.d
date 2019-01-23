;; -----------------------------------------------------------------------------
;; engine-mode
;; -----------------------------------------------------------------------------
(when (require 'engine-mode)

  (defengine stackoverflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine bing
    "https://cn.bing.com/search?ensearch=1&q=%s"
    :keybinding "b")

  (defengine npm
    "https://www.npmjs.com/search?q=%s"
    :keybinding "n")

  (engine-mode t))

;; -----------------------------------------------------------------------------
;; web-mode
;; -----------------------------------------------------------------------------
(when (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode)))

;; -----------------------------------------------------------------------------
;; grip
;; -----------------------------------------------------------------------------
(defvar grip-buffer "*grip*")

(defun jh/grip (DIR FILENAME &optional HOST PORT)
  "start a grip in backgroud in DIR."
  (unless HOST (setq HOST "localhost"))
  (unless PORT (setq PORT 2758))
  (let ((default-directory DIR))
    (when (get-buffer-process grip-buffer)
      (quit-process grip-buffer))
    (start-process "grip" grip-buffer "grip" "-b" FILENAME
      (format "%s:%d" HOST PORT))))

(defun grip ()
  "start a grip daemon."
  (interactive)
  (let ((git-dir (jh/git-root-dir))
        (filename (jh/git-file-name)))
    (unless filename
      (error "filename is nil"))
    (if (string-match-p ".md\\'" filename)
      (when git-dir
        (jh/grip git-dir filename))
      (error "Buffer '%s' is not a Markdown file!" filename))))


;; -----------------------------------------------------------------------------
;; highlight-indent-guides
;; -----------------------------------------------------------------------------
(when (require 'highlight-indent-guides)
  (defun jh/highlighter (level responsive display)
    (if (> 1 level) nil
      (highlight-indent-guides--highlighter-default level responsive display)))
  (setq
    highlight-indent-guides-method 'character
    highlight-indent-guides-character 182 ; ¶
    highlight-indent-guides-auto-character-face-perc 0
    highlight-indent-guides-responsive 'top
    highlight-indent-guides-highlighter-function 'jh/highlighter))


(provide 'init-experimental)
