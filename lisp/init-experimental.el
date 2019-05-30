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
    "https://cn.bing.com/search?q=%s"
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

(defun jh/grip (dir file &optional host port)
  "start a grip in backgroud in DIR."
  (unless host (setq host "localhost"))
  (unless port (setq port 2758))
  (let ((default-directory dir))
    (when (get-buffer-process grip-buffer)
      (quit-process grip-buffer))
    (start-process "grip" grip-buffer "grip" "-b" file
      (format "%s:%d" host port))))

(defun grip ()
  "start a grip daemon."
  (interactive)
  (let ((dir (jh/git-project-root-dir default-directory))
         (file (jh/git-relative-filename (buffer-file-name))))
    (unless file
      (error "file is nil"))
    (if (string-match-p "\\.md$" file)
      (when dir
        (jh/grip dir file))
      (error "Buffer '%s' is not a Markdown file!" file))))


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
