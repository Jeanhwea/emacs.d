;; -----------------------------------------------------------------------------
;; engine-mode
;; -----------------------------------------------------------------------------
(when (require 'engine-mode)
  (defengine allacronyms
    "https://www.allacronyms.com/%s/abbreviated"
    :keybinding "a")

  (defengine codelf
    "https://unbug.github.io/codelf/#%s"
    :keybinding "v")

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
  (let ((dir (jh/git-root default-directory))
         (file (jh/git-relative-filename (buffer-file-name))))
    (unless file
      (error "file is nil"))
    (if (string-match-p "\\.md$" file)
      (when dir
        (jh/grip dir file))
      (error "Buffer '%s' is not a Markdown file!" file))))

;; -----------------------------------------------------------------------------
;; figlet
;; -----------------------------------------------------------------------------
(when (require 'figlet)
  (setq
    figlet-default-font "big"
    figlet-options '("-k")))

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

(when (require 'keyfreq)
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(when (require 'restclient))

;; -----------------------------------------------------------------------------
;; iterm2
;; -----------------------------------------------------------------------------
(when (jh/mac?)
  (defun jh/iterm2-maybe-escape-string (line)
    "escape double quote."
    (let* ((line (jh/re-replace "\\\\" "\\\\" line nil t))
            (line (jh/re-replace "\"" "\\\"" line nil t))
            (line (jh/re-replace "\'" "'\"'\"'" line nil t)))
      line))

  (defun jh/iterm2-maybe-remove-blank-lines (lines)
    "Maybe remove blank lines."
    (remove-if (lambda (line) (string-match-p "^\s*$" line)) lines))

  (defun jh/iterm2-write-line (line)
    "write line statement."
    (let* ((line (jh/iterm2-maybe-escape-string line)))
      (concat
        "-e 'delay 0.05' "
        "-e 'write text \""
        line
        "\"' ")))

  (defun jh/iterm2-osascript (lines)
    "Generate osascript for STR."
    (concat "osascript "
      "-e 'tell app \"iTerm2\"' "
      "-e 'tell current window' "
      "-e 'tell current session' "
      (mapconcat #'jh/iterm2-write-line lines " ")
      "-e 'end tell' "
      "-e 'end tell' "
      "-e 'end tell' "))

  (defun jh/iterm2-send-string (&optional cmd)
    "Send CMD to a running iTerm instance."
    (interactive)
    (setq cmd (or cmd (read-from-minibuffer "CMD > ")))
    (let* ((lines (split-string cmd "\n"))
            ;; (lines (jh/iterm2-maybe-remove-blank-lines lines))
            (script (jh/iterm2-osascript lines)))
      (shell-command script)))

  (defun jh/iterm2-cd (&optional dir)
    "Change iterm2 directory."
    (interactive)
    (jh/iterm2-send-string (concat "cd " (or dir default-directory))))

  (defun jh/iterm2-send-region ()
    "Send selected text to iterm2."
    (interactive)
    (if (use-region-p)
      (let ((beg (region-beginning))
             (end (region-end)))
        (progn
          (jh/iterm2-send-string (buffer-substring beg end))
          (deactivate-mark)))
      (error "select a region first!"))))

;; -----------------------------------------------------------------------------
;; tilix
;; -----------------------------------------------------------------------------
(when (jh/linux?)
  (defun jh/tilix-cd (&optional dir)
    "Open a tilix session, and set dir as working directory."
    (interactive)
    (let ((working-dir (directory-file-name (or dir default-directory))))
      (shell-command
        (format "tilix --working-directory='%s'" working-dir)))))

(provide 'init-experimental)
