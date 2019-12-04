(defun km/expand-codetta-command ()
  "Expand codetta command."
  (interactive)
  (ct/expand-command))

(defun km/drop-file (&optional startdir)
  "Drop the file content to current point according to action."
  (interactive)
  (let ((options
          '("Content" "Filename" "Relative Path" "Relative to Project Root"))
         (action (completing-read "Drop Action >> " options))
         (filename
           (expand-file-name
             (read-file-name "Drop file >> " (or startdir default-directory)))))
    (cond
      ((string= action "Content")
        (and (file-regular-p filename)
          (file-readable-p filename)
          (insert (jh/read-file-content filename))))
      ((string= action "Filename")
        (and (file-exists-p filename) (insert filename)))
      ((string= action "Relative Path")
        (and (file-exists-p filename)
          (insert (jh/relative-path filename default-directory))))
      ((string= action "Relative to Project Root")
        (and (file-exists-p filename)
          (insert
            (jh/relative-path filename
              (jh/git-project-root-dir default-directory)))))
      (t (error "Never happend in km/drop-file!")))))

(defun km/format-source-codes ()
  "Format codes."
  (interactive)
  (cond
    ((eq major-mode 'java-mode) (spt/meghanada-format-code))
    ((eq major-mode 'python-mode) (elpy-format-code))
    ((eq major-mode 'typescript-mode) (tide-format))
    (t (message "Ops, no format backend!"))))

(defun km/reveal-in-file-manager ()
  "Open the folder containing this buffer file"
  (interactive)
  (browse-url default-directory))

(defun km/shell-send-region ()
  "Send selected text to shell."
  (interactive)
  (when (jh/mac?)
    (if (use-region-p)
      (jh/iterm2-send-region)
      (jh/iterm2-send-string (thing-at-point 'line)))))

(defun km/M-x ()
  "Start a command M-x with prefix `^jh/'"
  (interactive)
  (counsel-M-x "jh/"))

(progn
  (define-prefix-command 'leader/f12)
  (define-key leader/f12 (kbd "c") 'km/expand-codetta-command)
  (define-key leader/f12 (kbd "d") 'km/drop-file)
  (define-key leader/f12 (kbd "f") 'km/format-source-codes)
  (define-key leader/f12 (kbd "r") 'km/reveal-in-file-manager)
  (define-key leader/f12 (kbd "s") 'km/shell-send-region)
  (define-key leader/f12 (kbd "x") 'km/M-x))
(global-set-key (kbd "M-]") 'leader/f12)


;; -----------------------------------------------------------------------------
;;  _____        _  __
;; |  ___| __   | |/ /___ _   _
;; | |_ | '_ \  | ' // _ \ | | |
;; |  _|| | | | | . \  __/ |_| |
;; |_|  |_| |_| |_|\_\___|\__, |
;;                        |___/
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; F1: Window, Frame, Narrow & widen
;; -----------------------------------------------------------------------------
(progn
  (define-prefix-command 'leader/f1)
  ;; Window
  (define-key leader/f1 (kbd "0") #'delete-window)
  (define-key leader/f1 (kbd "1") #'delete-other-windows)
  (define-key leader/f1 (kbd "2") #'split-window-below)
  (define-key leader/f1 (kbd "3") #'split-window-right)
  (define-key leader/f1 (kbd "f") #'toggle-frame-fullscreen)
  ;; Frame
  (define-key leader/f1 (kbd "5") #'make-frame-command)
  (define-key leader/f1 (kbd "6") #'delete-frame)
  ;; Narrow and Widen
  (define-key leader/f1 (kbd "n") #'narrow-to-region)
  (define-key leader/f1 (kbd "w") #'widen)
  ;; undotree
  (define-key leader/f1 (kbd "u") #'undo-tree-visualize)
  ;; color and theme
  (define-key leader/f1 (kbd "c") #'jh/cycle-color-theme)
  (define-key leader/f1 (kbd "t") #'jh/toggle-transparency)
  ;; Multiple line
  (define-key leader/f1 (kbd "|") #'mc/edit-lines)
  (define-key leader/f1 (kbd "=") #'mc/mark-all-like-this)
  (define-key leader/f1 (kbd "<left>") #'mc/mark-previous-like-this)
  (define-key leader/f1 (kbd "<right>") #'mc/mark-next-like-this)
  ;; evil mode
  (define-key leader/f1 (kbd "v") #'evil-mode))
(global-set-key (kbd "<f1>") 'leader/f1)
(global-set-key (kbd "M-o") 'other-window)


;; -----------------------------------------------------------------------------
;; F2: git and version control
;; -----------------------------------------------------------------------------
(progn
  (define-prefix-command 'leader/f2)
  ;; git
  (define-key leader/f2 (kbd "s") #'magit-status)
  (define-key leader/f2 (kbd "b") #'magit-blame)
  (define-key leader/f2 (kbd "c") #'magit-commit)
  (define-key leader/f2 (kbd "l") #'magit-log)
  (define-key leader/f2 (kbd "L") #'magit-log-all)
  (define-key leader/f2 (kbd "p") #'magit-push)
  (define-key leader/f2 (kbd "f") #'magit-fetch)
  ;; smerge
  (define-key leader/f2 (kbd "<left>") #'smerge-prev)
  (define-key leader/f2 (kbd "<right>") #'smerge-next)
  (define-key leader/f2 (kbd "<ret>") #'smerge-keep-current)
  (define-key leader/f2 (kbd "1") #'smerge-keep-mine)
  (define-key leader/f2 (kbd "2") #'smerge-keep-other)
  (define-key leader/f2 (kbd "3") #'smerge-keep-all)
  ;; open remote url
  (define-key leader/f2 (kbd "o") #'browse-at-remote))
(global-set-key (kbd "<f2>") 'leader/f2)

;; -----------------------------------------------------------------------------
;; F8: counsel & projectitle
;; -----------------------------------------------------------------------------
(progn
  (define-prefix-command 'leader/f8)
  ;; counsel command
  (define-key leader/f8 (kbd "a") #'counsel-ag)
  (define-key leader/f8 (kbd "g") #'counsel-git-grep)
  (define-key leader/f8 (kbd "f") #'counsel-git)
  (define-key leader/f8 (kbd "b") #'counsel-bookmark)
  (define-key leader/f8 (kbd "r") #'counsel-recentf)
  ;; projectile
  (define-key leader/f8 (kbd "p") #'projectile-switch-project)
  (define-key leader/f8 (kbd "=") #'projectile-replace)
  (define-key leader/f8 (kbd "c") #'projectile-compile-project)
  (define-key leader/f8 (kbd "u") #'projectile-run-project))
(global-set-key (kbd "<f8>") 'leader/f8)


;; -----------------------------------------------------------------------------
;; F12: Misc
;; -----------------------------------------------------------------------------
(progn
  (define-prefix-command 'leader/f12)
  (define-key leader/f12 (kbd "c") 'km/expand-codetta-command)
  (define-key leader/f12 (kbd "d") 'km/drop-file)
  (define-key leader/f12 (kbd "f") 'km/format-source-codes)
  (define-key leader/f12 (kbd "r") 'km/reveal-in-file-manager)
  (define-key leader/f12 (kbd "s") 'km/shell-send-region)
  (define-key leader/f12 (kbd "x") 'km/M-x))
(global-set-key (kbd "<f12>") 'leader/f12)

(provide 'init-keymate)
