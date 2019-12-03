(defun km/expand-codetta-command ()
  "Expand codetta command."
  (interactive)
  (ct/expand-command))

(defun km/drop-file (&optional startdir)
  "Drop the file content to current point according to action."
  (interactive)
  (let ((action
          (completing-read "Drop Action >> "
            '("Content" "Filename" "Relative Path" "Relative to Project Root")))
         (filename
           (expand-file-name
             (read-file-name "Drop file >> " (or startdir default-directory)))))
    (cond
      ((string= action "Content")
        (and (file-regular-p filename) (file-readable-p filename) (insert (jh/read-file-content filename))))
      ((string= action "Filename")
        (and (file-exists-p filename) (insert filename)))
      ((string= action "Relative Path")
        (and (file-exists-p filename) (insert (jh/relative-path filename default-directory))))
      ((string= action "Relative to Project Root")
        (and (file-exists-p filename)
          (insert (jh/relative-path filename (jh/git-project-root-dir default-directory)))))
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
  (define-prefix-command 'km/leader-key-map)
  (define-key km/leader-key-map (kbd "c") 'km/expand-codetta-command)
  (define-key km/leader-key-map (kbd "d") 'km/drop-file)
  (define-key km/leader-key-map (kbd "f") 'km/format-source-codes)
  (define-key km/leader-key-map (kbd "r") 'km/reveal-in-file-manager)
  (define-key km/leader-key-map (kbd "s") 'km/shell-send-region)
  (define-key km/leader-key-map (kbd "x") 'km/M-x))
(global-set-key (kbd "M-]") 'km/leader-key-map)



;; -----------------------------------------------------------------------------
;; F1: Window, Frame, Narrow & widen
;; -----------------------------------------------------------------------------
(progn
  ;; Leader Key
  (define-prefix-command 'leader/f1)
  ;; Window
  (define-key leader/f1 (kbd "0") #'delete-window)
  (define-key leader/f1 (kbd "1") #'delete-other-windows)
  (define-key leader/f1 (kbd "2") #'split-window-below)
  (define-key leader/f1 (kbd "3") #'split-window-right)
  (define-key leader/f1 (kbd "4") #'toggle-frame-fullscreen)
  ;; Frame
  (define-key leader/f1 (kbd "5") #'make-frame-command)
  (define-key leader/f1 (kbd "6") #'delete-frame)
  ;; Narrow and Widen
  (define-key leader/f1 (kbd "n") #'narrow-to-region)
  (define-key leader/f1 (kbd "w") #'widen))
(global-set-key (kbd "<f1>") 'leader/f1)
(global-set-key (kbd "M-o") 'other-window)

;; -----------------------------------------------------------------------------
;; F2: smerge-mode
;; -----------------------------------------------------------------------------
(setq smerge-command-prefix (kbd "<f2>"))

(provide 'init-keymate)
