;; -----------------------------------------------------------------------------
;; tab, space width configuration
;; -----------------------------------------------------------------------------
(delete-selection-mode 1)
(setq-default
  indent-tabs-mode nil
  tab-width 4)

;; -----------------------------------------------------------------------------
;; editing lines: open lines, insert newline
;; -----------------------------------------------------------------------------
(defun jh/open-next-line (N)
  "Open N lines next the cursor."
  (interactive "P")
  (save-excursion
    (move-end-of-line 1)
    (if (integerp N)
      (open-line N)
      (open-line 1))))

(defun jh/open-previous-line (N)
  "Open N lines before the cursor."
  (interactive "P")
  (save-excursion
    (move-beginning-of-line 1)
    (if (integerp N)
      (newline N)
      (newline 1))))

(defun jh/newline-at-the-end-of-previous-line ()
  "Move to the end of previous line, enter a newline and indent."
  (interactive)
  (previous-line 1)
  (move-end-of-line 1)
  (newline-and-indent))

(defun jh/newline-at-the-end-of-line ()
  "Move to the end of the line, enter a newline and indent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

;; https://www.emacswiki.org/emacs/MoveLine
(defmacro save-column (&rest BODY)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@BODY)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

(defun jh/shift-up-line ()
  "Shift the line up."
  (interactive)
  (save-column
    (transpose-lines 1)
    (forward-line -2)))

(defun jh/shift-down-line ()
  "Shift the line down."
  (interactive)
  (save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

(global-set-key (kbd "C-o") 'jh/open-next-line)
(global-set-key (kbd "C-S-o") 'jh/open-previous-line)
(global-set-key (kbd "C-<return>") 'jh/newline-at-the-end-of-line)
(global-set-key (kbd "S-<return>") 'jh/newline-at-the-end-of-previous-line)
(global-set-key (kbd "M-p") 'jh/shift-up-line)
(global-set-key (kbd "M-n") 'jh/shift-down-line)
(global-set-key (kbd "C-c j") 'join-line)

;; -----------------------------------------------------------------------------
;; editing words: upcase, capitalized
;; -----------------------------------------------------------------------------
(defun jh/upcase-previous-word (N)
  "Convert previous word to upper case format, moving over."
  (interactive "P")
  (save-excursion
    (backward-word N)
    (if (integerp N)
      (upcase-word N)
      (upcase-word 1))))

(defun jh/capitalize-previous-word (N)
  "Convert previous word to capitalized format, moving over."
  (interactive "P")
  (save-excursion
    (backward-word N)
    (if (integerp N)
      (capitalize-word N)
      (capitalize-word 1))))

(global-set-key (kbd "M-c") 'jh/upcase-previous-word)
(global-set-key (kbd "M-h") 'jh/capitalize-previous-word)


;; -----------------------------------------------------------------------------
;; file operation
;; -----------------------------------------------------------------------------
(defun jh/new-scratch-buffer ()
  "Create a temporary buffer."
  (interactive)
  (let ((current-datetime-string (format-time-string "%Y%m%d%H%M%S")))
    (switch-to-buffer
      (concatenate 'string "scratch+" current-datetime-string))))

(defun jh/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is binding to this buffer!"))
  (when (yes-or-no-p
          (format "Delete %s: "
            (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun jh/rename-this-buffer-and-file (NAME)
  "Rename both current buffer and file it's visiting to NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
         (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename NAME 1))
      (set-visited-file-name NAME)
      (rename-buffer NAME))))


;; -----------------------------------------------------------------------------
;; mark
;; -----------------------------------------------------------------------------
(global-set-key (kbd "C-,") 'set-mark-command)
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-;") 'pop-global-mark)

;; -----------------------------------------------------------------------------
;; windows
;; -----------------------------------------------------------------------------
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "<f9>") 'make-frame-command)
(global-set-key (kbd "<f10>") 'delete-frame)
(global-set-key (kbd "C-M-f") 'toggle-frame-fullscreen)
(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c n") 'narrow-to-region)
(global-set-key (kbd "C-c w") 'widen)


;; -----------------------------------------------------------------------------
;; file accession
;; -----------------------------------------------------------------------------
(global-set-key (kbd "C-c r") 'recentf-open-files)
(global-set-key (kbd "M-7") 'recentf-open-files)
(global-set-key (kbd "M-8") 'list-buffers)
(global-set-key (kbd "M-9") 'list-bookmarks)

;; -----------------------------------------------------------------------------
;; undo-tree
;; -----------------------------------------------------------------------------
;;(when (require 'undo-tree)
;;  (global-undo-tree-mode))

;; -----------------------------------------------------------------------------
;; ag
;; -----------------------------------------------------------------------------
(when (require 'ag)
  (global-set-key (kbd "C-c a") 'ag))

;; -----------------------------------------------------------------------------
;; multiple-cursors
;; -----------------------------------------------------------------------------
(when (require 'multiple-cursors)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-c C-l") 'mc/edit-lines))

;; -----------------------------------------------------------------------------
;; expand-region
;; -----------------------------------------------------------------------------
(when (require 'expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region))

;; -----------------------------------------------------------------------------
;; projectile
;; -----------------------------------------------------------------------------
(when (require 'projectile)
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


(provide 'init-basic)
