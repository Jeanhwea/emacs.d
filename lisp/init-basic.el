;; -----------------------------------------------------------------------------
;; mark
;; -----------------------------------------------------------------------------
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

;; -----------------------------------------------------------------------------
;; windows
;; -----------------------------------------------------------------------------
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-o") 'other-window)

(if (string-equal "windows-nt" system-type)
  (global-set-key (kbd "C-M-f") 'toggle-frame-maximized)
  (global-set-key (kbd "C-M-f") 'toggle-frame-fullscreen))

;; -----------------------------------------------------------------------------
;; editing lines: open lines, insert newline
;; -----------------------------------------------------------------------------
(defun jh/open-next-line (N)
  "Open N lines next the cursor"
  (interactive "P")
  (save-excursion
    (move-end-of-line 1)
    (open-line N)))

(defun jh/open-previous-line (N)
  "Open N lines before the cursor"
  (interactive "P")
  (save-excursion
    (move-beginning-of-line 1)
    (newline N)))

(defun jh/newline-at-the-end-of-previous-line ()
  "Move to the end of previous line, enter a newline and indent"
  (interactive)
  (previous-line 1)
  (move-end-of-line 1)
  (newline-and-indent))

(defun jh/newline-at-the-end-of-line ()
  "Move to the end of the line, enter a newline and indent"
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "C-o") 'jh/open-next-line)
(global-set-key (kbd "C-S-o") 'jh/open-previous-line)
(global-set-key (kbd "C-<return>") 'jh/newline-at-the-end-of-line)
(global-set-key (kbd "S-<return>") 'jh/newline-at-the-end-of-previous-line)
(global-set-key (kbd "C-j") 'join-line)

;; -----------------------------------------------------------------------------
;; editing words: upcase, capitalized
;; -----------------------------------------------------------------------------
(defun jh/upcase-previous-word (N)
  "Convert previous word to upper case format, moving over"
  (interactive "P")
  (save-excursion
    (backward-word N)
    (if (integerp N)
      (upcase-word N)
      (upcase-word 1))))

(defun jh/capitalize-previous-word (N)
  "Convert previous word to capitalized format, moving over"
  (interactive "P")
  (save-excursion
    (backward-word N)
    (if (integerp N)
      (capitalize-word N)
      (capitalize-word 1))))

(global-set-key (kbd "M-u") 'jh/upcase-previous-word)
(global-set-key (kbd "M-c") 'jh/capitalize-previous-word)


;; (defun jh/new-temporary-buffer ()
;;   "Create a temporary buffer"
;;   (interactive)
;;   (switch-to-buffer (make-temp-name "scratch+")))


;; -------------------------------------------------------------------------
;; undo-tree
;; -------------------------------------------------------------------------
(when (require 'undo-tree)
  (global-undo-tree-mode))


;; -------------------------------------------------------------------------
;; expand-region
;; -------------------------------------------------------------------------
(when (require 'expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region))


;; -----------------------------------------------------------------------------
;; tab, space width configuration
;; -----------------------------------------------------------------------------
(setq-default
  delete-selection-mode t
  indent-tabs-mode nil
  tab-width 4)


(provide 'init-basic)
