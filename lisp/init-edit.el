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
;; (defun jh/upcase-previous-word (N)
;;   "Convert previous word to upper case format, moving over."
;;   (interactive "P")
;;   (save-excursion
;;     (backward-word N)
;;     (if (integerp N)
;;       (upcase-word N)
;;       (upcase-word 1))))

;; (defun jh/capitalize-previous-word (N)
;;   "Convert previous word to capitalized format, moving over."
;;   (interactive "P")
;;   (save-excursion
;;     (backward-word N)
;;     (if (integerp N)
;;       (capitalize-word N)
;;       (capitalize-word 1))))

;; (defun jh/upcase-symbol-at-point ()
;;   "Convert symbol at point to upper case format, moving over."
;;   (interactive)
;;   (let ((text (thing-at-point 'symbol))
;;         (bounds (bounds-of-thing-at-point 'symbol)))
;;     (when text
;;       (setq new-text (upcase text))
;;       (delete-region (car bounds) (cdr bounds))
;;       (insert new-text))))

;; -----------------------------------------------------------------------------
;; string-inflection
;; -----------------------------------------------------------------------------
(when (require 'string-inflection)
  (global-set-key (kbd "M-u") 'string-inflection-all-cycle))


;; -----------------------------------------------------------------------------
;; pangu-spacing
;; -----------------------------------------------------------------------------
(when (require 'pangu-spacing)
  (global-pangu-spacing-mode 1))

;; -----------------------------------------------------------------------------
;; hungry-delete
;; -----------------------------------------------------------------------------
(when (require 'hungry-delete)
  (global-hungry-delete-mode 1))


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


(provide 'init-edit)
