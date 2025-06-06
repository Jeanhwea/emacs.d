;; https://www.emacswiki.org/emacs/MoveLine
(defmacro save-column (&rest BODY)
  `(let ((column (current-column)))
     (unwind-protect
       (progn ,@BODY)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

;; -----------------------------------------------------------------------------
;; editing lines: open lines, insert newline
;; -----------------------------------------------------------------------------
(defun jh/shiftup-line ()
  "Shift the line up."
  (interactive)
  (save-column
    (transpose-lines 1)
    (forward-line -2)))

(defun jh/shiftdown-line ()
  "Shift the line down."
  (interactive)
  (save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

(defun jh/joinline ()
  "Join line."
  (interactive)
  (if (use-region-p)
    (let*
      ((beg (region-beginning)) (end (region-end))
        (count (- (line-number-at-pos end) (line-number-at-pos beg))))
      (save-excursion
        (progn
          (deactivate-mark)
          (goto-char end)
          (dotimes (i count) (join-line)))))
    (join-line)))

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
(require 'string-inflection)


;; (when (require 'flyspell)
;;   (setq
;;     ;; ispell-personal-dictionary "C:/path/to/your/.ispell"
;;     ;; hunspell, aspell
;;     ispell-program-name "aspell"))

;; (setq ispell-silently-savep t)
(setq ispell-alternate-dictionary (expand-file-name "lang/words.txt" user-emacs-directory))

;; edit inside grep buffer, which C-c C-p to toggle readonly mode
(when (require 'wgrep))

;; -----------------------------------------------------------------------------
;; hungry-delete
;; -----------------------------------------------------------------------------
;; (when (require 'hungry-delete)
;;   (global-hungry-delete-mode 1))

;; -----------------------------------------------------------------------------
;; multiple-cursors
;; -----------------------------------------------------------------------------
;; (when (require 'multiple-cursors)
;;   (global-set-key (kbd "C-c C-l") 'mc/edit-lines))

;; -----------------------------------------------------------------------------
;; expand-region
;; -----------------------------------------------------------------------------
(when (require 'expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region))

;; -----------------------------------------------------------------------------
;; tramp
;; -----------------------------------------------------------------------------
(when (require 'tramp)
  (setq
    tramp-remote-shell "/bin/zsh"
    tramp-default-remote-shell "/bin/zsh"))

;; -----------------------------------------------------------------------------
;; evil-mode
;; -----------------------------------------------------------------------------
;; This should set before loading evil-mode, which is needed by evil-collection
(setq-default
  evil-want-integration t
  evil-want-keybinding nil
  ;; reserve <tab> for org-mode
  evil-want-C-i-jump nil)

(when (require 'evil)
  (evil-mode 1)
  (evil-commentary-mode))

(when (require 'evil-collection nil t)
  (evil-collection-init))

(when (require 'evil-pinyin nil t)
  (global-evil-pinyin-mode))

(provide 'init-edit)
