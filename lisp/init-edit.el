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

(when (require 'flyspell)
  (setq
    ;; ispell-personal-dictionary "C:/path/to/your/.ispell"
    ;; hunspell, aspell
    ispell-program-name "aspell"))

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
;; evil-mode
;; -----------------------------------------------------------------------------
;; This should set before loading evil-mode, which is needed by evil-collection
(setq evil-want-integration t)
(setq evil-want-keybinding nil)

(when (require 'evil)
  (evil-mode 1))

(when (require 'evil-leader)
  (global-evil-leader-mode)

  (define-key evil-normal-state-map (kbd "<SPC>") 'evil-scroll-down)

  (evil-leader/set-leader ",")
  (evilnc-default-hotkeys)

  (evil-leader/set-key-for-mode 'java-mode "a" 'eglot-code-actions)
  (evil-leader/set-key-for-mode 'java-mode "r" 'eglot-rename)
  (evil-leader/set-key-for-mode 'python-mode "r" 'eglot-rename)
  (evil-leader/set-key-for-mode 'go-mode "r" 'eglot-rename)

  (evil-leader/set-key
    "cc" 'evilnc-comment-or-uncomment-lines
    "ee" 'workflow-alternative-buffer
    "es" 'workflow-eshell-open-from-here
    "ew" 'workflow-dot-open-file
    "f=" 'workflow-format-current-source
    "ff" 'workflow-buffer-or-recentf-open
    "fs" 'workflow-switch-to-buffer
    "gc" 'workflow-git-commit-changes
    "gf" 'workflow-git-open-file
    "gg" 'workflow-git-popup
    "hh" 'hs-toggle-hiding
    "kb" 'kill-buffer
    "ma" 'smerge-keep-all
    "mc" 'smerge-keep-current
    "mm" 'smerge-keep-mine
    "mo" 'smerge-keep-other
    "sa" 'workflow-save-all-buffers
    "sg" 'workflow-search-git
    "ss" 'workflow-search-any-text
    "wh" 'workflow-horizontal-split-window
    "wo" 'other-window
    "wv" 'workflow-vertical-split-window
    "ww" 'workflow-delete-other-windows
    "wx" 'workflow-close-current-window
    "xw" 'workflow-M-x
    "xx" 'counsel-M-x))

(when (require 'evil-collection nil t)
  (evil-collection-init))

(provide 'init-edit)
