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
;; tramp
;; -----------------------------------------------------------------------------
(when (require 'tramp)
  (setq tramp-remote-shell "/bin/bash"))

;; -----------------------------------------------------------------------------
;; evil-mode
;; -----------------------------------------------------------------------------
;; This should set before loading evil-mode, which is needed by evil-collection
(setq evil-want-integration t)
(setq evil-want-keybinding nil)

(when (require 'evil)
  (evil-mode 1)
  (evil-commentary-mode))

(when (and (require 'evil-leader) (require 'evil-numbers))
  (global-evil-leader-mode)

  (evil-define-key '(normal visual) 'global (kbd "<SPC>") 'evil-scroll-down)
  (evil-define-key '(normal visual) 'global (kbd "gp") 'xref-pop-marker-stack)
  (evil-define-key '(normal visual) 'global (kbd "go") 'workflow-find-definitions-other-window)
  (evil-define-key '(normal visual) 'global (kbd "gl") 'workflow-find-definitions-lookup)
  (evil-define-key '(normal visual) 'global (kbd "gr") 'workflow-find-references)
  (evil-define-key '(normal visual) 'global (kbd "gi") 'workflow-goto-implementation)
  (evil-define-key '(normal visual) 'global (kbd "gs") 'workflow-search-rg)

  (evil-leader/set-leader ",")

  ;; (evil-leader/set-key-for-mode 'java-mode "a" 'eglot-code-actions)
  ;; (evil-leader/set-key-for-mode 'java-mode "r" 'eglot-rename)
  ;; (evil-leader/set-key-for-mode 'python-mode "r" 'eglot-rename)
  ;; (evil-leader/set-key-for-mode 'go-mode "r" 'eglot-rename)

  (evil-leader/set-key
    "bb" 'list-bookmarks
    "bB" 'workflow-bookmark-current-file
    "cw" 'workflow-replace
    "db" 'gud-break
    "dc" 'gud-cont
    "dn" 'gud-next
    "dp" 'gud-print
    "ds" 'gud-step
    "dw" 'gud-watch
    "ee" 'workflow-alternative-buffer
    "eg" 'goal/switch-to
    "es" 'workflow-eshell-open-from-here
    ;; "et" 'go/swap-test-and-subject
    ;; "ei" 'go/swap-impl-and-subject
    "ew" 'workflow-dot-open-file
    "er" 'go-show-cover-report
    "eb" 'eval-buffer
    "ev" 'eval-expression
    "fb" 'workflow-switch-to-buffer
    "ff" 'workflow-buffer-or-recentf-open
    "fs" 'workflow-format-current-source
    "gc" 'workflow-git-commit-changes
    "gf" 'workflow-git-open-file
    "gg" 'workflow-git-popup
    "hl" 'hs-hide-level
    "hh" 'hs-toggle-hiding
    "hs" 'workflow-highlight-symbol
    "kb" 'kill-buffer
    "ma" 'smerge-keep-all
    "mc" 'smerge-keep-current
    "mm" 'smerge-keep-mine
    "mo" 'smerge-keep-other
    "ni" 'evil-numbers/inc-at-pt
    "nd" 'evil-numbers/dec-at-pt
    "nb" 'workflow-new-buffer
    "sa" 'workflow-save-all-buffers
    "sg" 'workflow-search-git
    "ss" 'workflow-search-any-text
    "sr" 'projectile-ripgrep
    "ra" 'projectile-run-async-shell-command-in-root
    "rn" 'eglot-rename
    "rs" 'workflow-run-scratch
    "rr" 'workflow-test-func
    "rt" 'workflow-test-file
    "wh" 'workflow-horizontal-split-window
    "wo" 'other-window
    "wv" 'workflow-vertical-split-window
    "ww" 'workflow-delete-other-windows
    "wx" 'workflow-close-current-window
    "xw" 'workflow-M-x
    "xx" 'counsel-M-x
    "," 'other-window
    "<SPC>" 'other-window))

(when (require 'evil-collection nil t)
  (evil-collection-init))

(provide 'init-edit)
