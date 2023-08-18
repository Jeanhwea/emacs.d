;; -----------------------------------------------------------------------------
;; Most common used commands first
;; -----------------------------------------------------------------------------
(global-set-key (kbd "M-w") 'workflow-M-x)
;; (global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-i") 'workflow-inflect-string)
;; (global-set-key (kbd "M-r") 'workflow-buffer-or-recentf-open)
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-j") 'workflow-emmet-expand)

(global-set-key (kbd "<f5>") 'workflow-run-scratch)
(global-set-key (kbd "<f7>") 'gud-finish)
(global-set-key (kbd "<f8>") 'gud-step)
(global-set-key (kbd "<f9>") 'gud-next)
(global-set-key (kbd "<f10>") 'gud-break)
(global-set-key (kbd "<f12>") 'gud-cont)

;; -----------------------------------------------------------------------------
;; evil keybinding
;; -----------------------------------------------------------------------------
(when (and (require 'evil-leader) (require 'evil-numbers))
  (global-evil-leader-mode)

  (evil-define-key '(normal visual) 'global (kbd "<SPC>") 'evil-scroll-down)
  (evil-define-key '(normal visual) 'global (kbd "gp") 'xref-pop-marker-stack)
  (evil-define-key '(normal visual) 'global (kbd "gf") 'evil-find-file-at-point-with-line)
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
    "bv" 'workflow-git-browse-remote
    "cw" 'workflow-replace
    "cd" 'workflow-cd-workdir
    "cg" 'workflow-cd-projdir
    "db" 'gud-break
    "dc" 'gud-cont
    "dd" 'gud-down
    "df" 'gud-finish
    "dn" 'gud-next
    "dp" 'gud-print
    "dr" 'gud-run
    "ds" 'gud-step
    "dt" 'gud-until
    "du" 'gud-up
    "dw" 'gud-watch
    "dx" 'gud-remove
    "ee" 'workflow-alternative-buffer
    "eg" 'goal/switch-to
    "es" 'workflow-eshell-open-from-here
    "ev" 'projectile-run-vterm
    "ez" 'projectile-run-term
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
    "ii" 'workflow-inflect-string
    "kb" 'kill-buffer
    "ma" 'smerge-keep-all
    "mc" 'smerge-keep-current
    "mm" 'smerge-keep-mine
    "mo" 'smerge-keep-other
    "mp" 'smerge-prev
    "mn" 'smerge-next
    "nc" 'create-new-component
    "ni" 'evil-numbers/inc-at-pt
    "nd" 'evil-numbers/dec-at-pt
    "nb" 'workflow-new-buffer
    "oa" 'org-agenda
    "oc" 'jh/org-capture-task
    "od" 'org-deadline
    "ot" 'org-schedule
    "oe" 'org-edit-special
    "oi" 'org-insert-link
    "oo" 'org-open-at-point
    "os" 'org-store-link
    "ox" 'org-babel-execute-src-block
    "sa" 'workflow-save-all-buffers
    "sg" 'workflow-search-git
    "ss" 'workflow-search-any-text
    "sr" 'projectile-ripgrep
    "ra" 'projectile-run-async-shell-command-in-root
    "rs" 'workflow-run-scratch
    "rt" 'workflow-test-func
    "rn" 'eglot-rename
    "rr" 'emamux:run-command
    "wh" 'workflow-horizontal-split-window
    "wo" 'other-window
    "wv" 'workflow-vertical-split-window
    "ww" 'workflow-delete-other-windows
    "wx" 'workflow-close-current-window
    "xw" 'workflow-M-x
    "xx" 'counsel-M-x
    "," 'other-window
    "<SPC>" 'other-window))

;; -----------------------------------------------------------------------------
;;  _   _               _   __  __           _
;; | | | | __ _ _ __ __| | |  \/  | ___   __| | ___
;; | |_| |/ _` | '__/ _` | | |\/| |/ _ \ / _` |/ _ \
;; |  _  | (_| | | | (_| | | |  | | (_) | (_| |  __/
;; |_| |_|\__,_|_|  \__,_| |_|  |_|\___/ \__,_|\___|
;; -----------------------------------------------------------------------------
;; disable keys
(global-set-key (kbd "C-x 1") nil)
(global-set-key (kbd "C-x 2") nil)
(global-set-key (kbd "C-x 3") nil)
(global-set-key (kbd "C-x 0") nil)
(global-set-key (kbd "C-x k") nil)
(global-set-key (kbd "C-x C-f") nil)
(global-set-key (kbd "C-x C-s") nil)
(global-set-key (kbd "C-x C-c") nil)
(global-set-key (kbd "C-x b") nil)
(global-set-key (kbd "C-x g") nil)

(provide 'init-keymate)
