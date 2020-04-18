;; -----------------------------------------------------------------------------
;;  _____        _  __
;; |  ___| __   | |/ /___ _   _
;; | |_ | '_ \  | ' // _ \ | | |
;; |  _|| | | | | . \  __/ |_| |
;; |_|  |_| |_| |_|\_\___|\__, |
;;                        |___/
;; -----------------------------------------------------------------------------
(global-set-key (kbd "M-;") 'repeat)

;; -----------------------------------------------------------------------------
;; Most common used commands
;; -----------------------------------------------------------------------------
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-s") 'save-buffer)

;; -----------------------------------------------------------------------------
;; F2: Edit
;; -----------------------------------------------------------------------------
(progn
  (define-prefix-command 'leader/f2)
  ;; Narrow & Widen
  (define-key leader/f2 (kbd "n") #'narrow-to-region)
  (define-key leader/f2 (kbd "w") #'widen)
  ;; hide and show
  (define-key leader/f2 (kbd "h") #'hs-toggle-hiding)
  (define-key leader/f2 (kbd "l") #'workflow-cycle-hide-level)
  (define-key leader/f2 (kbd "0") #'hs-hide-all)
  (define-key leader/f2 (kbd "9") #'hs-show-all)
  ;; Undotree
  (define-key leader/f2 (kbd "u") #'undo-tree-visualize)
  ;; Search global
  (define-key leader/f2 (kbd "a") #'ag)
  (define-key leader/f2 (kbd "s") #'counsel-ag)
  (define-key leader/f2 (kbd "g") #'counsel-git-grep)
  ;; Replace, regular eXpression replace
  (define-key leader/f2 (kbd "r") #'workflow-replace)
  (define-key leader/f2 (kbd "M-r") #'projectile-replace)
  (define-key leader/f2 (kbd "x") #'query-replace-regexp)
  (define-key leader/f2 (kbd "M-x") #'projectile-replace-regexp)
  ;; Line-wise Editting
  (define-key leader/f2 (kbd "j") #'jh/joinline)
  (define-key leader/f2 (kbd "n") #'jh/shiftdown-line)
  (define-key leader/f2 (kbd "p") #'jh/shiftup-line)
  ;; Multiple line
  (define-key leader/f2 (kbd "|") #'mc/edit-lines)
  (define-key leader/f2 (kbd "=") #'mc/mark-all-like-this)
  (define-key leader/f2 (kbd "<left>") #'mc/mark-previous-like-this)
  (define-key leader/f2 (kbd "<right>") #'mc/mark-next-like-this)
  ;; evil mode
  (define-key leader/f2 (kbd "v") #'evil-mode))
(global-set-key (kbd "<f2>") 'leader/f2)
(global-set-key (kbd "M-2") 'leader/f2)

;; -----------------------------------------------------------------------------
;; F9: Misc
;; -----------------------------------------------------------------------------
(progn
  (define-prefix-command 'leader/f9)
  ;; Color theme
  (define-key leader/f9 (kbd "t") #'jh/cycle-color-theme)
  (define-key leader/f9 (kbd "M-t") #'jh/cycle-transparency)
  ;;
  (define-key leader/f9 (kbd "v") #'verb-send-request-on-point-other-window-stay)
  (define-key leader/f9 (kbd "c") #'ct/expand-command)
  (define-key leader/f9 (kbd "d") #'workflow-drop-file)
  (define-key leader/f9 (kbd "r") #'workflow-reveal-in-file-manager)
  (define-key leader/f9 (kbd "s") #'workflow-send-to-shell))
(global-set-key (kbd "<f9>") 'leader/f9)
(global-set-key (kbd "M-9") 'leader/f9)

;; -----------------------------------------------------------------------------
;; F0: Version control & Git
;; -----------------------------------------------------------------------------
(progn
  (define-prefix-command 'leader/f0)
  ;; git
  (define-key leader/f0 (kbd "v") #'magit)
  (define-key leader/f0 (kbd "s") #'magit-status)
  (define-key leader/f0 (kbd "b") #'magit-blame)
  (define-key leader/f0 (kbd "c") #'magit-commit)
  (define-key leader/f0 (kbd "l") #'magit-log)
  (define-key leader/f0 (kbd "M-l") #'magit-log-all)
  (define-key leader/f0 (kbd "@") #'magit-log-buffer-file)
  (define-key leader/f0 (kbd "p") #'magit-push)
  (define-key leader/f0 (kbd "F") #'magit-fetch)
  (define-key leader/f0 (kbd "f") #'magit-pull)
  (define-key leader/f0 (kbd "m") #'magit-merge)
  ;; smerge
  (define-key leader/f0 (kbd "<left>") #'smerge-prev)
  (define-key leader/f0 (kbd "<right>") #'smerge-next)
  (define-key leader/f0 (kbd "<return>") #'smerge-keep-current)
  (define-key leader/f0 (kbd "1") #'smerge-keep-mine)
  (define-key leader/f0 (kbd "2") #'smerge-keep-other)
  (define-key leader/f0 (kbd "3") #'smerge-keep-all)
  ;; open remote url
  (define-key leader/f0 (kbd "o") #'browse-at-remote))
(global-set-key (kbd "<f10>") 'leader/f0)
(global-set-key (kbd "M-0") 'leader/f0)

;; -----------------------------------------------------------------------------
;; F11: UI operations
;; -----------------------------------------------------------------------------
(progn
  ;; Leader Key
  (define-prefix-command 'leader/f11)
  ;; Window
  (define-key leader/f11 (kbd "u") #'workflow-unique-window)
  (define-key leader/f11 (kbd "x") #'workflow-close-current-window)
  (define-key leader/f11 (kbd "h") #'workflow-horizontal-split-window)
  (define-key leader/f11 (kbd "v") #'workflow-vertically-split-window)
  (define-key leader/f11 (kbd "q") #'workflow-quit-emacs-application)
  (define-key leader/f11 (kbd "e") #'workflow-expand-fullscreen)
  ;; File access
  (define-key leader/f11 (kbd "<SPC>") #'workflow-swith-to-buffer)
  (define-key leader/f11 (kbd "r") #'workflow-recentf-open-file)
  (define-key leader/f11 (kbd "f") #'workflow-fuzzy-open-file)
  (define-key leader/f11 (kbd "g") #'workflow-git-open-file)
  (define-key leader/f11 (kbd ".") #'workflow-currdir-open-file)
  (define-key leader/f11 (kbd "b") #'workflow-bookmark-open-file)
  (define-key leader/f11 (kbd "B") #'workflow-bookmark-current-file)
  (define-key leader/f11 (kbd "a") #'workflow-alternative-buffer)
  ;; Highlight symbol & Color theme
  (define-key leader/f11 (kbd ";") #'workflow-highlight-symbol)
  (define-key leader/f11 (kbd "c") #'workflow-cycle-color-theme)
  ;; bu
  ;; Frame
  ;; (define-key leader/f11 (kbd "5") #'make-frame-command)
  ;; (define-key leader/f11 (kbd "6") #'delete-frame)
  ;; File create & save
  ;; (define-key leader/f11 (kbd "l") #'toggle-read-only)
  (define-key leader/f11 (kbd "k") #'kill-buffer)
  (define-key leader/f11 (kbd "s") #'workflow-save-buffers)
  (define-key leader/f11 (kbd "d") #'workflow-delete-file)
  ;; Ace jump
  (define-key leader/f11 (kbd "[") #'ace-jump-mode))
(global-set-key (kbd "<f11>") 'leader/f11)
(global-set-key (kbd "M-[") 'leader/f11)

;; -----------------------------------------------------------------------------
;; F12: Project Management
;; -----------------------------------------------------------------------------
(progn
  ;; Leader Key
  (define-prefix-command 'leader/f12)
  ;; misc
  (define-key leader/f12 (kbd "n") #'workflow-new-buffer)
  ;; Unit test
  (define-key leader/f12 (kbd "u") 'spt/run-test-method-command)
  (define-key leader/f12 (kbd "M-u") 'spt/run-test-class-command)
  ;; Project starter
  (define-key leader/f12 (kbd "c") #'projectile-compile-project)
  (define-key leader/f12 (kbd "r") #'projectile-run-project)
  (define-key leader/f12 (kbd "s") #'workflow-shrimp-open)
  (define-key leader/f12 (kbd "t") #'workflow-term-cd)
  ;; Prettify source code
  (define-key leader/f12 (kbd "f") 'workflow-format-code)
  (define-key leader/f12 (kbd "i") 'spt/import-unknown-class))
(global-set-key (kbd "<f12>") 'leader/f12)
(global-set-key (kbd "M-]") 'leader/f12)

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
