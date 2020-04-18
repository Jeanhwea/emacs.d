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
;; F9: Misc
;; -----------------------------------------------------------------------------
(progn
  (define-prefix-command 'leader/f9)
  (define-key leader/f9 (kbd "v") #'verb-send-request-on-point-other-window-stay)
  (define-key leader/f9 (kbd "c") #'ct/expand-command)
  (define-key leader/f9 (kbd "d") #'workflow-drop-file)
  (define-key leader/f9 (kbd "r") #'workflow-open-default-folder)
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
;; F11: UI & Basic Operations
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
  (define-key leader/f11 (kbd "o") #'workflow-open-default-folder)
  (define-key leader/f11 (kbd "O") #'workflow-open-this-file)
  (define-key leader/f11 (kbd "r") #'workflow-recentf-open-file)
  (define-key leader/f11 (kbd "f") #'workflow-fuzzy-open-file)
  (define-key leader/f11 (kbd "g") #'workflow-git-open-file)
  (define-key leader/f11 (kbd ".") #'workflow-dot-open-file)
  (define-key leader/f11 (kbd "b") #'workflow-bookmark-open-file)
  (define-key leader/f11 (kbd "B") #'workflow-bookmark-current-file)
  ;; Buffer operation
  (define-key leader/f11 (kbd "<SPC>") #'workflow-switch-to-buffer)
  (define-key leader/f11 (kbd "a") #'workflow-alternative-buffer)
  (define-key leader/f11 (kbd "k") #'workflow-kill-buffer)
  (define-key leader/f11 (kbd "n") #'workflow-new-buffer)
  (define-key leader/f11 (kbd "s") #'workflow-save-all-buffers) ;; M-s => save current buffer
  (define-key leader/f11 (kbd "R") #'workflow-rename-current-file)
  (define-key leader/f11 (kbd "d") #'workflow-delete-current-file)
  ;; Hide & Show
  (define-key leader/f11 (kbd "<tab>") #'workflow-hideshow-toggle)
  (define-key leader/f11 (kbd "=") #'workflow-cycle-hide-level)
  (define-key leader/f11 (kbd "<") #'workflow-hide-all-level)
  (define-key leader/f11 (kbd ">") #'workflow-show-all-level)
  ;; Highlight symbol & Colortheme, Transparency
  (define-key leader/f11 (kbd ";") #'workflow-highlight-symbol)
  (define-key leader/f11 (kbd "w") #'workflow-wipeout-all-highlights)
  (define-key leader/f11 (kbd "c") #'workflow-colortheme-cycling)
  (define-key leader/f11 (kbd "t") #'workflow-transparency-cycling)
  ;; Undotree
  ;; (define-key leader/f11 (kbd "i") #'undo-tree-visualize)
  ;; Frame
  ;; (define-key leader/f11 (kbd "5") #'make-frame-command)
  ;; (define-key leader/f11 (kbd "6") #'delete-frame)
  ;; File create & save
  ;; (define-key leader/f11 (kbd "l") #'toggle-read-only)
  ;; Ace jump
  (define-key leader/f11 (kbd "[") #'ace-jump-mode))
(global-set-key (kbd "<f11>") 'leader/f11)
(global-set-key (kbd "M-[") 'leader/f11)

;; -----------------------------------------------------------------------------
;; F12: Editing & Source Code Editing
;; -----------------------------------------------------------------------------
(progn
  ;; Leader Key
  (define-prefix-command 'leader/f12)
  ;; Search any thing
  (define-key leader/f12 (kbd "a") #'workflow-ag-search)
  (define-key leader/f12 (kbd "s") #'workflow-search-any-text)
  (define-key leader/f12 (kbd "g") #'workflow-git-search)
  ;; Replace, regular eXpression replace
  (define-key leader/f12 (kbd "r") #'workflow-replace)
  (define-key leader/f12 (kbd "R") #'workflow-replace-projectile)
  (define-key leader/f12 (kbd "x") #'workflow-regexp-replace)
  (define-key leader/f12 (kbd "X") #'workflow-regexp-replace-projectile)
  ;; Source code related
  (define-key leader/f12 (kbd "f") 'workflow-format-current-source)
  (define-key leader/f12 (kbd "/") #'workflow-comment-source-code)
  ;; Unit test & Project starter
  ;; (define-key leader/f12 (kbd "u") 'spt/run-test-method-command)
  ;; (define-key leader/f12 (kbd "M-u") 'spt/run-test-class-command)
  ;; (define-key leader/f12 (kbd "c") #'projectile-compile-project)
  ;; (define-key leader/f12 (kbd "r") #'projectile-run-project)
  ;; Misc
  (define-key leader/f12 (kbd "c") #'workflow-codetta-expand-command)
  (define-key leader/f12 (kbd "e") #'workflow-eshell-open-from-here)
  (define-key leader/f12 (kbd "w") #'workflow-working-directory-send))
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
