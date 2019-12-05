;; -----------------------------------------------------------------------------
;;  _____        _  __
;; |  ___| __   | |/ /___ _   _
;; | |_ | '_ \  | ' // _ \ | | |
;; |  _|| | | | | . \  __/ |_| |
;; |_|  |_| |_| |_|\_\___|\__, |
;;                        |___/
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; F1: File, Window, Frame
;; -----------------------------------------------------------------------------
(progn
  (define-prefix-command 'leader/f1)
  ;; Window
  (define-key leader/f1 (kbd "0") #'delete-window)
  (define-key leader/f1 (kbd "1") #'delete-other-windows)
  (define-key leader/f1 (kbd "2") #'split-window-below)
  (define-key leader/f1 (kbd "3") #'split-window-right)
  (define-key leader/f1 (kbd "f") #'toggle-frame-fullscreen)
  (define-key leader/f1 (kbd "F") #'toggle-frame-maximized)
  ;; Frame
  (define-key leader/f1 (kbd "5") #'make-frame-command)
  (define-key leader/f1 (kbd "6") #'delete-frame)
  ;; File create & access
  (define-key leader/f1 (kbd "n") #'jh/new-scratch-buffer)
  (define-key leader/f1 (kbd "R") #'toggle-read-only)
  (define-key leader/f1 (kbd "g") #'counsel-git)
  (define-key leader/f1 (kbd "d") #'counsel-find-file)
  (define-key leader/f1 (kbd "p") #'projectile-find-file)
  (define-key leader/f1 (kbd "b") #'counsel-bookmark)
  (define-key leader/f1 (kbd "r") #'counsel-recentf)
  (define-key leader/f1 (kbd "m") #'workflow-bookmark-current-file)
  ;; Project management
  (define-key leader/f1 (kbd "o") #'projectile-switch-project)
  (define-key leader/f1 (kbd "c") #'projectile-compile-project)
  (define-key leader/f1 (kbd "u") #'projectile-run-project))
(global-set-key (kbd "<f1>") 'leader/f1)
(global-set-key (kbd "M-o") 'other-window)


;; -----------------------------------------------------------------------------
;; F2: Edit
;; -----------------------------------------------------------------------------
(progn
  (define-prefix-command 'leader/f2)
  ;; Narrow & Widen
  (define-key leader/f2 (kbd "n") #'narrow-to-region)
  (define-key leader/f2 (kbd "w") #'widen)
  ;; Undotree
  (define-key leader/f2 (kbd "u") #'undo-tree-visualize)
  ;; Search global
  (define-key leader/f2 (kbd "a") #'counsel-ag)
  (define-key leader/f2 (kbd "g") #'counsel-git-grep)
  (define-key leader/f2 (kbd "s") #'ag)
  ;; Replace, regular eXpression replace
  (define-key leader/f2 (kbd "r") #'workflow-replace)
  (define-key leader/f2 (kbd "R") #'projectile-replace)
  (define-key leader/f2 (kbd "x") #'query-replace-regexp)
  (define-key leader/f2 (kbd "X") #'projectile-replace-regexp)
  ;; Multiple line
  (define-key leader/f2 (kbd "|") #'mc/edit-lines)
  (define-key leader/f2 (kbd "=") #'mc/mark-all-like-this)
  (define-key leader/f2 (kbd "<left>") #'mc/mark-previous-like-this)
  (define-key leader/f2 (kbd "<right>") #'mc/mark-next-like-this)
  ;; evil mode
  (define-key leader/f2 (kbd "v") #'evil-mode))
(global-set-key (kbd "<f2>") 'leader/f2)


;; -----------------------------------------------------------------------------
;; F11: Misc
;; -----------------------------------------------------------------------------
(progn
  (define-prefix-command 'leader/f11)
  ;; Color theme
  (define-key leader/f11 (kbd "t") #'jh/cycle-color-theme)
  (define-key leader/f11 (kbd "T") #'jh/toggle-transparency)
  ;;
  (define-key leader/f11 (kbd "c") 'ct/expand-command)
  (define-key leader/f11 (kbd "d") 'workflow-drop-file)
  (define-key leader/f11 (kbd "r") 'workflow-reveal-in-file-manager)
  (define-key leader/f11 (kbd "s") 'workflow-send-to-shell))
(global-set-key (kbd "<f11>") 'leader/f11)


;; -----------------------------------------------------------------------------
;; F12: Version control & Git
;; -----------------------------------------------------------------------------
(progn
  (define-prefix-command 'leader/f12)
  ;; git
  (define-key leader/f12 (kbd "v") #'magit)
  (define-key leader/f12 (kbd "s") #'magit-status)
  (define-key leader/f12 (kbd "b") #'magit-blame)
  (define-key leader/f12 (kbd "c") #'magit-commit)
  (define-key leader/f12 (kbd "l") #'magit-log)
  (define-key leader/f12 (kbd "L") #'magit-log-all)
  (define-key leader/f12 (kbd "p") #'magit-push)
  (define-key leader/f12 (kbd "f") #'magit-fetch)
  ;; smerge
  (define-key leader/f12 (kbd "<left>") #'smerge-prev)
  (define-key leader/f12 (kbd "<right>") #'smerge-next)
  (define-key leader/f12 (kbd "<ret>") #'smerge-keep-current)
  (define-key leader/f12 (kbd "1") #'smerge-keep-mine)
  (define-key leader/f12 (kbd "2") #'smerge-keep-other)
  (define-key leader/f12 (kbd "3") #'smerge-keep-all)
  ;; open remote url
  (define-key leader/f12 (kbd "o") #'browse-at-remote))
(global-set-key (kbd "<f12>") 'leader/f12)


;; -----------------------------------------------------------------------------
;;  __  __      _              __
;; |  \/  | ___| |_ __ _      | _|
;; | |\/| |/ _ \ __/ _` |_____| |
;; | |  | |  __/ || (_| |_____| |
;; |_|  |_|\___|\__\__,_|     | |
;;                            |__|
;; -----------------------------------------------------------------------------
(progn
  ;; Leader Key
  (define-prefix-command 'leader/meta-lb)

  ;; Switcher Keybinding for Springboot
  (define-key leader/meta-lb (kbd "e") #'(lambda () (interactive) (spt/switch-to 'entity)))
  (define-key leader/meta-lb (kbd "r") #'(lambda () (interactive) (spt/switch-to 'repo)))
  (define-key leader/meta-lb (kbd "s") #'(lambda () (interactive) (spt/switch-to 'service)))
  (define-key leader/meta-lb (kbd "i") #'(lambda () (interactive) (spt/switch-to 'impl)))
  (define-key leader/meta-lb (kbd "c") #'(lambda () (interactive) (spt/switch-to 'controller)))
  (define-key leader/meta-lb (kbd "h") #'(lambda () (interactive) (spt/switch-to 'helper)))
  (define-key leader/meta-lb (kbd "t") #'spt/swap-test-and-source)
  (define-key leader/meta-lb (kbd "d") #'spt/swap-markdown-and-endpoint)

  ;; Swither for Angular
  (define-key leader/meta-lb (kbd "a") #'ng/cycle-source-files)

  ;; Open sources
  (define-key leader/meta-lb (kbd "p") #'workflow-open-class)

  ;; highlight symbol
  (define-key leader/meta-lb (kbd "1") #'workflow-highlight-symbol)
  (define-key leader/meta-lb (kbd "0") #'workflow-unhighlight-all)

  ;; Unit test
  (define-key leader/meta-lb (kbd "u") 'spt/run-test-method-command)
  (define-key leader/meta-lb (kbd "U") 'spt/run-test-class-command)

  ;; workflow
  (define-key leader/meta-lb (kbd "f") 'workflow-format-code)
  (define-key leader/meta-lb (kbd "j") 'workflow-join-line)
  (define-key leader/meta-lb (kbd "RET") 'spt/import-unknown-class))
(global-set-key (kbd "M-[") 'leader/meta-lb)

(provide 'init-keymate)
