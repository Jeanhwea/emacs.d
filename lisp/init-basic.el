;; -----------------------------------------------------------------------------
;; custom system encoding, locale...
;; -----------------------------------------------------------------------------
(prefer-coding-system 'utf-8)
(setq-default
  default-buffer-file-coding-system 'utf-8
  system-time-locale "C")


;; -----------------------------------------------------------------------------
;; setup system keyboard
;; -----------------------------------------------------------------------------
(setq-default
  mac-command-modifier 'meta
  mac-option-modifier 'super
  w32-pass-lwindow-to-system nil
  w32-lwindow-modifier 'super
  w32-pass-rwindow-to-system nil
  w32-rwindow-modifier 'super)


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
;; undo-tree
;; -----------------------------------------------------------------------------
;;(when (require 'undo-tree)
;;  (global-undo-tree-mode))


;; -----------------------------------------------------------------------------
;; session
;; -----------------------------------------------------------------------------
(save-place-mode 1)
(when (require 'desktop)
  (setq desktop-buffers-not-to-save
    (concat "\\("
      "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
      "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
      "\\)$"))
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'magit-mode))
;; (desktop-save-mode 1)

;; parenthesis behavior
(show-paren-mode 1)
(electric-pair-mode 1)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'downcase-region 'disabled nil) ; C-x C-u
(put 'upcase-region 'disabled nil)   ; C-x C-l


;; -----------------------------------------------------------------------------
;; backup and auto-save
;; -----------------------------------------------------------------------------
(setq-default
  create-lockfiles nil
  auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t))
  backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
(auto-save-visited-mode 1)


;; -----------------------------------------------------------------------------
;; exec-path-from-shell
;; -----------------------------------------------------------------------------
(when (require 'exec-path-from-shell)
  (when (jh/mac?)
    (exec-path-from-shell-initialize)))


;; -----------------------------------------------------------------------------
;; recentf
;; -----------------------------------------------------------------------------
(setq-default
  recentf-max-saved-items 1000
  recentf-exclude
    '("/.emacs.d/elfeed/*"
      "/.emacs.d/elpa/*"
      "/.emacs.d/ido.last"
      "/.emacs.d/bookmarks"
      "/agenda/"
      "/tmp/"
      "/ssh:"))
(recentf-mode 1)


(setq-default ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)


;; -----------------------------------------------------------------------------
;; when compact large fonts cause lots of resources, the editor will be very slow
;; so just inhibit compacting, when using chinese font
;; -----------------------------------------------------------------------------
(when (jh/windows?)
  (setq inhibit-compacting-font-caches t))


;; -----------------------------------------------------------------------------
;; tab, space width configuration
;; -----------------------------------------------------------------------------
(delete-selection-mode 1)
(setq-default
  indent-tabs-mode nil
  tab-width 4)

;; auto delete trailing whitespace before saving
(add-hook 'before-save-hook 'whitespace-cleanup)

;; -----------------------------------------------------------------------------
;; gui
;; -----------------------------------------------------------------------------
(setq-default
  inhibit-startup-message t
  fill-column 80
  blink-cursor-mode t
  ;; cursor-type 'bar
  blink-cursor-interval 1
  ;; ediff-split-window-function 'split-window-horizontally
  ;; ediff-window-setup-function 'ediff-setup-windows-plain
  display-time-format "%Y-%m-%d %H:%M"
  line-number-mode t
  column-number-mode t
  size-indication-mode t)

;;(when (require 'nyan-mode) (nyan-mode 1))

(if (jh/windows?)
  (menu-bar-mode -1)
  (menu-bar-mode 1))
(scroll-bar-mode -1)
(tool-bar-mode -1)
(display-time-mode 1)

;;(global-hl-line-mode 1) ; highlight the current line
;;(when (require 'fill-column-indicator)
;;  (add-hook 'after-change-major-mode-hook 'fci-mode))

(global-prettify-symbols-mode 1)

;; -----------------------------------------------------------------------------
;; font
;; -----------------------------------------------------------------------------
(when (jh/windows?)
  (set-face-attribute 'default nil :font "Consolas 12")
  ;; 中文字体单独设置
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
      charset
      (font-spec :family "WenQuanYi Micro Hei Mono" :size 26))))

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
;; theme
;; -----------------------------------------------------------------------------
(when (require 'color-theme-sanityinc-solarized nil t)
  ;; default use solarized dark theme
  (add-hook 'after-init-hook
    (lambda () (load-theme 'sanityinc-solarized-light t)))
  ;; Add helper command to make changing color theme more faster
  (defun jh/load-light-theme()
    "Activate a light color theme"
    (interactive)
    (load-theme 'sanityinc-solarized-light t))
  (defun jh/load-dark-theme()
    "Activate a dark color theme"
    (interactive)
    (load-theme 'sanityinc-solarized-dark t))
  (defun jh/toggle-light-dark-theme ()
    "Toggle solarized light/dark theme"
    (interactive)
    (let ((current-theme (car custom-enabled-themes)))
      (when (string-equal current-theme "sanityinc-solarized-dark")
        (load-theme 'sanityinc-solarized-light))
      (when (string-equal current-theme "sanityinc-solarized-light")
        (load-theme 'sanityinc-solarized-dark))))
  (global-set-key (kbd "<f12>") 'jh/toggle-light-dark-theme))

(provide 'init-basic)
