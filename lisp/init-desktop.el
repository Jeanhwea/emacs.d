;; -----------------------------------------------------------------------------
;; session, desktop mode
;; -----------------------------------------------------------------------------
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

;; -----------------------------------------------------------------------------
;; backup, autosave file, place and cleanup whitespace
;; -----------------------------------------------------------------------------
(setq-default
  create-lockfiles nil
  auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t))
  backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
(save-place-mode 1)
(auto-save-visited-mode 1)
;; auto delete trailing whitespace before saving
(add-hook 'before-save-hook 'whitespace-cleanup)


;; -----------------------------------------------------------------------------
;; navigation helper
;; -----------------------------------------------------------------------------
(when (require 'fancy-narrow)
  (fancy-narrow-mode 1))

(when (require 'dashboard)
  (dashboard-setup-startup-hook)
  (setq
    show-week-agenda-p t
    dashboard-banner-logo-title (format "%s@%s: Keep thinking while not in coding!"
                                  (user-login-name) (system-name))
    dashboard-startup-banner (concat user-emacs-directory "dashboard-banner.png")
    dashboard-items '((bookmarks . 5)
                       (recents  . 10)
                       (agenda . 5)
                       (projects . 5)))
  (defun jh/switch-to-dashboard ()
    "Load dashboard at this frame."
    (interactive)
    (switch-to-buffer "*dashboard*")
    (dashboard-refresh-buffer))
  (global-set-key (kbd "C-c d") 'jh/switch-to-dashboard))

(when (require 'ace-jump-mode)
  (autoload
    'ace-jump-mode
    "ace-jump-mode"
    "Emacs quick move minor mode"
    t)
  ;; you can select the key you prefer to
  (define-key global-map (kbd "C-'") 'ace-jump-mode))


;; -----------------------------------------------------------------------------
;; exec-path-from-shell, read the $PATH
;; -----------------------------------------------------------------------------
(when (require 'exec-path-from-shell)
  (unless (jh/windows?)
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

;; -----------------------------------------------------------------------------
;; undo-tree
;; undo-tree on elpa is very old and hard to use, try this mirror
;; git clone git@github.com:Jeanhwea/undo-tree.git
;; -----------------------------------------------------------------------------
(when (require 'undo-tree)
  ;; Persistent undo history
  (setq
    undo-tree-enable-undo-in-region nil
    undo-tree-auto-save-history t
    undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo" user-emacs-directory))))
  ;; Compressing undo history
  (defadvice undo-tree-make-history-save-file-name
    (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz")))

  (defun jh/undofile-expired-p (filename)
    "Return ture if the undo file is expired."
    (let
      ((expired-seconds (* 100 60 60 24)) ; file expired time limits to 100 days
        (last-modification-time
          (file-attribute-modification-time
            (file-attributes
              (expand-file-name filename)))))
      (time-less-p (time-add last-modification-time expired-seconds)
        (current-time))))

  (defun jh/undofile-size-exceed-p (filename)
    "Return ture if the undo file exceeds maximum size limits."
    (let ((max-size-limit (* 5 1024))   ; file size limits to 8k
           (file-size (file-attribute-size (file-attributes filename))))
      (> file-size max-size-limit)))

  ;; Delete big file to avoid C stack overflow
  (defun jh/delete-unused-undofiles ()
    (let ((undodir (expand-file-name "undo" user-emacs-directory)))
      (dolist (undofile (directory-files undodir t "gz$"))
        (when (or (jh/undofile-size-exceed-p undofile) (jh/undofile-expired-p undofile))
          (delete-file undofile)))))
  (jh/delete-unused-undofiles)
  (global-undo-tree-mode))


;; -----------------------------------------------------------------------------
;; manually install howdoi
;; 1. pip install howdoi
;; 2. git clone git@github.com:Jeanhwea/howdoi-emacs.git ~/.emacs.d/site-lisp/howdoi
;; -----------------------------------------------------------------------------
(when (file-directory-p (expand-file-name "site-lisp/howdoi" user-emacs-directory))
  (require 'howdoi))

;; -----------------------------------------------------------------------------
;; font
;; -----------------------------------------------------------------------------
;; (when (jh/windows?)
;;   (set-face-attribute 'default nil :font "WenQuanYi Micro Hei Mono 11"))

;; (when (jh/windows?)
;;   (set-face-attribute 'default nil :font "Consolas 11")
;;   ;; 中文字体单独设置
;;   (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;     (set-fontset-font (frame-parameter nil 'font)
;;       charset
;;       (font-spec :family "WenQuanYi Micro Hei Mono" :size 22))))

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

(defun jh/rename-this-buffer-and-file (name)
  "Rename both current buffer and file it's visiting to name."
  (interactive "sNew name: ")
  (let ((buffername (buffer-name))
         (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" buffername))
    (progn
      (when (file-exists-p filename)
        (rename-file filename name 1))
      (set-visited-file-name name)
      (rename-buffer name))))

;; -----------------------------------------------------------------------------
;; shrimp shell
;; -----------------------------------------------------------------------------
(defun jh/shrimp-project-name ()
  "Return the project name."
  (let* ((project-root (jh/git-project-root-dir default-directory))
          (root (and project-root (directory-file-name project-root))))
    (and root
      (replace-regexp-in-string
        (regexp-quote (jh/parent-dir root)) "" root nil 'literal))))

(defun jh/shrimp-shell-name ()
  "Return the shell name."
  (let ((name (jh/shrimp-project-name)))
    (if name (format "*shrimp[%s]*" name) "*shrimp*")))

(defun jh/shrimp-open ()
  "open a eshell as a temporary shell, and rename the buffer to `*shrimp*'."
  (interactive)
  (let ((name (jh/shrimp-shell-name)))
    (if (get-buffer name)
      (switch-to-buffer name)
      (let ((eshell-buffer-name name)) (eshell)))))
(global-set-key (kbd "C-c s") 'jh/shrimp-open)

;; -----------------------------------------------------------------------------
;; theme
;; -----------------------------------------------------------------------------
(when
  (and
    (require 'color-theme-sanityinc-solarized nil t)
    (require 'color-theme-sanityinc-tomorrow nil t))
  (defvar jh/themes
    (list
      'sanityinc-solarized-dark
      'sanityinc-solarized-light
      'sanityinc-tomorrow-bright
      'sanityinc-tomorrow-day
      'sanityinc-tomorrow-night
      'sanityinc-tomorrow-blue
      'sanityinc-tomorrow-eighties)
    "My favirate color themes list")
  (defun jh/cycle-color-theme ()
    "Cycle through all prefered color theme."
    (interactive)
    (let*
      ((current-theme (car custom-enabled-themes))
        (next-theme (cadr (member current-theme jh/themes))))
      (if next-theme
        (load-theme next-theme t) (load-theme (car jh/themes) t))))
  ;; default theme
  (load-theme 'sanityinc-solarized-light t))

;; -----------------------------------------------------------------------------
;; transparency
;; -----------------------------------------------------------------------------
(defun jh/toggle-transparency ()
  "Toggle frame transparency."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
      nil 'alpha
      (if (eql (cond
                 ((numberp alpha) alpha)
                 ((numberp (cdr alpha)) (cdr alpha))
                 ((numberp (cadr alpha)) (cadr alpha)))
            100)
        '(85 . 50) '(100 . 100)))))

(provide 'init-desktop)
