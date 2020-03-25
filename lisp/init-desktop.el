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
    dashboard-banner-logo-title
    (format
      "%s@%s: Keep thinking while not in coding!"
      (user-login-name) (system-name))
    dashboard-startup-banner
    (concat user-emacs-directory "dashboard-banner.png")
    dashboard-items
    '((bookmarks . 5)
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
  (autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t))

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
  '(
     "/.emacs.d/elfeed/*"
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
    undo-tree-history-directory-alist
    `(("." . ,(expand-file-name "undo" user-emacs-directory))))
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
;; chinese pinyin input method
;; git clone https://github.com/tumashu/pyim
;; -----------------------------------------------------------------------------
(when
  (and
    (jh/linux?)
    (require 'pyim)
    (require 'pyim-basedict))
  (pyim-basedict-enable)
  (setq
    pyim-dicts
    `((:name "bigdict"
        :file ,(expand-file-name "pyim/bigdict.pyim" user-emacs-directory)))
    pyim-page-tooltip 'posframe
    default-input-method "pyim"))

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

;; (defvar jh/english-font-size 14 "English font size.")
;; (defvar jh/chinese-font-size 22 "Chinese font size.")

;; ;; english font
;; (set-face-attribute
;;   'default nil :font (format "WenQuanYi Micro Hei Mono %d" jh/english-font-size))
;; ;; 中文字体单独设置
;; (dolist
;;   (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font
;;     (frame-parameter nil 'font)
;;     charset (font-spec :family "WenQuanYi Micro Hei Mono" :size jh/chinese-font-size)))

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
(defvar jh/transparency-alist
  '((100 . 100) (90 . 50) (80 . 50) (70 . 50))
  "Transparency list")

(defun jh/cycle-transparency ()
  "Cycling frame transparency."
  (interactive)
  (let*
    ((alpha (frame-parameter nil 'alpha))
      (value (or (and alpha (car alpha)) 100))
      (lookup
        (member-if
          #'(lambda (e) (= (car e) value)) jh/transparency-alist))
      (next
        (if (> (length lookup) 1)
          (cadr lookup) (car jh/transparency-alist))))
    (set-frame-parameter nil 'alpha next)))

(provide 'init-desktop)
