;; -----------------------------------------------------------------------------
;; basic keybinds
;; -----------------------------------------------------------------------------
(global-set-key (kbd "C-,") 'set-mark-command)
(global-set-key (kbd "C-x C-,") 'pop-global-mark)
(global-set-key (kbd "C-j") 'join-line)

(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-o") 'other-window)

(if (string-equal "windows-nt" system-type)
  (global-set-key (kbd "C-M-f") 'toggle-frame-maximized)
  (global-set-key (kbd "C-M-f") 'toggle-frame-fullscreen))


;; C-o to open next lines
(defun jh/open-next-line (N)
  "Open N lines next the cursor"
  (interactive "P")
  (save-excursion
    (move-end-of-line 1)
    (open-line N)))
;; C-S-o to open previous lines
(defun jh/open-previous-line (N)
  "Open N lines before the cursor"
  (interactive "P")
  (save-excursion
    (move-beginning-of-line 1)
    (newline N)))

(global-set-key (kbd "C-o") 'jh/open-next-line)
(global-set-key (kbd "C-S-o") 'jh/open-previous-line)

;; (defun jh/temporary-buffer ()
;;   "Create a temporary buffer"
;;   (interactive)
;;   (switch-to-buffer (make-temp-name "scratch+")))


;; --------------------------------------------------------------------------
;; behaviour
;; --------------------------------------------------------------------------
(recentf-mode 1)
(save-place-mode 1)
(electric-pair-mode 1)
;(desktop-save-mode 1)
(ido-mode 1)

(put 'narrow-to-page 'disable nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defun jh/upcase-previous-word (N)
  "Convert to upper case previous word, moving over"
  (interactive "P")
  (save-excursion
    (backward-word N)
    (if (integerp N)
      (upcase-word N)
      (upcase-word 1))))

(global-set-key (kbd "M-u") 'jh/upcase-previous-word)


;; -----------------------------------------------------------------------------
;; tab, space width
;; -----------------------------------------------------------------------------
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Language-independent indentation
(add-hook 'mhtml-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq html-indent-level 2)
    (setq tab-width 2)))

(add-hook 'js-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq js-indent-level 2)
    (setq tab-width 2)))

(add-hook 'css-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq css-indent-offset 2)
    (setq tab-width 2)))

(add-hook 'emacs-lisp-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq lisp-indent-offset 2)
    (setq tab-width 2)))

(add-hook 'sh-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq sh-basic-offset 2)
    (setq tab-width 2)))

(add-hook 'java-mode-hook
  (lambda()
    (setq show-trailing-whitespace t)
    (setq indent-tabs-mode nil)
    (setq c-basic-offset 2)
    (setq tab-width 2)))

(provide 'init-basic)
