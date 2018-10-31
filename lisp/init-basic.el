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
;; projectile
;; -----------------------------------------------------------------------------
(when (require 'projectile)
  (projectile-mode 1)
  (setq-default
    projectile-mode-line-prefix " Proj"
    projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "M-9") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-register-project-type 'py3code '("requirements.txt")
    :test "python -m unittest"
    :compile "pip install -r requirements.txt"
    :run "python -m unittest test/test_basic.py")
  (projectile-register-project-type 'yarn '("package.json")
    :compile "yarn install"
    :test "yarn test"
    :run "yarn start"))


(provide 'init-basic)
