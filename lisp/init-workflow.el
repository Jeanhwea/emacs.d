;; utils
(defun wf/symbol-or-selection-at-point ()
  "Read symbol and selection at point."
  (if (use-region-p)
    (let
      ((beg (region-beginning)) (end (region-end)))
      (deactivate-mark)
      (buffer-substring-no-properties beg end))
    (let
      ((sym (symbol-at-point)))
      (and sym (symbol-name sym)))))

;; Part 1-1: windows commands
(defun workflow-unique-window ()
  "Delete other windows, just leave current windows."
  (interactive)
  (delete-other-windows))

(defun workflow-close-current-window ()
  "Delete current window."
  (interactive)
  (delete-window))

(defun workflow-horizontal-split-window ()
  "Horizontal split window."
  (interactive)
  (split-window-below))

(defun workflow-vertically-split-window ()
  "Vertically split window."
  (interactive)
  (split-window-right))

(defun workflow-quit-emacs-application ()
  "Quit emacs."
  (interactive)
  (save-buffers-kill-terminal))

(defun workflow-expand-fullscreen ()
  "Expand or toggle the fullscreen of frame."
  (interactive)
  (toggle-frame-fullscreen))

;; Part 1-2: File & Buffer, Bookmark
(defun workflow-recentf-open-file ()
  "Open recently opened files."
  (interactive)
  (counsel-recentf))

(defun workflow-fuzzy-open-file ()
  "Open file in this project, fuzzy find way."
  (interactive)
  (projectile-find-file))

(defun workflow-git-open-file ()
  "Open file under git version control."
  (interactive)
  (counsel-git))

(defun workflow-dot-open-file ()
  "Open file under current directory."
  (interactive)
  (counsel-find-file))

(defun workflow-bookmark-open-file ()
  "Open file that bookmarked."
  (interactive)
  (counsel-bookmark))

(defun workflow-bookmark-current-file ()
  "Add/Remove current file to bookmark"
  (interactive)
  (let*
    ((name (buffer-name))
      (lookup (member name (bookmark-all-names))))
    (if lookup
      (progn
        (bookmark-delete name)
        (message (concat "Removed bookmark: " name)))
      (progn
        (bookmark-set name)
        (message (concat "Added bookmark: " name))))))

;; Part 1-3: Buffers
(defun workflow-swith-to-buffer ()
  "Swith to a buffer."
  (interactive)
  (ivy-switch-buffer))

(defun workflow-alternative-buffer ()
  "Swap between recently buffer."
  (interactive)
  (progn
    (switch-to-buffer nil)))

(defun workflow-kill-buffer ()
  "Kill current buffer"
  (interactive)
  (kill-buffer))

(defun workflow-new-buffer ()
  "Create a temporary buffer."
  (interactive)
  (let
    ((suffix (format-time-string "%Y%m%d%H%M%S")))
    (switch-to-buffer (concat "scratch+" suffix))))

(defun workflow-save-all-buffers ()
  "Save buffers."
  (interactive)
  (save-some-buffers t t))

(defun workflow-delete-current-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is binding to this buffer!"))
  (when
    (yes-or-no-p
      (format "Delete %s: "
        (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; Part 1-4: Highlight symbol & change color theme
(defun workflow-highlight-symbol ()
  "Toggle highlight state of symbol at point."
  (interactive)
  (let*
    ((sym (wf/symbol-or-selection-at-point))
      (sym-regexp (concat "\\_<" sym "\\_>"))
      (lookup
        (and sym (member sym-regexp (mapcar #'car hi-lock-interactive-patterns)))))
    (if sym
      (if lookup (unhighlight-regexp sym-regexp) (highlight-symbol-at-point))
      (message "Ops: No sysmbol to highlight at point!"))))

(defun workflow-unhighlight-all ()
  "Unhighlight all symbols"
  (interactive)
  (let ((pattens (mapcar #'car hi-lock-interactive-patterns)))
    (dolist (sym-regexp pattens) (unhighlight-regexp sym-regexp))))

(defun workflow-cycle-color-theme ()
  "Cycle color theme ring."
  (interactive)
  (jh/cycle-color-theme))

;; TODO: update
(defun workflow-replace ()
  "Better workflow for query-replace."
  (interactive)
  (let*
    ((old-text
       (read-string "Replace: "
         (wf/symbol-or-selection-at-point)))
      (new-text
        (read-string (format "Replace %s with: " old-text)
          old-text)))
    (progn
      (beginning-of-line)
      (query-replace old-text new-text))))

(defvar wf/project-type-alist
  '(("pom.xml" . maven) ("package.json" . angular))
  "Project file to project type.")

(defun wf/project-type ()
  "Get current project type."
  (let*
    ((root (jh/git-root default-directory))
      (lookup
        (remove-if-not
          #'(lambda (f)
              (file-exists-p (expand-file-name f root)))
          (mapcar #'car wf/project-type-alist))))
    (cdr (assoc (car lookup) wf/project-type-alist))))

(defun workflow-open-class ()
  "Open a class source file."
  (interactive)
  (let
    ((project-type (wf/project-type)))
    (cond
      ((equal project-type 'maven) (spt/jump-to-class))
      ((equal project-type 'angular) (ng/find-source-file))
      (t (message "Ops, unknown project type!")))))

(defvar wf/known-indent-mode
  '(nxml-mode mhtml-mode less-css-mode emacs-lisp-mode sh-mode ymal-mode)
  "Known indent major mode.")

(defun wf/indent-buffer ()
  "Indent current buffer."
  (save-excursion (indent-region (point-min) (point-max))))

(defun workflow-format-code ()
  "Format codes."
  (interactive)
  (cond
    ((eq major-mode 'java-mode) (jh/format-java-source))
    ((eq major-mode 'python-mode) (elpy-format-code))
    ((eq major-mode 'typescript-mode) (tide-format))
    ((eq major-mode 'sql-mode) (sqlformat-buffer))
    ((member major-mode wf/known-indent-mode)
      (wf/indent-buffer))
    (t (message "Ops, no format backend!"))))


(defun workflow-reveal-in-file-manager ()
  "Open the folder containing this buffer file"
  (interactive)
  (browse-url default-directory))

(defun workflow-drop-file (&optional startdir)
  "Drop the file content to current point according to action."
  (interactive)
  (let*
    ((options
       '("Content" "Filename" "Relative Path" "Relative to Project Root"))
      (action (completing-read "Drop Action >> " options))
      (filename
        (expand-file-name
          (read-file-name "Drop file >> "
            (or startdir default-directory)))))
    (cond
      ((string= action "Content")
        (and (file-regular-p filename)
          (file-readable-p filename)
          (insert (jh/read-file-content filename))))
      ((string= action "Filename")
        (and (file-exists-p filename) (insert filename)))
      ((string= action "Relative Path")
        (and (file-exists-p filename)
          (insert (jh/relative-path filename default-directory))))
      ((string= action "Relative to Project Root")
        (and (file-exists-p filename)
          (insert
            (jh/relative-path filename
              (jh/git-root default-directory)))))
      (t (error "Never happend in workflow-drop-file!")))))

(defun workflow-send-to-shell ()
  "Send selected text to shell."
  (interactive)
  (or (jh/mac?)
    (error "Send region only support on macOS!"))
  (if (use-region-p)
    (jh/iterm2-send-region)
    (jh/iterm2-send-string (thing-at-point 'line))))

(defun workflow-rename-file (name)
  "Rename both current buffer and file it's visiting to name."
  (interactive "sNew name: ")
  (let
    ((buffername (buffer-name))
      (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" buffername))
    (progn
      (when (file-exists-p filename)
        (rename-file filename name 1))
      (set-visited-file-name name)
      (rename-buffer name))))

(defun workflow-cycle-hide-level ()
  "Cycling hide level."
  (interactive)
  (if (local-variable-p 'hs-cycle-level)
    (progn
      (setq hs-cycle-level (% (+ hs-cycle-level 1) 5))
      (hs-hide-level (+ hs-cycle-level 1)))
    (progn
      (set (make-local-variable 'hs-cycle-level) 0)
      (hs-hide-level (+ hs-cycle-level 1)))))

;; -----------------------------------------------------------------------------
;; shrimp shell
;; -----------------------------------------------------------------------------
(defun wf/shrimp-project-name ()
  "Return the project name."
  (let*
    ((dir default-directory)
      (root
        (directory-file-name
          (or (jh/git-root dir) dir)))
      (parent (regexp-quote (jh/parent-dir root))))
    (jh/re-replace parent "" root nil 'literal)))

(defun wf/shrimp-shell-name ()
  "Return the shell name."
  (let ((name (wf/shrimp-project-name)))
    (if name (format "*shrimp[%s]*" name) "*shrimp*")))

(defun workflow-shrimp-open ()
  "open a eshell as a temporary shell, and rename the buffer to `*shrimp*'."
  (interactive)
  (let ((name (wf/shrimp-shell-name)))
    (if (get-buffer name)
      (switch-to-buffer name)
      (let ((eshell-buffer-name name)) (eshell)))))

;; -----------------------------------------------------------------------------
;; terminal
;; -----------------------------------------------------------------------------
(defun workflow-term-cd ()
  "Send current working directory to terminal."
  (interactive)
  (cond
    ((jh/linux?) (jh/tilix-cd))
    ((jh/mac?) (jh/iterm2-cd))
    (t (message "Unsupport term cd on this OS!"))))

(provide 'init-workflow)
