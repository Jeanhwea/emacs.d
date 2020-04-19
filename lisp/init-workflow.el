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
(defun workflow-open-default-folder ()
  "Open the folder containing this buffer file"
  (interactive)
  (browse-url default-directory))

(defun workflow-open-this-file ()
  "Open this file by default open method."
  (interactive)
  (let ((file (buffer-file-name)) (line (jh/line-number)))
    (cond
      ((eq major-mode 'java-mode)
        (progn
          (shell-command (format "idea %s:%d" file line))
          (message (format "idea open %s:%d" file line))))
      (t (browse-url (buffer-file-name))))))

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
(defun workflow-switch-to-buffer ()
  "Switch to a buffer."
  (interactive)
  (ivy-switch-buffer))

(defun workflow-alternative-buffer ()
  "Swap between recently buffer."
  (interactive)
  (switch-to-buffer nil))

(defun workflow-kill-buffer ()
  "Kill current buffer"
  (interactive)
  (ido-kill-buffer))

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

(defun workflow-rename-current-file (name)
  "Rename both current buffer and file it's visiting to name."
  (interactive "sRename current file to: ")
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

;; Part 1-4: Hide & Show
(defun workflow-hideshow-toggle ()
  "Toggling hide and show state."
  (interactive)
  (hs-toggle-hiding))

(defun workflow-hide-all-level ()
  "Hide all level."
  (interactive)
  (hs-hide-all))

(defun workflow-show-all-level ()
  "Show all level."
  (interactive)
  (hs-show-all))

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

;; Part 1-5: Highlight symbol & Change Colortheme
(defun workflow-highlight-symbol ()
  "Toggle highlight state of symbol at point."
  (interactive)
  (let*
    ((sym (jh/symbol-at-point))
      (sym-regexp (concat "\\_<" sym "\\_>"))
      (lookup
        (and sym (member sym-regexp (mapcar #'car hi-lock-interactive-patterns)))))
    (if sym
      (if lookup (unhighlight-regexp sym-regexp) (highlight-symbol-at-point))
      (message "Ops: No sysmbol to highlight at point!"))))

(defun workflow-wipeout-all-highlights ()
  "Unhighlight all symbols"
  (interactive)
  (let ((pattens (mapcar #'car hi-lock-interactive-patterns)))
    (dolist (sym-regexp pattens) (unhighlight-regexp sym-regexp))))

(defun workflow-colortheme-cycling ()
  "Cycling color theme ring."
  (interactive)
  (jh/cycle-color-theme))

(defun workflow-transparency-cycling ()
  "Cycling frame transparency."
  (interactive)
  (jh/cycle-transparency))

;; Part 2-1: Search
(defun workflow-search-any-text ()
  "Search any text by grep-like program."
  (interactive)
  (counsel-ag))

(defun workflow-search-git ()
  "Search by git-grep."
  (interactive)
  (counsel-git-grep))

(defun workflow-search-ag ()
  "Search by ag"
  (interactive)
  (call-interactively #'ag))

;; Part 2-2: Replace & Regular eXpression Replace
(defun workflow-replace ()
  "Better workflow for query-replace."
  (interactive)
  (let*
    ((old-text
       (read-string "Replace: " (jh/symbol-at-point)))
      (new-text
        (read-string (format "Replace %s with: " old-text) old-text)))
    (progn
      (beginning-of-line)
      (query-replace old-text new-text))))

(defun workflow-replace-projectile ()
  "Repace in this project."
  (interactive)
  (projectile-replace))

(defun workflow-regexp-replace ()
  "Better workflow for regexp query replace."
  (interactive)
  (let*
    ((old-regexp
       (read-string "Replace regexp: " (jh/symbol-at-point)))
      (new-regexp
        (read-string (format "Replace regexp %s with: " old-regexp) old-regexp)))
    (progn
      (beginning-of-line)
      (query-replace-regexp old-regexp new-regexp))))

(defun workflow-regexp-replace-projectile ()
  "Regexp replace in this project."
  (interactive)
  (projectile-replace-regexp))

;; Part 2-3: Source code realted: Formatting, Comment
(defun workflow-format-current-source ()
  "Format codes."
  (interactive)
  (cond
    ((eq major-mode 'java-mode) (jh/format-java-source))
    ((eq major-mode 'python-mode) (elpy-format-code))
    ((eq major-mode 'typescript-mode) (tide-format))
    ((eq major-mode 'sql-mode) (sqlformat-buffer))
    ((member major-mode
       '(emacs-lisp-mode less-css-mode mhtml-mode nxml-mode sh-mode ymal-mode))
      (jh/indent-current-buffer))
    (t (message "Ops, no format backend!"))))

(defun workflow-comment-source-code ()
  "Comment the source code"
  (interactive)
  (if
    (use-region-p)
    (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-line 1)))

;; Part 2-4: Code Navigation
(defun workflow-goto-definition ()
  "Goto definition of current symbol."
  (interactive)
  (evil-jump-to-tag))

;; Part 2-5: Test
(defun workflow-post-http-request ()
  "Post a HTTP request by verb in org-mode."
  (interactive)
  (verb-send-request-on-point-other-window-stay))

;; Part 2-6: Git version control
(defun workflow-git-popup ()
  "Popup magit control."
  (interactive)
  (magit))

(defun workflow-git-commit-changes ()
  "Commit current changes."
  (interactive)
  (progn
    (workflow-save-all-buffers)
    (magit-stage-modified)
    (magit-commit)))

(defun workflow-git-push ()
  "Push current project to remote."
  (interactive)
  (progn
    (workflow-save-all-buffers)
    (magit-push)))

(defun workflow-git-pull ()
  "Pull all change from previous remote."
  (interactive)
  (progn
    (workflow-save-all-buffers)
    (magit-pull)))

(defun workflow-git-keep-current ()
  "Accept current change in this diff."
  (interactive)
  (smerge-keep-current))

(defun workflow-git-browse-remote ()
  "Open git remote in browser."
  (interactive)
  (browse-at-remote))

;; Part 2-9: Misc
(defun workflow-codetta-expand-command ()
  "Codetta expand command."
  (interactive)
  (ct/expand-command))

(defun workflow-eshell-open-from-here ()
  "open a eshell as a temporary shell, and rename the buffer to `*shrimp*'."
  (interactive)
  (let*
    ((project (jh/project-name))
      (name (if project (format "*shrimp[%s]*" project) "*shrimp*")))
    (if (get-buffer name)
      (switch-to-buffer name) (let ((eshell-buffer-name name)) (eshell)))))

(defun workflow-working-directory-send ()
  "Send current working directory to terminal."
  (interactive)
  (cond
    ((jh/linux?) (jh/tilix-cd))
    ((jh/mac?) (jh/iterm2-cd))
    (t (message "Unsupport term cd on this OS!"))))

;; ------------------------------------------------------------------------------
;; Global common used commands
(defun workflow-inflect-string ()
  "Inflect string or word cases."
  (interactive)
  (cond
    ((eq major-mode 'java-mode) (string-inflection-java-style-cycle))
    ((eq major-mode 'python-mode) (string-inflection-python-style-cycle))
    (t (string-inflection-all-cycle-function))))

(defun workflow-M-x ()
  "Start M-x, but add `workflow-' as the default prefix."
  (interactive)
  (counsel-M-x "^workflow "))

;; todo: add
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

(provide 'init-workflow)
