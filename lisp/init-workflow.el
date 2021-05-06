;; Part 1-1: windows commands
(defun workflow-delete-other-windows ()
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

(defun workflow-vertical-split-window ()
  "Vertical split window."
  (interactive)
  (split-window-right))

(defun workflow-quit-emacs-application ()
  "Quit emacs."
  (interactive)
  (save-buffers-kill-terminal))

(defun workflow-expand-fullscreen ()
  "Expand or toggle the fullscreen of frame."
  (interactive)
  (if (jh/windows?)
    (toggle-frame-maximized)
    (toggle-frame-fullscreen)))

;; Part 1-2: File & Buffer, Bookmark
(defun workflow-open-in-file-manager ()
  "Open the folder containing this buffer file"
  (interactive)
  (browse-url default-directory))

(defun workflow-inspect-file ()
  "Inspect file by IDEA or Open by default open method."
  (interactive)
  (let ((file (buffer-file-name)) (line (jh/line-number)))
    (cond
      ((eq major-mode 'java-mode)
        (progn
          (shell-command (format "idea %s:%d" file line))
          (message (format "idea open %s:%d" file line))))
      (t (browse-url (buffer-file-name))))))

(defun workflow-buffer-or-recentf-open ()
  "Open buffer or recently opened files."
  (interactive)
  (call-interactively #'counsel-buffer-or-recentf))

(defun workflow-recentf-open ()
  "Open recently opened files."
  (interactive)
  (call-interactively #'counsel-recentf))

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

(defun workflow-jump-to-relative-file ()
  "Jump to relative files."
  (interactive)
  (cond
    ((eq major-mode 'java-mode) (spt/switch-to))
    (t (user-error "Ops: unknown jump to relative file."))))

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
    ((suffix (format-time-string "%Y-%m-%d %H:%M:%S")))
    (switch-to-buffer (format "*empty %s*" suffix))))

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
      (setq hs-cycle-level (% (1+ hs-cycle-level) 5))
      (hs-hide-level (1+ hs-cycle-level)))
    (progn
      (set (make-local-variable 'hs-cycle-level) 0)
      (hs-hide-level (1+ hs-cycle-level)))))

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
      (user-error "Ops: No sysmbol to highlight at point!"))))

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

;; Part 1-6: Git version control
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

;; Part 2-1: Search
(defun workflow-search-any-text ()
  "Search any text by grep-like program."
  (interactive)
  (counsel-rg))

(defun workflow-search-git ()
  "Search by git-grep."
  (interactive)
  (counsel-git-grep))

(defun workflow-search-ag ()
  "Search by ag"
  (interactive)
  ;; (call-interactively #'ag)
  (counsel-ag))

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
  (call-interactively #'projectile-replace))

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
  (call-interactively #'projectile-replace-regexp))

(defun workflow-browse-kill-ring ()
  "Browse the king ring."
  (interactive)
  (call-interactively #'browse-kill-ring))

;; Part 2-3: Source code realted: Formatting, Comment
(defun workflow-format-current-source ()
  "Format codes."
  (interactive)
  (cond
    ((eq major-mode 'java-mode) (jh/format-java-source))
    ((eq major-mode 'web-mode) (jh/format-js-source))
    ((eq major-mode 'python-mode) (elpy-format-code))
    ((eq major-mode 'typescript-mode) (tide-format))
    ((eq major-mode 'sql-mode) (jh/format-sql-source))
    ;; ((member major-mode '(c++-mode cuda-mode)) (jh/format-cc-source))
    ((member major-mode '(c++-mode cuda-mode)) (jh/indent-current-buffer))
    ((member major-mode
       '(emacs-lisp-mode less-css-mode mhtml-mode nxml-mode sh-mode ymal-mode))
      (jh/indent-current-buffer))
    (t (user-error "Ops, no format backend!"))))

(defun workflow-comment-source-code ()
  "Comment the source code"
  (interactive)
  (if
    (use-region-p)
    (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-line 1)))

(defun workflow-emmet-expand ()
  "Expand emmet line."
  (interactive)
  (if (member 'emmet-mode minor-mode-list)
    (call-interactively #'emmet-expand-line)
    (user-error "Ops, emmet-mode is disable in current buffer.")))

(defun workflow-execute-code-action ()
  "Execute action for code writing."
  (interactive)
  (cond
    ((eq major-mode 'java-mode) (call-interactively #'lsp-execute-code-action))
    (t (user-error "Ops, unknown code action type."))))

(defun workflow-rename-symbol-here ()
  "Do rename symbol at point"
  (interactive)
  (cond
    ((eq major-mode 'java-mode) (call-interactively #'lsp-rename))
    (t (user-error "Ops, can not rename symbol here."))))

(defun workflow-describe-things-at-point ()
  "Display help for the thing at point."
  (interactive)
  (cond
    ((eq major-mode 'java-mode) (call-interactively #'lsp-describe-thing-at-point))
    (t (user-error "Ops, unknown describe things."))))

(defun workflow-organize-imports ()
  "Organize imports."
  (interactive)
  (cond
    ((eq major-mode 'java-mode) (call-interactively #'lsp-organize-imports))
    (t (user-error "Ops, unknown command action."))))

(defun workflow-generate-overrides ()
  "Generate Override methods."
  (interactive)
  (cond
    ((eq major-mode 'java-mode) (call-interactively #'lsp-java-generate-overrides))
    (t (user-error "Ops, unknown command action."))))

(defun workflow-generate-getter-and-setter ()
  "Generate Getter and Setter."
  (interactive)
  (cond
    ((eq major-mode 'java-mode)
      (call-interactively #'lsp-java-generate-getters-and-setters))
    (t (user-error "Ops, unknown command action."))))

(defun workflow-extract-to-constant ()
  "Extract constant refactoring."
  (interactive)
  (cond
    ((eq major-mode 'java-mode) (call-interactively #'lsp-java-extract-to-constant))
    (t (user-error "Ops, unknown command action."))))

(defun workflow-add-unimplemented-methods ()
  "Extract constant refactoring"
  (interactive)
  (cond
    ((eq major-mode 'java-mode) (call-interactively #'lsp-java-add-unimplemented-methods))
    (t (user-error "Ops, unknown command action."))))

(defun workflow-create-parameter ()
  "Create parameter refactoring"
  (interactive)
  (cond
    ((eq major-mode 'java-mode) (call-interactively #'lsp-java-create-parameter))
    (t (user-error "Ops, unknown command action."))))

(defun workflow-create-field ()
  "Create field refactoring"
  (interactive)
  (cond
    ((eq major-mode 'java-mode) (call-interactively #'lsp-java-create-field))
    (t (user-error "Ops, unknown command action."))))

(defun workflow-create-local ()
  "Create local refactoring"
  (interactive)
  (cond
    ((eq major-mode 'java-mode) (call-interactively #'lsp-java-create-local))
    (t (user-error "Ops, unknown command action."))))

(defun workflow-extract-method ()
  "Extract method refactoring"
  (interactive)
  (cond
    ((eq major-mode 'java-mode) (call-interactively #'lsp-java-extract-method))
    (t (user-error "Ops, unknown command action."))))

(defun workflow-add-import ()
  "Add missing import"
  (interactive)
  (cond
    ((eq major-mode 'java-mode) (call-interactively #'lsp-java-add-import))
    (t (user-error "Ops, unknown command action."))))

;; Part 2-4: Code Navigation
(defun workflow-goto-definition ()
  "Goto definition of current symbol."
  (interactive)
  (evil-jump-to-tag))

(defun workflow-find-references ()
  "Find references."
  (interactive)
  (cond
    ((eq major-mode 'java-mode) (call-interactively #'lsp-find-references))
    (t (user-error "Ops, no reference found."))))

(defun workflow-goto-implementation ()
  "Goto to implemention."
  (interactive)
  (cond
    ((eq major-mode 'java-mode) (call-interactively #'lsp-goto-implementation))
    (t (user-error "Ops, no implementation found."))))

;; Part 2-5: Test
(defun workflow-post-http-request ()
  "Post a HTTP request by verb in org-mode."
  (interactive)
  (verb-send-request-on-point-other-window-stay))

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
    (t (user-error "Unsupport term cd on this OS!"))))

(defun workflow-encode-string-at-point ()
  "Encode the string at point."
  (interactive)
  (let ((pt1) (pt2))
    (if (use-region-p)
      (and (setq pt1 (region-beginning))
        (setq pt2 (region-end)))
      (and (setq pt1 (beginning-of-thing 'symbol))
        (setq pt2 (end-of-thing 'symbol))))
    (progn
      (setq str (buffer-substring-no-properties pt1 pt2))
      (message "Encoding \"%s\"" str)
      (delete-region pt1 pt2)
      (insert (url-encode-url str)))))

(defun workflow-decode-string-at-point ()
  "Decode the string at point."
  (interactive)
  (let ((pt1) (pt2))
    (if (use-region-p)
      (and (setq pt1 (region-beginning))
        (setq pt2 (region-end)))
      (and (setq pt1 (beginning-of-thing 'symbol))
        (setq pt2 (end-of-thing 'symbol))))
    (progn
      (setq str (buffer-substring-no-properties pt1 pt2))
      (message "Decoding \"%s\"" str)
      (delete-region pt1 pt2)
      (insert (decode-coding-string (url-unhex-string str) 'utf-8)))))

(defun workflow-server-start ()
  "Running a emacs server"
  (interactive)
  (unless (server-running-p) (server-start)))

;; Global common used commands
(defun workflow-inflect-string ()
  "Inflect string or word cases."
  (interactive)
  (cond
    ((eq major-mode 'java-mode)
      (string-inflection-java-style-cycle))
    ((eq major-mode 'python-mode)
      (string-inflection-python-style-cycle))
    (t (string-inflection-all-cycle))))

(defun workflow-M-x ()
  "Start M-x, but add `workflow-' as the default prefix."
  (interactive)
  (counsel-M-x "^workflow "))

(defun workflow-build-tags ()
  "Build the TAGS file."
  (interactive)
  (let*
    ((dir (jh/git-root default-directory))
      (default-directory dir)
      (srcdir (expand-file-name "src" dir)))
    (and
      (file-directory-p srcdir)
      (shell-command (format "ctags -e --languages=java -R %s" srcdir))
      (message (format "Generate TAGS at %s" srcdir)))))

(defun workflow-sql-client-start ()
  "Start the SQL client daemon."
  (interactive)
  (daemon-client-start))

(defun workflow-drop-file (&optional dir)
  "Drop the file content to current point according to action."
  (interactive)
  (let*
    ((opts '("Content" "Filename" "Relative Path" "Relative to Project Root"))
      (action (completing-read "Drop Action >> " opts))
      (filename (read-file-name "Drop file >> " (or dir default-directory)))
      (file (expand-file-name filename)))
    (cond
      ((string= action "Content")
        (and (file-regular-p file) (file-readable-p file) (insert (jh/read-file-content file))))
      ((string= action "Filename")
        (and (file-exists-p file) (insert file)))
      ((string= action "Relative Path")
        (and (file-exists-p file)
          (insert (jh/relative-path file default-directory))))
      ((string= action "Relative to Project Root")
        (and (file-exists-p file)
          (insert (jh/relative-path file (jh/git-root default-directory)))))
      (t (error "Never happend in workflow-drop-file!")))))

(defun workflow-send-to-shell ()
  "Send selected text to shell."
  (interactive)
  (or (jh/mac?)
    (error "Send region only support on macOS!"))
  (if (use-region-p)
    (jh/iterm2-send-region)
    (jh/iterm2-send-string (thing-at-point 'line))))

(defun workflow-copy-sql-from-jpa-query ()
  "Copy SQL from query, and yank to kill ring."
  (interactive)
  (if (spt/jpa-query-start-point)
    (kill-new (spt/jpa-decode-query (spt/jpa-yank-query-str)))
    (user-error "No @Query(...) annotation found!")))

(defun workflow-trans-sql-to-jpa-query ()
  "Trans SQL to query, and yank to kill ring."
  (interactive)
  (kill-new (spt/jpa-encode-query (spt/jpa-yank-sql-str))))

(defun workflow-format-jpa-query ()
  "Format SQL inside @Query(...) string."
  (interactive)
  (if (spt/jpa-query-start-point)
    (let ((sql-str (spt/jpa-decode-query (spt/jpa-yank-query-str)))
           (beg) (end))
      (with-temp-buffer
        (progn
          (insert sql-str)
          (jh/format-sql-source)
          (setq sql-str (jh/re-replace ";\n*" "" (jh/current-buffer)))))
      ;; (message sql-str)
      (progn
        (search-backward "@Query(")
        (setq beg (point))
        (re-search-forward ")$")
        (setq end (point))
        (delete-region beg end)
        (insert (spt/jpa-encode-query sql-str))))
    (user-error "No @Query(...) annotation found!")))

(defun workflow-copy-sql-from-jpa-formula ()
  "Copy jpa SQL to kill ring."
  (interactive)
  (if (spt/jpa-formula-start-point)
    (kill-new (spt/jpa-decode-formula (spt/jpa-yank-formula-str)))
    (user-error "No @Formula(...) annotation found!")))

(defun workflow-trans-sql-to-jpa-formula ()
  "Trans SQL to kill ring."
  (interactive)
  (kill-new (spt/jpa-encode-formula (spt/jpa-yank-sql-str))))

(defun workflow-prettify-sql-file ()
  "Format all sql file."
  (interactive)
  (jh/format-sql-file))

(provide 'init-workflow)
