(defun wf/symbol-or-selection-at-point ()
  "Read symbol and selection at point."
  (if (use-region-p)
    (let
      ((beg (region-beginning)) (end (region-end)))
      (deactivate-mark)
      (buffer-substring-no-properties beg end))
    (symbol-name (symbol-at-point))))

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
    (query-replace old-text new-text)))

(defun workflow-format-code ()
  "Format codes."
  (interactive)
  (cond
    ((eq major-mode 'java-mode) (spt/meghanada-format-code))
    ((eq major-mode 'python-mode) (elpy-format-code))
    ((eq major-mode 'typescript-mode) (tide-format))
    ((eq major-mode 'mhtml-mode) (jh/html-format-code))
    (t (message "Ops, no format backend!"))))

(defun workflow-reveal-in-file-manager ()
  "Open the folder containing this buffer file"
  (interactive)
  (browse-url default-directory))

(defun workflow-drop-file (&optional startdir)
  "Drop the file content to current point according to action."
  (interactive)
  (let
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
              (jh/git-project-root-dir default-directory)))))
      (t (error "Never happend in workflow-drop-file!")))))

(defun workflow-send-to-shell ()
  "Send selected text to shell."
  (interactive)
  (or (jh/mac?)
    (error "Send region only support on macOS!"))
  (if (use-region-p)
    (jh/iterm2-send-region)
    (jh/iterm2-send-string (thing-at-point 'line))))

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

(defun workflow-join-line ()
  "Join line."
  (interactive)
  (if (use-region-p)
    (let*
      ((beg (region-beginning))
        (end (region-end))
        (count (- (line-number-at-pos end) (line-number-at-pos beg))))
      (progn
        (deactivate-mark)
        (goto-char end)
        (dotimes (i count) (join-line))))
    (join-line)))

(provide 'init-workflow)
