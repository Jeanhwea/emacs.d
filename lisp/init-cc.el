(add-hook 'c-mode-common-hook
  #'(lambda()
      (setq
        show-trailing-whitespace t
        ;; indent-tabs-mode t
        indent-tabs-mode nil
        c-basic-offset 4
        tab-width 4
        comment-start "// "
        comment-end ""
        prettify-symbols-alist
        (append prettify-symbols-alist
          '(
             ;; ----------------------------------------------------------------
             ("public" . 9794)          ; ♂
             ("private" . 9792)         ; ♀
             ("this" . 964)             ; τ
             ("return" . 1103)          ; я
             ("new" . 1081)             ; й
             ;; ----------------------------------------------------------------
             ("null" . 1488)            ; א
             ("NULL" . 1488)            ; א
             ("true" . 8730)            ; √
             ("false" . 215)            ; ×
             ;; ----------------------------------------------------------------
             ("&&" . 8743)              ; ∧
             ("||" . 8744)              ; ∨
             ("!" . 172)                ; ¬
             ("<=" . 8804)              ; ≤
             ("!=" . 8800)              ; ≠
             ;; ----------------------------------------------------------------
             )))


      (evil-define-key '(normal visual) 'local (kbd "<tab>") 'cc-tab-action)
      (evil-define-key '(normal visual) 'local (kbd "TAB") 'cc-tab-action)

      (highlight-current-line)
      (rainbow-delimiters-mode 1)))

(defun cc-tab-action ()
  "Default <tab> key action for golang."
  (interactive)
  (jh/tab-dwim))

(defun jh/format-cc-source (&optional file)
  "Format cc source code."
  (let
    ((server (eglot-current-server))
      (file (or file (buffer-file-name))))
    (if (null server)
      (progn
        (jh/indent-current-buffer)
        (message (format "indent buffer only: %s" file)))
      (progn
        (eglot-format-buffer)
        (message (format "format by eglot: %s" file))))))

(defun jh/format-cc-source-cli (&optional file)
  "Format cc source code using command line."
  (let
    ((file (or file (buffer-file-name))))
    (progn
      (save-buffer)
      ;; format buffer
      (shell-command (format "clang-format -style=\"{BasedOnStyle: Google, ColumnLimit:120}\" -i %s" file))
      ;; reload buffer
      (revert-buffer nil t)
      ;; leave a messge
      (message (format "Formatted %s" file)))))

(add-to-list 'auto-mode-alist '("\\.ic\\'" . c++-mode))

(when (require 'cuda-mode)
  (add-to-list 'auto-mode-alist '("\\.cu.cc\\'" . cuda-mode)))

(require 'cmake-mode)

(provide 'init-cc)
