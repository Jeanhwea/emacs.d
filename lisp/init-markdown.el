;; -----------------------------------------------------------------------------
;; markdown-mode
;; -----------------------------------------------------------------------------
(when (require 'markdown-mode)
  (setq-default
    markdown-asymmetric-header t
    markdown-fontify-code-blocks-natively t
    markdown-header-scaling t
    markdown-hide-urls nil
    markdown-max-image-size '(640 . 480))
  ;; markdown-mode use very bad code font on my Windows machine, get rid of it
  ;; by set `markdown-code-face' to default
  (when (jh/windows?)
    (custom-set-faces
      '(markdown-code-face ((t (:inherit default)))))))

(add-hook 'markdown-mode-hook
  (lambda ()
    (setq pangu-spacing-real-insert-separtor t)
    (pangu-spacing-mode 1)))

(provide 'init-markdown)
