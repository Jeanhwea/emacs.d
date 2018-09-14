;; key bindings
(global-set-key (kbd "C-M-f") 'toggle-frame-fullscreen)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-4") 'delete-window)

;; disable
(setq inhibit-startup-message t)

;; disable some tool bars
;(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; cursor shape
(blink-cursor-mode -1)

;; use bar as default cursor
;;(setq-default cursor-type 'bar)

(when (string-equal "windows-nt" system-type)
  (set-default-font "Courier New 14"))



(provide 'init-gui)
