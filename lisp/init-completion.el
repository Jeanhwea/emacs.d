;; -----------------------------------------------------------------------------
;; company
;; -----------------------------------------------------------------------------
(when (require 'company)
  (add-hook 'after-init-hook 'global-company-mode)

  ;; https://emacs.stackexchange.com/questions/10837/how-to-make-company-mode-be-case-sensitive-on-plain-text/10838
  ;; https://github.com/company-mode/company-mode/issues/14
  (setq company-dabbrev-downcase nil)

  ;; add normal keybindings
  (define-key company-mode-map (kbd "M-/") 'company-complete)
  (define-key company-active-map (kbd "M-/") 'company-other-backend)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)

  ;; set a global keybindings for more help
  (global-set-key (kbd "M-C-/") 'company-complete)

  (setq-default company-dabbrev-other-buffers 'all
    company-tooltip-align-annotations t))


;; -----------------------------------------------------------------------------
;; ivy, counsel & swiper
;; -----------------------------------------------------------------------------
(when (require 'swiper)
  (ivy-mode 1)
  (setq
    ivy-use-virtual-buffers t
    enable-recursive-minibuffers t)

  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-7") 'counsel-recentf)
  (global-set-key (kbd "C-c r") 'counsel-recentf)
  (global-set-key (kbd "M-8") 'counsel-bookmark)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))


(provide 'init-completion)
