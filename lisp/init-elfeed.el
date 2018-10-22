;; -----------------------------------------------------------------------------
;; elfeed
;; -----------------------------------------------------------------------------
(when (require 'elfeed)
  (setq elfeed-feeds
    '(
       ;; ----------------------------------------------------------------------
       ;; emacs
       ("http://planet.emacsen.org/atom.xml" emacsen emacs important)
       ("http://pragmaticemacs.com/feed/" pragmaticemacs emacs)
       ("http://ergoemacs.org/emacs/blog.xml" xah emacs)
       ("http://sachachua.com/blog/feed/" sachachua emacs)
       ;; programming
       ("http://www.ruanyifeng.com/blog/atom.xml" ruanyifeng blog coding)
       ("http://blog.binchen.org/rss.xml" chenbin blog coding)
       ("http://feeds2.feedburner.com/catonmat" catonmat blog coding)
       ("https://nullprogram.com/feed/" nullprogram blog coding important)
       ("https://blog.csdn.net/pennyliang/rss/list" pennyliang blog coding)
       ;; news
       ("https://laod.cn/feed/" laod blog news)
       ("https://www.zhihu.com/rss" zhihu blog daily-popular news)
       ("https://blog.github.com/blog.atom" github blog git)
       ;; ----------------------------------------------------------------------
       ))

  (defvar elfeed-search-default-filter "@18-months-ago #100 -junk +unread")

  (defun jh/elfeed-search-reset-default-filter ()
    "Reset elfeed filter to the default."
    (interactive)
    (setq elfeed-search-filter elfeed-search-default-filter)
    (elfeed-search-update :force))
  (defun jh/elfeed-mark-all-as-read ()
    "Mark all entries read"
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread))

  (define-key elfeed-search-mode-map (kbd "S")
    'jh/elfeed-search-reset-default-filter)
  (define-key elfeed-search-mode-map (kbd "R")
    'jh/elfeed-mark-all-as-read)

  (defalias 'jh/elfeed-search-tag-all-important
    (elfeed-expose #'elfeed-search-tag-all 'important)
    "Add the `important' tag to all selected entries.")
  (defalias 'jh/elfeed-search-untag-all-important
    (elfeed-expose #'elfeed-search-untag-all 'important)
    "Remove the `important' tag to all selected entries.")
  (defalias 'jh/elfeed-search-tag-all-junk
    (elfeed-expose #'elfeed-search-tag-all 'junk)
    "Add the `junk' tag to all selected entries.")

  (define-key elfeed-search-mode-map (kbd "i")
    'jh/elfeed-search-tag-all-important)
  (define-key elfeed-search-mode-map (kbd "I")
    'jh/elfeed-search-untag-all-important)
  (define-key elfeed-search-mode-map (kbd "j")
    'jh/elfeed-search-tag-all-junk)


  ;; highlight some feed tags
  (dolist
    (face (list '(emacs (:foreground "#c065db"))
            '(news (:foreground "#268bd2"))
            '(github (:foreground "#28a745"))
            '(important (:foreground "#d33682"))))
    (push face elfeed-search-face-alist))

  ;; display feeds last 18 months ago, total items not greater than 100, and
  ;; remove junk, show unread only
  (setq-default elfeed-search-filter elfeed-search-default-filter)
  (setq url-queue-timeout 30)
  (global-set-key (kbd "<f7>") 'elfeed)
  (global-set-key (kbd "C-c e") 'elfeed))


(provide 'init-elfeed)
