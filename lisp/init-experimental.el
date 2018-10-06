;; -----------------------------------------------------------------------------
;; browse-at-remote
;; -----------------------------------------------------------------------------
;; (when (require 'browse-at-remote)
;;   (defalias 'open 'browse-url))


;; -----------------------------------------------------------------------------
;; elfeed
;; -----------------------------------------------------------------------------
(when (require 'elfeed)
  (setq elfeed-feeds
    '(
       ;; ----------------------------------------------------------------------
       ;; emacs
       ("http://planet.emacsen.org/atom.xml" emacsen emacs imp)
       ("http://pragmaticemacs.com/feed/" pragmaticemacs emacs)
       ("http://ergoemacs.org/emacs/blog.xml" xah emacs)
       ;; programming
       ("http://www.ruanyifeng.com/blog/atom.xml" ruanyifeng blog coding)
       ("http://blog.binchen.org/rss.xml" chenbin blog coding)
       ("http://feeds2.feedburner.com/catonmat" catonmat blog coding)
       ("https://nullprogram.com/feed/" nullprogram blog coding imp)
       ("https://blog.csdn.net/pennyliang/rss/list" pennyliang blog coding)
       ;; news
       ("https://laod.cn/feed/" laod blog news)
       ("https://www.zhihu.com/rss" zhihu blog daily-popular news)
       ;; ----------------------------------------------------------------------
       ))

  ;; highlight some tags of feeds
  (defface good-elfeed-entry '((t :foreground "#39e")) "good feeds.")
  (defface imp-elfeed-entry '((t :foreground "#f0a")) "important feeds.")
  (push '(good good-elfeed-entry) elfeed-search-face-alist)
  (push '(imp imp-elfeed-entry) elfeed-search-face-alist)

  ;; display feeds last 18 months ago, total items not greater than 100, and
  ;; remove junk, show unread only
  (setq-default elfeed-search-filter "@18-months-ago #100 -junk +unread")
  (setq url-queue-timeout 30)
  (global-set-key (kbd "C-x w") 'elfeed))


(provide 'init-experimental)
