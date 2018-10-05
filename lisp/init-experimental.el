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
       ("http://planet.emacsen.org/atom.xml" emacsen emacs)
       ("http://pragmaticemacs.com/feed/" pragmaticemacs emacs)
       (" http://feeds.feedburner.com/XahsEmacsBlog" xah emacs)
       ;; programming
       ("http://www.ruanyifeng.com/feed.html" ruanyifeng blog coding)
       ("http://blog.binchen.org/rss.xml" chenbin blog coding)
       ("http://feeds2.feedburner.com/catonmat" catonmat blog coding)
       ;; news
       ("https://laod.cn/feed/" laod blog Internet)
       ;; ----------------------------------------------------------------------
       )))


(provide 'init-experimental)
