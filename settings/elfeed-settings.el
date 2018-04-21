;; elfeed settings

; recommended curl settings
; from https://github.com/fasheng/elfeed-protocol
(setq elfeed-use-curl t)
(setf url-queue-timeout 30)
(setq elfeed-curl-extra-arguments '("--insecure"))

; feed configuration
(setq  elfeed-feeds
       '("http://feeds.washingtonpost.com/rss/national"
        "http://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml"
        "http://feeds.arstechnica.com/arstechnica/index.xml"
        "http://feeds.washingtonpost.com/rss/politics"
        "https://www.politico.com/rss/congress.xml"
        "https://www.politico.com/rss/politics08.xml"
        ))

; helper functions

(defun eww-elfeed-entry ()
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
  (switch-to-buffer-other-window (get-buffer-create "*eww*"))
  (eww link)))

; set up evil keybindings
(evil-define-key 'normal elfeed-search-mode-map (kbd "r") 'elfeed-search-fetch)
(evil-define-key 'normal elfeed-search-mode-map (kbd "R") 'elfeed-search-fetch)
(evil-define-key 'normal elfeed-search-mode-map (kbd "d") 'elfeed-search-show-entry)
(evil-define-key 'normal elfeed-search-mode-map (kbd "s") 'elfeed-search-live-filter)
(evil-define-key 'normal elfeed-search-mode-map (kbd "q") 'evil-delete-buffer)
(evil-define-key 'normal elfeed-show-mode-map (kbd "n") 'elfeed-show-next)
(evil-define-key 'normal elfeed-show-mode-map (kbd "p") 'elfeed-show-prev)
(evil-define-key 'normal elfeed-show-mode-map (kbd "e") 'eww-elfeed-entry)
(evil-define-key 'normal elfeed-show-mode-map (kbd "q") 'evil-delete-buffer)
(evil-define-key 'normal elfeed-show-mode-map (kbd "u") '(lambda () (interactive) (switch-to-buffer "*elfeed-search*")))

; disable images in elfeed-entry pages
(setq shr-inhibit-images t)
