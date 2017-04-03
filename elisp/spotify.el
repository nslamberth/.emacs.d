;; spotify.el
;; spotify interface for emacs
;; based on https://www.youtube.com/watch?v=XjKtkEMUYGc

(require 'url)
(require 'json)

(defun spotify-play-item (uri)
  "play spotify track for given track uri"
  (shell-command (format "spotify %s %s"
			 "play"
			 uri)))

(defun spotify-search (search-query)
  "search tracks for query and return results as alist"
  (with-current-buffer
      (url-retrieve-synchronously
       (format "https://api.spotify.com/v1/search?type=track&q=%s" search-query))
    (goto-char (+ 1 url-http-end-of-headers))
    (json-read-object)))

(defun format-track-for-display (track)
  "format track alist for display in helm"
  (format "%s -- %s"
  (cdr (assoc 'name track))
  (cdr (assoc 'name (elt (cdr (assoc 'artists track)) 0)))))

(defun helm-spotify-search ()
  "run spotify search and return in format usable by helm"
  (mapcar (lambda (track)
	    (cons (format-track-for-display track)
		  track))
	  (cdr (assoc 'items (assoc 'tracks (spotify-search helm-pattern))))))

(defun helm-spotify-play-track (track)
  "helm action for playing track from helm menu"
  (spotify-play-item (cdr (assoc 'uri track))))

(defun spotify-play-pause ()
  "toggle spotify between play/pause states"
  (interactive)
  (shell-command (format "spotify %s"
			 "play/pause")))

(defun spotify-next-track ()
  "go to next track in current play queue"
  (interactive)
  (shell-command (format "spotify next")))

(defun spotify-toggle-shuffle ()
  "go to next track in current play queue"
  (interactive)
  (shell-command (format "spotify shuffle")))


(defvar sample-uri "spotify:track:13X42np3KJr0o2LkK1MG76"
  "sample spotify track uri for testing")

(defvar sample-results (spotify-search "justin timberlake")
  "sample spotify results for testing")

(defvar sample-track (elt (cdr (assoc 'items (assoc 'tracks sample-results))) 2)
  "sample track object for testing")

(defvar helm-source-spotify
  '((name . "Spotify")
    (candidates . helm-spotify-search)
    (volatile)
    (action . (("Play track" . helm-spotify-play-track)))))

(defvar helm-source-my-playlists
  '((name . "My Spotify Playlists")
    (candidates . (("coding" . "spotify:user:nslamberth:playlist:2yPwg6iFNEzgfHETAiYLYa")
		   ("home" . "spotify:user:nslamberth:playlist:7zpnU66sccpuRWqwkGg7iU")
		   ("10s" . "spotify:user:nslamberth:playlist:5EMye9be5g28BBAGLA7OiT")
		   ("discover weekly" . "spotify:user:spotify:playlist:37i9dQZEVXcOBKbLDgbykh")))
    (action . spotify-play-item)))

(global-set-key (kbd "M-s")
		(lambda () (interactive)
		  (let ((debug-on-error t)
			(helm-input-idle-delay 0.5))
		    (helm :sources '(helm-source-spotify)))))

(global-set-key (kbd "M-p")
		(lambda () (interactive)
		  (helm :sources '(helm-source-my-playlists))))

