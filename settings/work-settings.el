;; work settings

(setq exec-path
      (append '("C:\\WinPython-64bit-3.6.1.0Qt5\\python-3.6.1.amd64\\" 
			  "C:\\WinPython-64bit-3.6.1.0Qt5\\python-3.6.1.amd64\\Scripts\\"
			  "C:\\WinPython-64bit-3.6.1.0Qt5\\python-3.6.1.amd64\\lib\\site-packages\\"
			  "C:\\PortableGit\\cmd\\"
			  "C:\\PortableGit\\bin\\"
			  "C:\\PgSQL\\bin"
			  "C:\\gow-0.8.0\\bin"
			  )
	      exec-path ))

(setenv "PATH" (concat 
		       "C:\\WinPython-64bit-3.6.1.0Qt5\\python-3.6.1.amd64\\"
		       ";C:\\WinPython-64bit-3.6.1.0Qt5\\python-3.6.1.amd64\\Scripts\\"
		       ";C:\\WinPython-64bit-3.6.1.0Qt5\\python-3.6.1.amd64\\lib\\site-packages\\"
		       ";C:\\PortableGit\\cmd\\"
		       ";C:\\PortableGit\\bin\\"
		       ";C:\\PgSQL\\bin"
		       ";C:\\gow-0.8.0\\bin"
		       ";"
		       (getenv "PATH")
		       ))

;; global variables
(setq desktop "~/../../Desktop")

;; work specific highlight-sexp color
;; (setq hl-sexp-background-color "floral white"); disable temporarily

;; windows-specific font and colorscheme
(set-face-attribute 'default nil :family "Consolas" :height 110)
(load-theme 'tsdh-dark)

;; fix some annoying python encoding problems
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")
(setenv "PYTHONIOENCODING" "utf-8")

;; custom functions

(defun gt-keywords-report (keywords)
  "Pull a google trend report and save result to current directory."
  (interactive)
  (let ((path (concat (expand-file-name "~") "/projects/google_trends/")))
    (async-shell-command (format
		    "python %sgoogle_trends.py keywords %s" path keywords))))

(defun gt-trends ()
  "Get list of currently trending search terms from Google Trends."
  (interactive)
  (let ((path (concat (expand-file-name "~") "/projects/google_trends/")))
    (insert (shell-command-to-string (format
				      "python %sgoogle_trends.py trends" path)))))

(defun twitter-trends ()
  "Get list of currently trending terms from Twitter."
  (interactive)
  (let ((path (concat (expand-file-name "~") "/projects/twitter_client/")))
    (insert  (shell-command-to-string (format "python %stwitter_cli.py trends" path)))))

(defun twitter-search (query)
  (interactive (list
                (read-string
		 (format "query (%s): " (replace-regexp-in-string "\n" "" (thing-at-point 'line)))
                             nil nil (replace-regexp-in-string "\n" "" (thing-at-point 'line)))))
  (browse-url-default-browser
   (concat  "https://www.twitter.com/search?q="
	    (replace-regexp-in-string "\\#" "%23" query))))

(defun trends ()
  (interactive)
  (insert "Google Trends")
  (newline)
  (gt-trends)
  (newline)
  (insert "Twitter Trends")
  (newline)
  (twitter-trends)
  )
