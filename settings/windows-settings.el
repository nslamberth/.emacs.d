;; work settings

(setq exec-path
      (append '("C:\\WinPython-64bit-3.6.1.0Qt5\\python-3.6.1.amd64\\" 
			  "C:\\WinPython-64bit-3.6.1.0Qt5\\python-3.6.1.amd64\\Scripts\\"
			  "C:\\WinPython-64bit-3.6.1.0Qt5\\python-3.6.1.amd64\\lib\\site-packages\\"
			  "C:\\PortableGit\\cmd\\"
			  "C:\\PortableGit\\bin\\"
			  "C:\\PgSQL\\bin"
			  "C:\\gow-0.8.0\\bin"
                          "C:\\ffmpeg\\bin"
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
                       ";C:\\ffmpeg\\bin"
		       ";"
		       (getenv "PATH")
		       ))

;; global variables
(setq desktop "~/../../Desktop")

;; windows-specific font and colorscheme
(set-face-attribute 'default nil :family "Consolas" :height 110)
(load-theme 'tsdh-dark)

;; fix some annoying python encoding problems
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")
(setenv "PYTHONIOENCODING" "utf-8")

;; custom functions

(defun twitter-get-timeline ()
  "Pull twitter timeline (without RT's) and display in new buffer."
  (interactive)
  (async-shell-command
   (concat "python "
           (expand-file-name "~/projects/twitter_client/main.py")) "twitter"))

(defun gt-keywords-report (&rest keywords)
  "Pull a google trend report and save result to current directory."
  (interactive)
  (let ((path (concat (expand-file-name "~") "/projects/google_trends/"))
        (keyword-string (combine-and-quote-strings keywords)))
    (async-shell-command
     (format "python %sgoogle_trends.py keywords --us-only %s" path keyword-string))))

(defun gt-trends ()
  "Get list of currently trending search terms from Google Trends."
  (interactive)
  (let ((path (concat (expand-file-name "~") "/projects/google_trends/")))
    (insert (shell-command-to-string (format
				      "python %sgoogle_trends.py trends" path)))))

;; dired shortcuts
(defun dired-clients () (interactive) (dired "~/../../Desktop/clients/"))
(defun dired-downloads () (interactive) (dired "~/../../Downloads"))
(defun dired-desktop () (interactive) (dired "~/../../Desktop"))
(defun dired-settings () (interactive) (progn (dired "~/.emacs.d/settings")))
(defun dired-projects () (interactive) (progn (dired "~/projects")))


(defun ffmpeg-convert-to-mp3 (file)
  "Convert an audio file to 320k mp3."
  (interactive (list (read-file-name "File to convert:")))
  (async-shell-command
  (string-join
   (list
    "ffmpeg" "-i"
    (concat "\"" (expand-file-name file) "\"")
    "-b:a 320k"
    (concat "\""
	    (replace-regexp-in-string
	     "\\.\\w*$" ".mp3" (expand-file-name file "~/Desktop"))
	    "\"")) " ")))
