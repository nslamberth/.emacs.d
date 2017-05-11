;; work settings

(setq exec-path
      (append '("C:\\WinPython-64bit-3.6.1.0Qt5\\python-3.6.1.amd64\\" 
			  "C:\\WinPython-64bit-3.6.1.0Qt5\\python-3.6.1.amd64\\Scripts\\"
			  "C:\\WinPython-64bit-3.6.1.0Qt5\\python-3.6.1.amd64\\lib\\site-packages\\"
			  "C:\\PortableGit\\cmd\\"
			  "C:\\PortableGit\\bin\\"
			  "C:\\PortableGit\\usr\\bin"
			  "C:\\PgSQL\\bin"
			  )
	      exec-path ))

(setenv "PATH" (concat 
		       "C:\\WinPython-64bit-3.6.1.0Qt5\\python-3.6.1.amd64\\"
		       ";C:\\WinPython-64bit-3.6.1.0Qt5\\python-3.6.1.amd64\\Scripts\\"
		       ";C:\\WinPython-64bit-3.6.1.0Qt5\\python-3.6.1.amd64\\lib\\site-packages\\"
		       ";C:\\PortableGit\\cmd\\"
		       ";C:\\PortableGit\\bin\\"
		       ";C:\\PortableGit\\usr\\bin"
		       ";C:\\PgSQL\\bin"
		       ";"
		       (getenv "PATH")
		       ))

;; global variables
(setq desktop "~/../../Desktop")

;; work specific highlight-sexp color
(setq hl-sexp-background-color "floral white")

;; fix some annoying python encoding problems
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")
(setenv "PYTHONIOENCODING" "utf-8")

;; custom functions

(defun google-trends-pull-report (keywords)
  "Pull a google trend report and save result to current directory."
  (interactive)
  (let ((path (concat (expand-file-name "~") "/projects/google_trends/")))
    (async-shell-command (format
		    "python %smain.py %s" path keywords))))

