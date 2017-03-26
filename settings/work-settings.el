;; work settings

(setq exec-path
      (append exec-path '("C:\\WinPython-64bit-3.6.0.1Qt5\\python-3.6.0.amd64\\" 
			  "C:\\WinPython-64bit-3.6.0.1Qt5\\python-3.6.0.amd64\\Scripts\\"
			  "C:\\WinPython-64bit-3.6.0.1Qt5\\python-3.6.0.amd64\\lib\\site-packages\\"
			  "C:\\PortableGit\\cmd\\"
			  "C:\\PortableGit\\bin\\"
			  )))

(setenv "PATH" (concat (getenv "PATH")
		       ";C:\\WinPython-64bit-3.6.0.1Qt5\\python-3.6.0.amd64\\"
		       ";C:\\WinPython-64bit-3.6.0.1Qt5\\python-3.6.0.amd64\\Scripts\\"
		       ";C:\\WinPython-64bit-3.6.0.1Qt5\\python-3.6.0.amd64\\lib\\site-packages\\"
		       ";C:\\PortableGit\\cmd\\"
		       ";C:\\PortableGit\\bin\\"
		       ))

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
