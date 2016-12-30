;; work settings

(setq exec-path
      (append exec-path '("C:\\WinPython-64bit-2.7.6.4\\python-2.7.6.amd64\\" 
			  "C:\\WinPython-64bit-2.7.6.4\\python-2.7.6.amd64\\Scripts\\"
			  "C:\\WinPython-64bit-2.7.6.4\\python-2.7.6.amd64\\lib\\site-packages\\"
			  "C:\\PortableGit\\cmd\\"
			  "C:\\PortableGit\\bin\\"
			  "C:\\Users\\nlambert1\\Desktop\\cmder\\bin\\"
			  "C:\\clojure-1.8.0\\"
			  "C:\\Program Files (x86)\\Java\\jre7\\bin\\"
			  "C:\\pgsql\\bin"
			  )))

(setenv "PATH" (concat (getenv "PATH")
		       ";C:\\WinPython-64bit-2.7.6.4\\python-2.7.6.amd64\\"
		       ";C:\\WinPython-64bit-2.7.6.4\\python-2.7.6.amd64\\Scripts\\"
		       ";C:\\WinPython-64bit-2.7.6.4\\python-2.7.6.amd64\\lib\\site-packages\\"
		       ";C:\\PortableGit\\cmd\\"
		       ";C:\\Users\\nlambert1\\Desktop\\cmder\\bin\\"
		       ";C:\\clojure-1.8.0\\"
		       ";C:\\Program Files (x86)\\Java\\jre7\\bin\\"
		       ";C:\\pgsql\\bin"
		       ))

;; load datorama.el
(load-file "~/.emacs.d/my-packages/datorama.el")
