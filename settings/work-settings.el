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
