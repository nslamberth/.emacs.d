;; windows settings

(progn 
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
                    )))

;; windows-specific font and colorscheme
(set-face-attribute 'default nil :family "Consolas" :height 110)
(load-theme 'tsdh-dark)

;; fix some annoying python encoding problems
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")
(setenv "PYTHONIOENCODING" "utf-8")
