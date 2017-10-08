;; home settings

(exec-path-from-shell-initialize)
(load-theme 'tsdh-dark)

(setq mac-command-modifier 'meta)

; (load "../elisp/spotify.el")
(load "/Users/nick/projects/gmail_project/gmail.el")

(setq elpy-rpc-python-command "python3")

;; global variables
(setq desktop "~/Desktop")

;; misc commands

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
