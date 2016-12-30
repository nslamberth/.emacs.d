;; eshell-settings

; custom functions

(defun eshell/cls ()
  "Clear all contents of eshell buffer."
  (let ((eshell-buffer-maximum-lines 0))
    (eshell-truncate-buffer)))

(defun eshell/ei ()
  "Open init.el file"
  (find-file "~/.emacs.d/init.el"))

(defun eshell/en ()
  "Open master notes file"
  (find-file "~/notes.org"))

(defun eshell/et ()
  "Open master todos file"
  (find-file "~/todos.org"))

(defun eshell/esr()
  "Open scratch buffer"
  (evil-buffer "*scratch*"))
