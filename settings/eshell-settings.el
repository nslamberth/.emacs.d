;; eshell-settings

(defun eshell/cls ()
  "Clear all contents of eshell buffer."
  (let ((eshell-buffer-maximum-lines 0))
    (eshell-truncate-buffer)))
