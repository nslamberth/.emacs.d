(defun my/new-line ()
  "insert new line below current line and move point to beginnign of new line"
  (interactive)
  (end-of-line)
  (default-indent-new-line))

(defun my/mark-line (arg)
  "Select the current line and move the cursor by ARG lines IF
no region is selected.

If a region is already selected when calling this command, only move
the cursor by ARG lines.

From
https://emacs.stackexchange.com/questions/15033/how-to-mark-current-line-and-move-cursor-to-next-line
"
  (interactive "p")
  (when (not (use-region-p))
    (forward-line 0)
    (set-mark-command nil))
  (forward-line arg))

(defun my/down-list (&optional arg interactive)
  "modified down-list that treats strings as a list"
  (interactive "^p\nd")
  (cond
   ((equal (list-at-point) (sexp-at-point)) (down-list arg nil))
   ((save-excursion
      (progn	 
	(forward-sexp)
	(forward-char 2)
	(in-string-p))) 
    (progn (forward-sexp) (forward-char 2)))
   (t (down-list arg nil))))

(defun my/kill-word (arg)
  (interactive "p")
  (if (< arg 0) (backward-kill-word (- arg)) (kill-word arg)))
