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
