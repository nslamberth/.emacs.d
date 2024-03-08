(defun my/previous-window ()
  "Cycle one window backwards"
  (interactive)
  (call-interactively 'other-window t (vector 1)))

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

(defun my/kill-region-or-line (&optional arg)
  "if region is active, kill region otherwise kill line"
  (interactive "P")
  (if (region-active-p)
      (call-interactively 'kill-region)
    (kill-line arg)))

(defun my/copy-region-or-line (&optional arg)
  "if region is active, copy region otherwise copy line"
  (interactive "P")
  (if (region-active-p)
      (call-interactively 'kill-ring-save)
    (save-excursion
      (call-interactively 'beginning-of-line)
      (call-interactively 'set-mark-command)
      (call-interactively 'end-of-line)
      (call-interactively 'forward-char)
      (call-interactively 'kill-ring-save))))


(defun my/kill-region-or-whole-line (&optional arg)
  "if region is active, copy region otherwise kill whole line"
  (interactive "P")
  (if (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively 'kill-whole-line)))

(defun my/copy-region-to-windows-clipboard (beg end)
  "copy region to windows clipboard in WSL"
  (interactive "r")
  (let* ((region-contents (buffer-substring beg end))
         (region-contents (string-replace "{env}" "prod" region-contents))
         (region-contents (string-replace "{run_dt}" "2024-01-15" region-contents))
         (region-contents (string-replace "{batch_dt}" "2024-01-15" region-contents)))
    (shell-command (format "echo %s | clip.exe" (shell-quote-argument region-contents)))))


(defvar my/last-repeatable-command nil
  "Copy of last-repeatable-command that ignores my/repeat-commands-to-ignore")

(defvar my/repeat-commands-to-ignore
  '(my/repeat next-line end-of-line left-char right-char
	      previous-line beginning-of-line beginning-of-visual-line)
  "List of commands for my/last-repeatable-command to ignore."
  )

(defun my/repeat (repeat-arg)
  "Modified version of repeat that ignore commands in my/repeat-commands-to-ignore."
  (interactive "P")
  (when (null repeat-arg)
    (setq repeat-arg last-prefix-arg))
  (let '(last-repeatable-command my/last-repeatable-command)
    (call-interactively 'repeat t (vector repeat-arg))))

(defun my/save-last-repeatable-command ()
  (if (not (member last-repeatable-command my/repeat-commands-to-ignore))
      (setq my/last-repeatable-command last-repeatable-command)))

(defun my/send-buffer-file-to-databricks ()
  "send the current region to databricks cli and return result in temp buffer"
  (interactive)
  (shell-command (format "dbsqlcli -e %s" (buffer-file-name))))
