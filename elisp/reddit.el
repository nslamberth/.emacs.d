;; reddit.el
;; functions for browsing reddit

(setq reddit-py
      (expand-file-name  "~/.emacs.d/python/reddit.py"))

(setq python (if (equal system-type 'windows-nt) "python" "python3"))

(defvar reddit-history nil
  "History of cached reddit contents visisted. These are not refreshed live from the server.")


;; core functions

(defun no-scroll-filter (proc string)
  "Process filter that outputs to buffer without moving point." ; from elisp info "Filter-Functions"
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (save-excursion
	(let ((moving (= (point) (process-mark proc))))
	  ;; Insert the text, advancing the process marker.
	  (goto-char (process-mark proc))
	  (insert string)
	  (set-marker (process-mark proc) (point))
	  (if moving (goto-char (process-mark proc))))))))


(defun reddit-get-submissions (subreddit &optional order)
  "Go to comments for submission id at point. Order is new by default and hot with prefix arg."
  (interactive "ssubreddit:")
  (if current-prefix-arg
      (setq order "hot"))
  (with-current-buffer (get-buffer-create "reddit")
    (delete-region (point-min) (point-max))
    (let ((proc
	   (apply
	    ; use apply, mapcar, eval and remove to filter for optional arguments
	    'start-process "reddit" "reddit" python
	    (mapcar 'eval
		    (remove nil (list reddit-py
				      "-r" subreddit
				      (if order "--order")
				      (if order order)))))))
	  (set-process-sentinel proc #'ignore)
      (set-process-filter proc 'no-scroll-filter))
    (goto-char 0)
    (funcall 'reddit-mode))
  (switch-to-buffer "reddit"))

(defun reddit-get-comments ()
  "Get comments for submission id at point."
  (interactive)
  (push (cons (buffer-string) (point)) reddit-history)
  (let ((submission-id
	 (progn 
	   (forward-paragraph)
	   (previous-line)
	   (thing-at-point 'word))))
    (with-current-buffer (get-buffer-create "reddit")
      (let ((proc
	     (start-process "reddit" "reddit" python reddit-py "-c" submission-id)))
	(set-process-sentinel proc #'ignore)
	(set-process-filter proc 'no-scroll-filter))
      (delete-region (point-min) (point-max))
      (beginning-of-buffer)
      (funcall 'reddit-mode))
    (switch-to-buffer "reddit")
    (message (format "pulling comments for %s..." submission-id))))

(defun reddit-go-back ()
  "Replce current buffer with previous page's contents."
  (interactive)
  (if reddit-history
      (let ((prev-page (pop reddit-history)))
	(erase-buffer)
	(insert (car prev-page))
	(goto-char (cdr prev-page)))
    (message "No more pages in history."))
  (reddit-mode)
  )

(defun forward-reddit-item ()
  "Temporary hack to navigate reddit items."
  (interactive)
  (forward-paragraph)
  (next-line 2)
  (evil-scroll-line-to-center (line-number-at-pos))
  )

;; set up mode

(setq reddit-mode-map
      (let ((map (make-sparse-keymap)))
	(evil-add-hjkl-bindings map 'emacs)
	(evil-define-key 'normal map (kbd "g s") 'reddit-get-submissions)
	(evil-define-key 'normal map (kbd "d") 'reddit-get-comments)
	(evil-define-key 'normal map (kbd "u") 'reddit-go-back)
	(evil-define-key 'normal map (kbd "e") 'eww)
	(evil-define-key 'normal map (kbd "b") 'browse-url)
	(evil-define-key 'normal map (kbd "q") 'evil-delete-buffer)
	(evil-define-key 'normal map (kbd "<return>") 'helm-M-x)
	(evil-define-key 'normal map (kbd "<SPC>") 'evil-ex)
	(evil-define-key 'normal map (kbd "n") 'forward-reddit-item)
	(evil-define-key 'normal map (kbd "g g") 'beginning-of-buffer)
	map))


(define-minor-mode reddit-mode
  "Minor-mode for interacting with reddit.el"
  :lighter " reddit"
  :keymap reddit-mode-map
  (evil-normal-state)
  (if (not visual-line-mode)
      (visual-line-mode)
    )
  )

;; convenience functions

(defun reddit-politics-new ()
  (interactive)
  (reddit-get-submissions "politics" "new")
  )

(defun reddit-politics-hot ()
  (interactive)
  (reddit-get-submissions "politics+worldnews" "hot")
  )

(defun reddit-politics-rising ()
  (interactive)
  (reddit-get-submissions "politics" "rising")
  )

(defun reddit-sports ()
  (interactive)
  (reddit-get-submissions "nfl+nba+baseball" "hot")
  )

