;; org-mode settings

;; disallow org-mode invisble edits
(setq-default org-catch-invisible-edits 'error)

;; disable M-o so other-window works
(mapc (lambda (state)
        (evil-define-key state evil-org-mode-map
          (kbd "M-o") nil
          ))
      '(normal insert))

;; change max depth of org-refile
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

;; add todos to org-agenda-files
(setq org-agenda-files
      '("~/todos.org")
      )
;; enable ob-ipython
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ipython . t)
   ;; other languages..
   ))

;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

;;don't prompt me to confirm everytime I want to evaluate a block
(setq org-confirm-babel-evaluate nil)


;; set M-e to evaluate
(mapc (lambda (state)
        (evil-define-key state evil-org-mode-map
          (kbd "M-e") 'org-ctrl-c-ctrl-c
          ))
      '(normal insert))

;; org-capture setup
(setq org-capture-templates
      '(("t" "Todo" entry (file "~/org/todos.org")
             "* TODO %?\n  %i")))

(defun org-capture-todo ()
  "insert todo into todos.org"
  (interactive)
  (org-capture nil "t"))

(define-key org-mode-map (kbd "S-<return>") 'org-capture-finalize)

; org-archive setup
(setq org-archive-location "~/org/archive/archive.org::")


