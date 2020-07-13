;; org-mode settings

(require 'org)

(setq org-archive-location "~/org/archive/archive.org::")
(setq-default org-catch-invisible-edits 'error) ;; disallow org-mode invisble edits

;; setup agenda-files
(if (equal system-type 'windows-nt)
    (setq dropbox-dir (expand-file-name "~/../../Dropbox/")
          boxsync-dir (expand-file-name "~/../../Box Sync/")))

(if (memq window-system '(mac ns))
    (setq dropbox-dir (expand-file-name "~/Dropbox/")))

(setq
 org-agenda-files
 (list
  (file-name-as-directory (concat dropbox-dir "org" ))
  (file-name-as-directory (concat boxsync-dir "org" ))
  (expand-file-name "~/org")
  )
 )

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6)))) ;; change max depth of org-refile

;; org-agenda tweaks
(setq org-agenda-use-time-grid nil) ; disable time grid in org-agenda



(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODOs"
         ((agenda "" (
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-entry-types '(:scheduled :deadline)
                      )))
         (alltodo "" ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'todo 'done))) )
        )))) ; in combined agenda: only show scheduled items in week view and non-scheduled in todo list

;; set keybindings
(evil-define-key '(normal visual) org-mode-map
  (kbd "g h") 'org-up-element
  (kbd "C-c a") 'org-agenda
  )

;; ensure that MS Office documents don't open in emacs
(defun my-org-open-at-point () (interactive) 
       (let* ((path (org-element-property :path (org-element-context)))
              (contains-office-doc
               (or (string-match-p (regexp-quote ".pptx") path)
                   (string-match-p (regexp-quote ".ppt") path)
                   (string-match-p (regexp-quote ".doc") path)
                   (string-match-p (regexp-quote ".docx") path)
                   (string-match-p (regexp-quote ".xls") path)
                   (string-match-p (regexp-quote ".xlsx") path))))
         (if contains-office-doc
             (org-open-file path 'system)
           (org-open-at-point)))
       )

(define-key org-mode-map (kbd "C-c C-o") 'my-org-open-at-point)
