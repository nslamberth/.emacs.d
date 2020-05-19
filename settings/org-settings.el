;; org-mode settings

(setq org-archive-location "~/org/archive/archive.org::")
(setq-default org-catch-invisible-edits 'error) ;; disallow org-mode invisble edits
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6)))) ;; change max depth of org-refile

;; setup agenda-files
(if (equal system-type 'windows-nt)
    (setq dropbox-dir (expand-file-name "~/../../Dropbox/")))

(if (memq window-system '(mac ns))
    (setq dropbox-dir (expand-file-name "~/Dropbox/")))

(setq
 org-agenda-files
 (list
  (file-name-as-directory (concat dropbox-dir "org" ))
  (expand-file-name "~/org")
  )
 )

;; set keybindings
(evil-define-key '(normal visual) org-mode-map
  (kbd "g h") 'org-up-element
  (kbd "C-c a") 'org-agenda
  )

;; I think this is unnecessary, commenting out for now
;; Will delete next commit if nothing breaks...
;; (mapc (lambda (state)
        ;; (evil-define-key state evil-org-mode-map
          ;; (kbd "M-o") nil
          ;; ))
      ;; '(normal insert))
