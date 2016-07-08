;; windows-only settings
(if (equal system-type 'windows-nt)
    (progn (setq exec-path (append exec-path '("C:/WinPython-64bit-2.7.6.4/python-2.7.6.amd64"))) ; from http://winpython.sourceforge.net/
	   (setq exec-path (append exec-path '("C:/WinPython-64bit-2.7.6.4/python-2.7.6.amd64/Scripts"))) ; from http://winpython.sourceforge.net/
	   (setq exec-path (append exec-path '("C:/PortableGit/cmd"))))) ; from https://git-scm.com/download/win

;; mac-only settings
(when (memq window-system '(mac ns))
  (progn (exec-path-from-shell-initialize)
	 (load-theme 'tsdh-dark)))

;; suppress annoying ad-handle-definition warnings
(setq ad-redefinition-action 'accept)

;; package manager config options
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

;; set up sml mode
(autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)
(autoload 'run-sml "sml-proc" "Run an inferior SML process." t)
(setenv "PATH" (concat "/usr/local/bin/sml:" (getenv "PATH")))
(setq exec-path (cons "/usr/local/bin/sml"  exec-path))

;; enable evil
(require 'evil)
(evil-mode t)

;; enable evil-org
(require 'evil-org)
(evil-org-mode t)

;; enable hackrnews
(require 'hackernews)

;;require and activate helm
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)

;; remove menu and tool bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; evil config
(define-key evil-normal-state-map (kbd "SPC") 'evil-ex)
(define-key evil-visual-state-map (kbd "SPC") 'evil-ex)
(define-key evil-motion-state-map (kbd "SPC") 'evil-ex)

(define-key evil-normal-state-map (kbd "<return>") 'helm-M-x)
(define-key evil-visual-state-map (kbd "<return>") 'helm-M-x)

(define-key evil-normal-state-map (kbd "C-n") 'evil-next-buffer) 
(define-key evil-visual-state-map (kbd "C-n") 'evil-next-buffer) 
(define-key evil-motion-state-map (kbd "C-n") 'evil-next-buffer) 

(define-key evil-normal-state-map (kbd "C-p") 'evil-prev-buffer) 
(define-key evil-visual-state-map (kbd "C-p") 'evil-prev-buffer) 
(define-key evil-motion-state-map (kbd "C-p") 'evil-prev-buffer) 

(define-key evil-normal-state-map (kbd "C-e") 'eval-last-sexp) 
(define-key evil-visual-state-map (kbd "C-e") 'eval-last-sexp)
(define-key evil-motion-state-map (kbd "C-e") 'eval-last-sexp)
(define-key evil-insert-state-map (kbd "C-e") 'eval-last-sexp)

(fset 'evil-visual-update-x-selection 'ignore)

;; extra ex commands
(evil-ex-define-cmd "df" 'delete-frame)
(evil-ex-define-cmd "nf" 'new-frame)
(evil-ex-define-cmd "ei" '(lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(evil-ex-define-cmd "en" '(lambda () (interactive) (find-file "~/notes.org")))
(evil-ex-define-cmd "et" '(lambda () (interactive) (find-file "~/todos.org")))
(evil-ex-define-cmd "ms" 'magit-status)

;; make escape behave as normal
(global-set-key (kbd "<escape>")      'keyboard-escape-quit) 

;; make C-j cycle windows (and remove org conflict)
(global-set-key (kbd "C-j")      'other-window) 
(define-key org-mode-map (kbd "C-j") nil) 

;; make 'a' go to end of line and 'e' evaluate in info-mode
(define-key Info-mode-map (kbd "a") 'move-end-of-line) 
(define-key Info-mode-map (kbd "e") 'eval-last-sexp) 

;; store backups in their own directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;; Removes *messages* from the buffer list
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; make quit-window kill the buffer
;; from http://superuser.com/questions/397806/emacs-modify-quit-window-to-delete-buffer-not-just-bury-it
(defun quit-window () 
 "modified quit window"
 (interactive)
 (kill-buffer-and-window)
 )

;; set python-mode to use ipython
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

;; disable tab in python buffer (acts strangely)
(defun disable-tab-in-python-shell ()
  (local-unset-key (kbd "<tab>")))
(add-hook 'inferior-python-mode-hook
          'disable-tab-in-python-shell)

;; disable electric-indent in python buffer
(defun disable-electric-indent-in-python-shell ()
  (electric-indent-local-mode -1))
(add-hook 'inferior-python-mode-hook
          'disable-electric-indent-in-python-shell)

;; Always use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; disallow org-mode invisble edits
(setq-default org-catch-invisible-edits 'error)

;; make magit keymap more vim-like
(with-eval-after-load 'magit
    (define-key magit-status-mode-map (kbd "j") 'evil-next-line)
    (define-key magit-status-mode-map (kbd "k") 'evil-previous-line)
    (define-key magit-status-mode-map (kbd "f") 'evil-find-char)
    (define-key magit-status-mode-map (kbd ":") 'evil-ex)
    (define-key magit-process-mode-map (kbd ":") 'evil-ex)
    (define-key magit-status-mode-map (kbd ";") 'evil-repeat-find-char)
    (define-key magit-status-mode-map (kbd "/") 'evil-search-forward)
    (define-key magit-status-mode-map (kbd "0") 'evil-beginning-of-line)
    (define-key magit-status-mode-map (kbd "$") 'evil-end-of-line)
    (define-key magit-status-mode-map (kbd "C-j") 'other-window)
    (define-key magit-file-section-map (kbd "C-j") 'other-window)
    (define-key magit-hunk-section-map (kbd "C-j") 'other-window))

;; bind tab in web-mode to fold/unfold in normal/visual
(define-key evil-normal-state-map (kbd "TAB") 'web-mode-fold-or-unfold)
(define-key evil-visual-state-map (kbd "TAB") 'web-mode-fold-or-unfold)

;; activate web-mode
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; for magit use
(defadvice Info-follow-nearest-node (around gitman activate)
"When encountering a cross reference to the `gitman' info
manual, then instead of following that cross reference show
the actual manpage using the function `man'."
(let ((node (Info-get-token
	(point) "\\*note[ \n\t]+"
	"\\*note[ \n\t]+\\([^:]*\\):\\(:\\|[ \n\t]*(\\)?")))
    (if (and node (string-match "^(gitman)\\(.+\\)" node))
(progn (require 'man)
	(man (match-string 1 node)))
    ad-do-it)))

;; enable show-paren-mode
(setq show-paren-delay 0)
(setq show-paren-mode t)

;; auto-generated stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("5bcd0c26bad3303c0325d12dd6562e4f7892d39d390d7db194dd141ba971cad7" default)))
 '(evil-want-fine-undo nil)
 '(inhibit-startup-screen t)
 '(org-M-RET-may-split-line nil)
 '(org-startup-indented t)
 '(org-startup-truncated t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; open todo and notes
(find-file "~/notes.org")
(find-file "~/todos.org")
