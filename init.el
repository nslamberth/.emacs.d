;;; NICK LAMBERTH EMACS INIT

;;; package setup
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

;; load datorama.el
(load-file "~/.emacs.d/my-packages/datorama.el")

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

;;; eshell

;; custom functions

(defun eshell/cls ()
  "Clear all contents of eshell buffer."
  (let ((eshell-buffer-maximum-lines 0))
    (eshell-truncate-buffer)))

;; todo add all of the commands from code/clitools

;; add git stuff

; note! need to bake in credentials to url when using MS Windows
; check this link: https://github.com/atom/atom/issues/8984#issuecomment-144697558

;;; quality of life stuff

;; smooth scrolling
;; from https://www.emacswiki.org/emacs/SmoothScrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 1) ;; keyboard scroll one line at a time

;; remove menu and tool bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; make escape behave normally
(global-set-key (kbd "<escape>")      'keyboard-escape-quit) 

;; make M-o cycle windows
(global-set-key (kbd "M-o")      nil)
(global-set-key (kbd "M-o")      'other-window)

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

;; make quit-window kill the buffer
;; from http://superuser.com/questions/397806/emacs-modify-quit-window-to-delete-buffer-not-just-bury-it
(defun quit-window () 
 "modified quit window"
 (interactive)
 (kill-buffer-and-window)
 )

;; disable revert-buffer in buffer-menu
(define-key Buffer-menu-mode-map (kbd "g") nil)

;; Always use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; disallow org-mode invisble edits
(setq-default org-catch-invisible-edits 'error)

;; enable show-paren-mode
(setq show-paren-delay 0)
(show-paren-mode 1)

;; advise next-buffer to skip unnecessary buffers
(setq buffers-to-skip
     '("*Messages*"
       "*Backtrace*"
       "*Buffer List*"
       "*Completions*"
       "*Help*"
       "*Scratch*"
       "*Warnings*"
       "*helm M-x*"
       ))

(defadvice evil-next-buffer (after avoid-messages-buffer-in-next-buffer)
  "Advice around `evil-next-buffer' to skip buffers in buffers-to-skip."
  (when (member (buffer-name) buffers-to-skip)
    (evil-next-buffer)))

(defadvice evil-prev-buffer (after avoid-messages-buffer-in-prev-buffer)
  "Advice around `evil-prev-buffer' to skip buffers in buffers-to-skip."
  (when (member (buffer-name) buffers-to-skip)
    (evil-prev-buffer)))

(ad-activate 'evil-next-buffer)
(ad-activate 'evil-prev-buffer)

;; global key commands

(global-set-key (kbd "M-e") 'eval-last-sexp)


;; buffer-list key commands
(define-key Buffer-menu-mode-map (kbd "r") 'revert-buffer)

;;; python enviornment setup
;;; from: https://realpython.com/blog/python/emacs-the-best-python-editor/
;; enable elpy
(elpy-enable)

;; set flymake to wait a bit longer before checking
(setq flymake-no-changes-timeout 3)

;; enable quickhelp for elpy
(add-hook 'elpy-mode-hook 'company-quickhelp-mode)

;; enable autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; set m-l to eval region or buffer
(add-hook 'elpy-mode-hook
	  '(lambda ()
	     (define-key elpy-mode-map (kbd "M-l") 'elpy-shell-send-region-or-buffer)))

;; suppress annoying ad-handle-definition warnings
(setq ad-redefinition-action 'accept)

;; activate ein
(require 'ein)

;; ein key-commands
(add-hook 'ein:notebook-mode-hook
	  (lambda ()
	    (define-key ein:notebook-multilang-mode-map (kbd "M-e") 'ein:worksheet-execute-cell)
	    (define-key ein:notebook-multilang-mode-map (kbd "C-e") 'ein:worksheet-execute-cell)
	    (define-key ein:notebook-multilang-mode-map (kbd "C-<return>") 'ein:worksheet-execute-cell)))

;;; evil config
;; change some evil keybindings
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

(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-visual-state-map (kbd "M-.") nil)
(define-key evil-motion-state-map (kbd "M-.") nil)

(define-key evil-normal-state-map (kbd "M-o") nil)
(define-key evil-visual-state-map (kbd "M-o") nil)
(define-key evil-motion-state-map (kbd "M-o") nil)
(define-key evil-org-mode-map (kbd "M-o") nil)

;; make evil undo behave more like vim
(setq evil-want-fine-undo t)

;; disable annoying command window
(defun evil-command-window-ex (&optional current-command)
  (interactive)
  (message "command window has been disabled"))

;; stop emacs from copying region to clipboard
(fset 'evil-visual-update-x-selection 'ignore)

;; add extra ex commands
(evil-ex-define-cmd "df" 'delete-frame)
(evil-ex-define-cmd "nf" 'new-frame)
(evil-ex-define-cmd "ei" '(lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(evil-ex-define-cmd "en" '(lambda () (interactive) (find-file "~/notes.org")))
(evil-ex-define-cmd "et" '(lambda () (interactive) (find-file "~/todos.org")))
(evil-ex-define-cmd "ms" 'magit-status)

;;; org-mode settings
;; change max depth of org-refile
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

;; add todos to org-agenda-files
(setq org-agenda-files
      '("~/todos.org")
      )

;;; magit settings
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

;; enable magit man pages
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


;;; web-mode settings
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

;; bind tab in web-mode to fold/unfold in normal/visual
(add-hook 'web-mode-hook
	  '(lambda ()
	     (define-key evil-normal-state-map (kbd "TAB") 'web-mode-fold-or-unfold)
	     (define-key evil-visual-state-map (kbd "TAB") 'web-mode-fold-or-unfold))) 

;;; os-specific stuff
;; windows settings
(if (equal system-type 'windows-nt)
    (progn (setq exec-path
		 (append exec-path '("C:\\WinPython-64bit-2.7.6.4\\python-2.7.6.amd64\\" 
				     "C:\\WinPython-64bit-2.7.6.4\\python-2.7.6.amd64\\Scripts\\"
				     "C:\\WinPython-64bit-2.7.6.4\\python-2.7.6.amd64\\lib\\site-packages\\"
				     "C:\\PortableGit\\cmd\\"
				     "C:\\Users\\nlambert1\\Desktop\\cmder\\bin\\"
				     "C:\\clojure-1.8.0\\"
				     "C:\\Program Files (x86)\\Java\\jre7\\bin\\"
				     )))

	   (setenv "PATH" (concat (getenv "PATH")
				  ";C:\\WinPython-64bit-2.7.6.4\\python-2.7.6.amd64\\"
				  ";C:\\WinPython-64bit-2.7.6.4\\python-2.7.6.amd64\\Scripts\\"
				  ";C:\\WinPython-64bit-2.7.6.4\\python-2.7.6.amd64\\lib\\site-packages\\"
				  ";C:\\PortableGit\\cmd\\"
				  ";C:\\Users\\nlambert1\\Desktop\\cmder\\bin\\"
				  ";C:\\clojure-1.8.0\\"
				  ";C:\\Program Files (x86)\\Java\\jre7\\bin\\"
				  ))
	   ))

;; mac settings
(when (memq window-system '(mac ns))
  (progn (exec-path-from-shell-initialize)
	 (load-theme 'tsdh-dark))
	 )

;;; auto-generated stuff from custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "5bcd0c26bad3303c0325d12dd6562e4f7892d39d390d7db194dd141ba971cad7" default)))
 '(inhibit-startup-screen t)
 '(org-M-RET-may-split-line nil)
 '(org-startup-indented t)
 '(org-startup-truncated t)
 '(package-selected-packages
   (quote
    (company-quickhelp ein cider jedi py-autopep8 flycheck elpy web-mode monokai-theme magit helm hackernews evil-visual-mark-mode evil-org evil-leader elm-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; finally, open init, notes and todo
(find-file "~/notes.org")
(find-file "~/.emacs.d/init.el")
(find-file "~/todos.org")