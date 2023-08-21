;; NICK LAMBERTH EMACS INIT

;;; General Setup

;; remove unneeded tool bars
(menu-bar-mode -1) 
(if (boundp 'tool-bar-mode) (tool-bar-mode -1)) 
(if (boundp 'scroll-bar-mode) (scroll-bar-mode -1)) 

;; clean up custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory)) ; hide custom.el garbage
(load custom-file)

;; quality of life minor modes
(global-visual-line-mode t) ; get rid of weird truncation of line
(winner-mode t) ; used for winner-undo
(electric-pair-mode t) ; balance parens
(setq show-paren-delay 0) (show-paren-mode 1) ;; enable show-paren-mode
(desktop-save-mode 1) ; recover buffers on crash/restart
(repeat-mode 1)
(setq shift-select-mode nil) ; allows for finer movemnt control

;; qualify of life variables
(setq process-adaptive-read-buffering nil) ; make comint and eshell print output as received
(setq help-window-select t); select help window after running describe commands
(define-key Buffer-menu-mode-map (kbd "g") nil) ;; disable revert-buffer in buffer-menu
(fset 'yes-or-no-p 'y-or-n-p) ;; Always use y/n instead of yes/no
(put 'narrow-to-region 'disabled nil) ; enable narrowing
(setq inhibit-startup-screen t)
(setq-default org-catch-invisible-edits 'error) ;; disallow org-mode invisble edits
(setq view-read-only t) ; enable view mode for read only files
(setq auto-revert-verbose nil) ; stop the "reverting buffer modeline messages"

;; use ibuffer as default buffer list
(global-set-key [remap list-buffers] 'ibuffer)

;; smooth scrolling
;; from https://www.emacswiki.org/emacs/SmoothScrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; store backups in their own directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;; always highlight current set of parentheses
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))

(if (boundp 'highlight-parentheses-mode)
    (global-highlight-parentheses-mode t))

;; dired settings
(put 'dired-find-alternate-file 'disabled nil) ; enable dired alternate file
(setq dired-dwim-target t) ; enable split-window copying
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode t)
            (auto-revert-mode 1)
            ))
(add-hook 'wdired-mode-hook
          (lambda ()
            (dired-hide-details-mode t)
            (auto-revert-mode 1)
            ))

;; os-specific settings
(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))
(if (equal system-type 'windows-nt)
    (load "windows-settings"))
(if (memq window-system '(mac ns))
    (load "mac-settings"))
(if (equal system-type 'gnu/linux)
    (load "linux-settings"))

;;; package setup
(setq package--init-file-ensured t) ; disables package init.el silliness

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  )

(eval-when-compile
  (require 'use-package))

(use-package try
  :ensure t)

(use-package undo-tree
 :ensure t
 :init
 (global-undo-tree-mode)
 (setq undo-tree-auto-save-history nil)
 )

(use-package which-key
 :ensure t
 :init
 (setq which-key-idle-delay 0.5)
 (which-key-mode t)
 )

(use-package olivetti
 :ensure t
 :defer t
 :config
 (olivetti-set-width 90)
)

(use-package avy
 :ensure t
 :init
 (setq avy-all-windows t)
)

(use-package ace-window
 :ensure t
)

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (keymap-set vertico-map "RET" #'vertico-directory-enter)
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
  (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
)

(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)))

(use-package consult
  :ensure t)

(use-package yasnippet
 :ensure t
 :init
 (yas-global-mode 1)
 (push 'yas-hippie-try-expand hippie-expand-try-functions-list)
 )

(use-package yasnippet-snippets
 :ensure t
 :init
 )

(use-package magit
  :ensure t)

(use-package dumb-jump
  :ensure t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  )

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-S-c C-S-c" . 'mc/edit-lines)
  ("M-m" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this)
  ("C-c C-<" . 'mc/mark-all-like-this))

(use-package anaconda-mode
  :ensure t
  :defer t)

;;; load custom commands
(load (expand-file-name "custom_commands.el" user-emacs-directory))

;;; Keybindings
(global-set-key (kbd "<select>") 'end-of-line)
(global-set-key (kbd "C-e") 'end-of-line)
(global-set-key (kbd "C-x O") #'(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-\\") 'other-window)
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-j") 'avy-goto-char-timer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)
(global-set-key (kbd "M-o") 'my/new-line)
(global-set-key (kbd "M-l") 'my/mark-line)
(global-set-key (kbd "<deletechar>") 'backward-kill-word)
(global-set-key (kbd "S-<delete>") 'backward-kill-sexp)
(global-set-key (kbd "M-<right>") 'forward-list)
(global-set-key (kbd "M-<left>") 'backward-list)
(global-set-key (kbd "C-<right>") 'forward-sexp)
(global-set-key (kbd "C-<left>") 'backward-sexp)
(global-set-key (kbd "C-S-<up>") 'up-list)
(global-set-key (kbd "C-S-<down>") 'my/down-list)
(global-set-key (kbd "M-D") 'kill-sexp)
(global-set-key (kbd "M-F") 'forward-sexp)
(global-set-key (kbd "M-B") 'backward-sexp)
(global-set-key (kbd "M-V") 'mark-sexp)
(global-set-key (kbd "M-T") 'transpose-sexps)
(global-set-key (kbd "C-x )") 'kmacro-end-or-call-macro)
(global-set-key (kbd "C-x j") 'dired-jump)

;; eww-mode keybindings
(add-hook
 'eww-mode-hook
 #'(lambda ()
    (define-key eww-mode-map (kbd "[") 'scroll-down-command)
    (define-key eww-mode-map (kbd "]") 'scroll-up-command)
    (define-key eww-mode-map (kbd "{") 'scroll-other-window-down)
    (define-key eww-mode-map (kbd "}") 'scroll-other-window)))

;; org-mode keybindings
(add-hook
 'org-mode-hook
 #'(lambda ()
    (define-key org-mode-map (kbd "<next>") 'org-metadown)
    (define-key org-mode-map (kbd "<prior>") 'org-metaup)))

;; python-mode keybindings
(add-hook 'python-mode-hook
	  #'(lambda ()
	     (define-key python-mode-map (kbd "M-e") 'python-nav-forward-block)
	     (anaconda-mode 1)))


;; enable browse-url on wsl
;; from https://hungyi.net/posts/browse-emacs-urls-wsl/
(when (and (eq system-type 'gnu/linux)
           (string-match
            "Linux.*Microsoft.*Linux"
            (shell-command-to-string "uname -a")))
  (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'browse-url-generic))
