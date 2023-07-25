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

;; open up some general files to have around
(find-file "~/.emacs.d/init.el")
(find-file "~")

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

(use-package evil-surround
  :ensure t
  :init
  )

(setq evil-disable-insert-state-bindings t)
(use-package evil
 :ensure t
 :config
 (evil-set-undo-system 'undo-tree)
 (global-evil-surround-mode 1)
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

(use-package smex
  :ensure t
  )

(use-package counsel
 :ensure t
 :init
 (ivy-mode 1)
 (counsel-mode 1)
 (global-set-key "\C-s" 'swiper)
)

(use-package ivy-rich
 :ensure t
 :init
 (ivy-rich-mode t)
 )

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

(use-package expand-region
  :ensure t
  :bind ("M-2" . 'er/expand-region)
  )

(use-package magit :ensure t)

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

;;; Keybindings
(global-set-key (kbd "C-\\") 'other-window)
(global-set-key (kbd "C-|") #'(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "C-x w") 'save-buffer)
(global-set-key (kbd "C-x B") 'list-buffers)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-e") 'eval-last-sexp)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-j") 'avy-goto-char-timer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-o") #'(lambda () (interactive) (end-of-line) (default-indent-new-line)))
(global-set-key (kbd "C-a") 'back-to-indentation)
(global-set-key (kbd "M-D") 'backward-kill-word)
(global-set-key (kbd "M-T") #'( () (interactive) (transpose-words -1)))
(global-set-key (kbd "C-<right>") 'forward-sexp)
(global-set-key (kbd "C-<left>") 'backward-sexp)
(global-set-key (kbd "M-<right>") 'forward-list)
(global-set-key (kbd "M-<left>") 'backward-list)
(global-set-key (kbd "C-S-<up>") 'up-list)
(global-set-key (kbd "C-S-<down>") 'down-list)

;; eww-mode keybindings
(add-hook 'eww-mode-hook '(lambda ()
    (define-key eww-mode-map (kbd "[") 'scroll-down-command)
    (define-key eww-mode-map (kbd "]") 'scroll-up-command)
    (define-key eww-mode-map (kbd "{") 'scroll-other-window-down)
    (define-key eww-mode-map (kbd "}") 'scroll-other-window)
    ))
