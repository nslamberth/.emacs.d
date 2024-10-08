;; NICK LAMBERTH EMACS INIT

;;; General Setup

;; set up custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;; load local .el files
(load (expand-file-name "custom_commands.el" user-emacs-directory))

;; non-customizable variables
(setq process-adaptive-read-buffering nil) ; make comint and eshell print output as received
(fset 'yes-or-no-p 'y-or-n-p) ;; Always use y/n instead of yes/no
(put 'narrow-to-region 'disabled nil) ; enable narrowing
(put 'dired-find-alternate-file 'disabled nil) ; enable dired alternate file
(put 'downcase-region 'disabled nil) ; enable downcase from region


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
 :bind
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
  :ensure t
  :init
  (setf completion-in-region-function 'consult-completion-in-region)
  :bind
  ("M-S" . consult-line)
  ("M-s s" . consult-imenu))

(use-package yasnippet
 :ensure t
 :init
 (yas-global-mode 1))

(use-package yasnippet-snippets
 :ensure t
 :init)

(use-package magit
  :ensure t)

(use-package dumb-jump
  :ensure t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package multiple-cursors
  :ensure t
  :bind
  ("M-m" . 'mc/mark-next-like-this))

(use-package expand-region
  :ensure t
  :bind ("M-=" . er/expand-region))

(use-package puni
  ;; for now use only for slurp and barf
  :ensure t
  :bind
  ("C-S-<right>" . puni-slurp-forward)
  ("C-c s" . puni-slurp-forward)
  ("C-S-<left>" . puni-barf-forward)
  ("C-c b" . puni-barf-forward))

(use-package jump-char
  :ensure t
  :bind
  ("M-F" . jump-char-forward))

(use-package markdown-mode
  :ensure t)

(use-package drag-stuff
  :ensure t
  :init
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(use-package org-jira
  :ensure t
  :init
  (let ((org-jira-dir "~/.org-jira"))
	  (if (not (file-exists-p org-jira-dir))
		  (make-directory org-jira-dir)))
  (setq jiralib-url "https://wbdstreaming.atlassian.net/"))

(use-package goto-chg
  :ensure t
  :bind
  ("C-x ;" . goto-last-change)
  ("C-x ," . goto-last-change-reverse))

(use-package slime
  :ensure t
  :init
  (setq inferior-lisp-program "sbcl"))

(use-package marginalia :ensure t)

(use-package embark
  :ensure t

  :bind
  (("M-o" . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  (setq prefix-help-command #'embark-prefix-help-command)

  ;; try this later
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t)

(use-package ledger-mode
  :ensure t)

(use-package iedit
  :ensure t
  :bind
  (("C-\\" . iedit-mode)))

(use-package quelpa
  :ensure t)

(use-package corfu
:ensure t)

;; Non-Melpa packages

(quelpa '(corfu-terminal
          :fetcher git
          :url "https://codeberg.org/akib/emacs-corfu-terminal.git"))

(unless (display-graphic-p)
  (corfu-terminal-mode +1))

;;; Keybindings
(global-set-key (kbd "<select>") 'end-of-line)
(global-set-key (kbd "C-e") 'end-of-line)
(global-set-key (kbd "C-a") 'beginning-of-line)
(global-set-key (kbd "C-x O") 'my/previous-window)
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "C-k") 'my/kill-region-or-line)
(global-set-key (kbd "M-w") 'my/copy-region-or-line)
(global-set-key (kbd "C-w") 'my/kill-region-or-whole-line)
(global-set-key (kbd "M-j") 'avy-goto-char-timer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)
(global-set-key (kbd "M-RET") 'my/new-line)
(global-set-key (kbd "M-l") 'my/mark-line)
(global-set-key (kbd "M-D") 'duplicate-dwim)
(global-set-key (kbd "C-x )") 'kmacro-end-or-call-macro)
(global-set-key (kbd "C-x j") 'dired-jump)
(global-set-key (kbd "C-<delete>") 'kill-buffer-and-window)
(global-set-key (kbd "M-<end>") 'delete-window)
(global-set-key (kbd "M-<home>") 'delete-other-windows)
(global-set-key [remap repeat] 'my/repeat)
(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key (kbd "C-<right>") 'forward-same-syntax)
(define-key Buffer-menu-mode-map (kbd "g") nil)
(global-set-key (kbd "M-\"") 'surround-sexp-with-quotes)

;; dired hook
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

;; eww-mode hook
(add-hook
 'eww-mode-hook
 #'(lambda ()
    (define-key eww-mode-map (kbd "[") 'scroll-down-command)
    (define-key eww-mode-map (kbd "]") 'scroll-up-command)
    (define-key eww-mode-map (kbd "{") 'scroll-other-window-down)
    (define-key eww-mode-map (kbd "}") 'scroll-other-window)))

;; org-mode hook
(add-hook
 'org-mode-hook
 #'(lambda ()
    (define-key org-mode-map (kbd "C-c n") 'org-next-visible-heading)
    (define-key org-mode-map (kbd "C-c p") 'org-previous-visible-heading)
	(drag-stuff-mode -1)))

;; python-mode hook
(add-hook 'python-mode-hook
	  #'(lambda ()
	     (define-key python-mode-map (kbd "M-e") 'python-nav-forward-block)
	     (define-key python-mode-map (kbd "C-c r") 'python-shell-send-region)
	     (define-key python-mode-map (kbd "C-c RET") 'recompile)))

;; repeat-mode hook
(add-hook 'pre-command-hook
		  'my/save-last-repeatable-command)

;; wsl settings
(when (and (eq system-type 'gnu/linux)
           (string-match
            "Linux.*Microsoft.*Linux"
            (shell-command-to-string "uname -a")))
  ;; enable browse-url on wsl
  ;; from https://hungyi.net/posts/browse-emacs-urls-wsl/
  (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'browse-url-generic)
  (global-set-key (kbd "C-c w w") 'my/copy-region-to-windows-clipboard)
  (global-set-key (kbd "C-c d r") 'send-region-to-databricks)
  )

;; rpi settings
(when (and (eq system-type 'gnu/linux)
           (string-match
            "raspberrypi"
            (shell-command-to-string "uname -a")))
  (setq
   org-refile-targets '(("/home/nicklamberth/Sync/org/Notes.org" :maxlevel . 1)
						("/home/nicklamberth/Sync/org/Cooking.org" :maxlevel . 1)
						("/home/nicklamberth/Sync/org/Watch.org" :maxlevel . 1)
						("/home/nicklamberth/Sync/org/Thoughts.org" :maxlevel . 1)
						("/home/nicklamberth/Sync/org/Kids.org" :maxlevel . 1))))


; repeat maps
; based on template from
; https://tildegit.org/acdw/define-repeat-map.el
(defvar my-other-window-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "o" #'other-window)
    (define-key map "O" #'previous-window)
    (define-key map "0" #'delete-window)
    (define-key map "1" #'delete-other-windows)
    (define-key map "2" #'split-window-below)
    (define-key map "3" #'split-window-right)
    (define-key map "b" #'switch-to-buffer)
	(define-key map "f" #'find-file)
    map)
  "A map to repeat all window commands")

(dolist (command
         '(other-window
		   previous-window
		   delete-window
		   delete-other-windows
		   split-window-below
		   split-window-right
		   switch-to-buffer
		   find-file))
  (put command 'repeat-map 'my-other-window-repeat-map))

(defvar sentence-navigation-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "e" #'forward-sentence)
    (define-key map "a" #'backward-sentence)
    map))

(defvar list-navigation-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "u" #'backward-up-list)
    (define-key map "d" #'down-list)
    (define-key map "f" #'forward-sexp)
    (define-key map "b" #'backward-sexp)
    (define-key map "x" #'eval-defun)
    (define-key map (kbd "@") #'mark-sexp)
    (define-key map "k" #'kill-sexp)
    (define-key map "r" #'raise-sexp)
    (define-key map "a" #'backward-sentence)
    (define-key map "t" #'transpose-sexp)
    (define-key map "q" #'indent-pp-sexp)
    map)
  "repeat map for navigating sexps")

(dolist (command
         '(backward-up-list
           down-list
           forward-sexp
           backward-sexp
           mark-sexp
           kill-sexp
           raise-sexp
           beginning-of-defun
           end-of-defun
           transpose-sexp
           indent-pp-sexp))
  (put command 'repeat-map 'list-navigation-repeat-map))
