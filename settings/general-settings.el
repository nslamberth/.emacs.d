;;; general settings

;; enable smartparens-mode
(smartparens-global-mode t)
(require 'smartparens-config)

(define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

;; disable DISTRIB key command
(global-set-key (kbd "C-h C-o") 'describe-symbol)

;; enable global-visual-line-mode
(global-visual-line-mode t)

;; enable projectile
(require 'projectile)
(require 'helm-projectile)
(projectile-global-mode)
(helm-projectile-on)
(setq projectile-indexing-method 'alien)

;; enable which-key
(require 'which-key)
(which-key-mode)

;; enable highlight-sexp
(require 'highlight-sexp)
(add-hook 'lisp-mode-hook 'highlight-sexp-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-sexp-mode)

;; smooth scrolling
;; from https://www.emacswiki.org/emacs/SmoothScrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 1) ;; keyboard scroll one line at a time

;; make comint and eshell print output as received
(setq process-adaptive-read-buffering nil)

;; make escape behave normally
(global-set-key (kbd "<escape>")      'keyboard-escape-quit) 

;; store backups in their own directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;; disable revert-buffer in buffer-menu
(define-key Buffer-menu-mode-map (kbd "g") nil)

;; Always use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; enable show-paren-mode
(setq show-paren-delay 0)
(show-paren-mode 1)

;; always highlight current set of parentheses
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))

(if (boundp 'highlight-parentheses-mode)
    (global-highlight-parentheses-mode t))

;; next-buffer and previous-buffer advice
(setq buffers-to-skip
     '("*Messages*"
       "*Backtrace*"
       "*Buffer List*"
       "*Completions*"
       "*Help*"
       "*Scratch*"
       "*Warnings*"
       "*helm M-x*"
       "*Flycheck error messages*"
       "*company-documentation*"
       ))

(setq buffers-to-use-normal-state
     '("*eshell*" "*Python*"
       ))

(defadvice evil-next-buffer (after avoid-messages-buffer-in-next-buffer)
  "Advise next-buffer to skip admin buffers and set eshell to normal-state"
  (when (member (buffer-name) buffers-to-skip)
    (evil-next-buffer))
  (when (member (buffer-name) buffers-to-use-normal-state)
    (evil-normal-state)))

(defadvice evil-prev-buffer (after avoid-messages-buffer-in-prev-buffer)
  "Advise prev-buffer to skip admin buffers and set eshell to normal-state"
  (when (member (buffer-name) buffers-to-skip)
    (evil-prev-buffer))
  (when (member (buffer-name) buffers-to-use-normal-state)
    (evil-normal-state)))

(ad-activate 'evil-next-buffer)
(ad-activate 'evil-prev-buffer)

; dired settings
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode t) (evil-motion-state)))

; enable narrowing
(put 'narrow-to-region 'disabled nil)

; some commands for saving/loading window state
(defvar window-memory nil)

(defun save-window ()
  "Save current window configration to window memory."
  (interactive)
  (setq window-memory (current-window-configuration)))

(defun restore-window ()
  "Restore window configuration to state saved in window memory."
  (interactive)
  (if window-memory
      (set-window-configuration window-memory)
    (message "No window configuration saved.")))

; convnience functions 
(defun open-current-directory ()
  "Open explorer/finder at point."
  (interactive)
    (if (equal system-type 'windows-nt)
	(shell-command "explorer .")
      (shell-command "open ."))
  )
(defalias 'ocd 'open-current-directory)

(defun change-dir-to-desktop ()
  "Change working directory to desktop."
  (cd desktop)
  )
(defalias 'cdd 'change-dir-to-desktop) 

(defun upgrade-praw()
  "Upgrade praw to latest version"
  (interactive)
    (if (equal system-type 'windows-nt)
	(async-shell-command "pip install --upgrade praw")
      (async-shell-command "sudo -H pip3 install --upgrade praw"))
  )
