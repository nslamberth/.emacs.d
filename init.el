;;; NICK LAMBERTH EMACS INIT

;; set up initial path

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))

;; package-settings for setting up and activating packages
(load "package-settings")

;; general settings for basic quality-of-life improvements
(load "general-settings")

;; location-specifc settings for work and home differences
(if (equal system-type 'windows-nt)
    (load "work-settings"))

(if (memq window-system '(mac ns))
    (load "home-settings"))

;; eshell-settings for eshell config, aliases and functions
(load "eshell-settings")

;; keybind-settings for global keybinds
(load "keybind-settings")

;; python-settings for python environment setup
(load "python-settings")

;; evil-settings for evil config and keybindings
(load "evil-settings")

;; org-settings for org config and keybindings
(load "org-settings")

;; miscellaneous settings for experimentation and mode-tweaks
(load "misc-settings")

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
    (flycheck-pyflakes py-autopep8 evil-surround highlight-parentheses multiple-cursors yasnippet elmacro ob-ipython company-anaconda anaconda-mode company-quickhelp ein cider jedi flycheck elpy web-mode monokai-theme magit helm hackernews evil-visual-mark-mode evil-org evil-leader elm-mode)))
 '(python-shell-prompt-detect-enabled nil)
 '(warning-suppress-log-types (quote ((python)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; finally, open init, notes and todo
(find-file "~/notes.org")
(find-file "~/.emacs.d/init.el")
(find-file "~/todos.org")
(eshell)
(evil-normal-state)
