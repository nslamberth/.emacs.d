;;; NICK LAMBERTH EMACS INIT
(package-initialize)

;; clean up window bars early

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; set up initial path
(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(load "package-settings")
(load "general-settings")

;; load windows-specific settings
(if (equal system-type 'windows-nt)
    (load "windows-settings"))

;; load mac-specific settings
(if (memq window-system '(mac ns))
    (load "mac-settings"))

;; general settings for basic quality-of-life improvements
;; (load "general-settings")

;; package-settings for setting up and activating packages
(setq package--init-file-ensured t) ; disables package init.el silliness
;; (load "package-settings")

;; evil-settings for evil config and keybindings
(load "evil-settings")

;; eshell-settings for eshell config, aliases and functions
(load "eshell-settings")

;; keybind-settings for global keybinds
(load "keybind-settings")

;; python-settings for python environment setup
(load "python-settings")

;; org-settings for org config and keybindings
(load "org-settings")

;; elfeed settings for configuring RSS feeds
(load "elfeed-settings")

;; miscellaneous settings for experimentation and mode-tweaks
(load "misc-settings")

;; finally, open up eshell in .emacs.d
(cd "~/.emacs.d")
(eshell)
(kill-buffer "*scratch*")
