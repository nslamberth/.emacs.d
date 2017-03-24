;;; NICK LAMBERTH EMACS INIT

;; clean up window bars early
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; set up initial path
(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; load work-specific path
(if (equal system-type 'windows-nt)
    (load "work-settings"))

;; package-settings for setting up and activating packages
(setq package--init-file-ensured t) ; disables package init.el silliness
(load "package-settings")

;; general settings for basic quality-of-life improvements
(load "general-settings")

;; home-specific settings
(if (memq window-system '(mac ns))
    (load "home-settings"))

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

;; miscellaneous settings for experimentation and mode-tweaks
(load "misc-settings")

;; finally, open init, notes and todo
(find-file "~/org/notes.org")
(find-file "~/.emacs.d/init.el")
(eshell)
(find-file "~/org/todos.org")
(evil-normal-state)
