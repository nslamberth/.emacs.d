;; package-settings

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(setq package-enable-at-startup nil)

(package-initialize)

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

;; enable yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; enable elmacro 
(require 'elmacro)
(elmacro-mode)
