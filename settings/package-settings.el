;; package-settings
;; install and configure third-party packages

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")) ; removing due to slow server response time
(package-initialize)


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  )

(eval-when-compile
  (require 'use-package))

(use-package
 evil
 :ensure t
 :config (evil-mode t))

(use-package
 undo-tree
 :ensure t
 :defer
 )

(use-package
 evil-org
 :ensure t
 :config (evil-org-mode t)
 :defer
 )

(use-package
 hackernews
 :ensure t
 :config (require 'hackernews)
 :defer)

(use-package
 helm
 :ensure t
 :config (require 'helm-config)
 :bind (("M-x" . helm-M-x)))

(use-package
 elmacro
 :ensure t
 :config (require 'elmacro-mode)
 :defer)

(use-package
 yasnippet
 :ensure t
 ; :config (yas-global-mode 1)
 :defer t)

(use-package
  smartparens
  :ensure t
  :config (smartparens-global-mode t) (require 'smartparens-config)
  :bind (("C-<right>" . sp-forward-slurp-sexp)
         ("C-<left>" . sp-forward-barf-sexp)
         ("C-M-<left>" . sp-backward-slurp-sexp)
         ("C-M-<right>" . sp-backward-barf-sexp)))

(use-package
  projectile
  :ensure t
  :config (progn (require 'projectile)
                 (projectile-global-mode)
                 (setq projectile-indexing-method 'alien))
  :defer t)

(use-package
  helm-projectile
  :ensure t
  :config (require 'helm-projectile) (helm-projectile-on)
  )

(use-package
  which-key
  :ensure t
  :config (require 'which-key) (which-key-mode)
  :defer t)

(use-package
  pony-mode
  :ensure t
  :config (require 'pony-mode)
  :defer t)

(use-package
  evil-surround
  :ensure t
  :defer t)

(use-package
  elpy
  :ensure t
  :defer t)

(use-package
  py-autopep8
  :ensure t
  :defer t)

(use-package
  ob-ipython
  :ensure t
  :defer t)

(use-package
  web-mode
  :ensure t
  :defer t)
