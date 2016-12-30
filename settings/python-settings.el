;; python-settings
;; heavily inspired by: https://realpython.com/blog/python/emacs-the-best-python-editor/

;; enable ipython
(require 'python)
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

;; hack command to fix annoying ipython magic issues on Windows
(defun ipython-get-docstring ()
  (interactive)
  (insert ")")
  (evil-normal-state)
  (evil-backward-WORD-begin nil)
  (evil-insert 1 nil nil)
  (insert "help(")
  (evil-normal-state)
  (evil-find-char nil 41)
  (evil-append 1 nil)
  (comint-send-input))

(when (equal python-shell-interpreter "ipython")
  (add-hook 'inferior-python-mode-hook
	    '(lambda () 
	       (evil-define-key 'insert inferior-python-mode-map (kbd "?") 'ipython-get-docstring))))

;; enable elpy
(elpy-enable)
(elpy-use-ipython)

;; reduce rpc buffer for windows (otherwise causes freezing)
(if (equal system-type 'windows-nt)
    (setq elpy-rpc-large-buffer-size 2094))


;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; make flycheck work only on save
(setq flycheck-check-syntax-automatically '(save mode-enabled))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; enable quickhelp for elpy
(add-hook 'elpy-mode-hook 'company-quickhelp-mode)

;; enable autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; set elpy hooks
(add-hook 'elpy-mode-hook
	  '(lambda ()
	     (define-key elpy-mode-map (kbd "M-l") 'elpy-shell-send-region-or-buffer) 
	     (define-key elpy-mode-map (kbd "M-e") 'elpy-shell-send-region-or-buffer)
	     (define-key elpy-mode-map (kbd "C-c C-p") 'run-python) ; personal preference for run-python
	     (define-key elpy-mode-map (kbd "C-h o") 'elpy-doc) ; make elpy-doc similar to emacs help
	     (setq eldoc-mode nil)
	     ))

;; suppress annoying ad-handle-definition warnings
(setq ad-redefinition-action 'accept)

;; activate ein
(require 'ein)

;; ein key-commands
(add-hook 'ein:notebook-mode-hook
	  (lambda ()
	    (define-key ein:notebook-multilang-mode-map (kbd "M-e") 'ein:worksheet-execute-cell)
	    (define-key ein:notebook-multilang-mode-map (kbd "C-e") 'ein:worksheet-execute-cell)
	    (define-key ein:notebook-multilang-mode-map (kbd "C-<return>") 'ein:worksheet-execute-cell)
	    (define-key ein:notebook-multilang-mode-map (kbd "M-<up>") 'ein:worksheet-goto-prev-input)
	    (define-key ein:notebook-multilang-mode-map (kbd "M-<down>") 'ein:worksheet-goto-next-input)))
