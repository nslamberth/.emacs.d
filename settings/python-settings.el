;; python-settings
;; heavily inspired by: https://realpython.com/blog/python/emacs-the-best-python-editor/

;; enable elpy
(elpy-enable)
(elpy-use-ipython)
(setq python-shell-interpreter-args "--simple-prompt")

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

;; set indent-tabs-mode to nil
(setq-default  indent-tabs-mode nil)

;; python version of eval-print-last-sexp
;; from http://stackoverflow.com/questions/43033022/is-there-a-function-like-eval-print-last-sexp-for-comint-mode
(defun python-eval-print-last-sexp ()
  "Print result of evaluating current line into current buffer."
  (interactive)
  (move-end-of-line nil)
  (let ((res (python-shell-send-string-no-output
              ;; modify to get a different sexp
              (buffer-substring (line-beginning-position)
                                (line-end-position))))
        (standard-output (current-buffer)))
    (when res
      (terpri)
      (princ res))))

;; set elpy hooks
(add-hook 'elpy-mode-hook
	  '(lambda ()
	     (define-key elpy-mode-map (kbd "M-l") 'elpy-shell-send-region-or-buffer) 
	     (define-key elpy-mode-map (kbd "M-e") 'elpy-shell-send-region-or-buffer)
	     (define-key elpy-mode-map (kbd "C-j") 'python-eval-print-last-sexp)
	     (define-key elpy-mode-map (kbd "C-c C-p") 'run-python) 
	     (define-key elpy-mode-map (kbd "C-h o") 'elpy-doc) 
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

;; python 3.6 ipython native completion fix
;; additional note: it worked!
;; if this doesn't work on Windows, make sure to pip install pyreadline

(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))
