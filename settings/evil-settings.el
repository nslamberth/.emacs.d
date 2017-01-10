;; evil-settings

;; make evil undo behave more like vim
(setq evil-want-fine-undo t)

;; motion-state for package.el
(add-hook 'package-menu-mode-hook 'evil-motion-state)

;; change "-" and "_" to be part of word objects
(modify-syntax-entry ?_ "w")
(modify-syntax-entry ?- "w")
;; potential better solution?
;; (setq-default evil-symbol-word-search 'symbol)
;; from comments of http://emacsredux.com/blog/2014/08/27/a-peek-at-emacs-24-dot-4-superword-mode/

;; disable annoying command window
(defun evil-command-window-ex (&optional current-command)
  (interactive)
  (message "command window has been disabled"))

;; stop emacs from copying region to clipboard
(fset 'evil-visual-update-x-selection 'ignore)

;; add extra ex commands
(evil-ex-define-cmd "df" 'delete-frame)
(evil-ex-define-cmd "nf" 'new-frame)
(evil-ex-define-cmd "ei" '(lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(evil-ex-define-cmd "en" '(lambda () (interactive) (find-file "~/notes.org")))
(evil-ex-define-cmd "et" '(lambda () (interactive) (find-file "~/todos.org")))
(evil-ex-define-cmd "esr" '(lambda () (interactive)(evil-buffer "*scratch*")))
(evil-ex-define-cmd "ms" 'magit-status)
(evil-ex-define-cmd "es" 'eshell)
(evil-ex-define-cmd "sw" 'save-window)
(evil-ex-define-cmd "rw" 'restore-window)



;; enable evil-surround
(require 'evil-surround)
(global-evil-surround-mode 1)

;; evil-specific keybindings
(define-key evil-normal-state-map (kbd "SPC") 'evil-ex)
(define-key evil-visual-state-map (kbd "SPC") 'evil-ex)
(define-key evil-motion-state-map (kbd "SPC") 'evil-ex)

(define-key evil-normal-state-map (kbd "<return>") 'helm-M-x)
(define-key evil-visual-state-map (kbd "<return>") 'helm-M-x)

(define-key evil-normal-state-map (kbd "C-n") 'evil-next-buffer) 
(define-key evil-visual-state-map (kbd "C-n") 'evil-next-buffer) 
(define-key evil-motion-state-map (kbd "C-n") 'evil-next-buffer) 

(define-key evil-normal-state-map (kbd "C-p") 'evil-prev-buffer) 
(define-key evil-visual-state-map (kbd "C-p") 'evil-prev-buffer) 
(define-key evil-motion-state-map (kbd "C-p") 'evil-prev-buffer) 

(define-key evil-normal-state-map (kbd "C-e") 'eval-last-sexp) 
(define-key evil-visual-state-map (kbd "C-e") 'eval-last-sexp)
(define-key evil-motion-state-map (kbd "C-e") 'eval-last-sexp)
(define-key evil-insert-state-map (kbd "C-e") 'eval-last-sexp)

(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-visual-state-map (kbd "M-.") nil)
(define-key evil-motion-state-map (kbd "M-.") nil)

(define-key evil-normal-state-map (kbd "M-o") nil)
(define-key evil-visual-state-map (kbd "M-o") nil)
(define-key evil-motion-state-map (kbd "M-o") nil)

(define-key evil-normal-state-map (kbd "M-l") 'org-insert-link)
(define-key evil-insert-state-map (kbd "M-l") 'org-insert-link)
(define-key evil-visual-state-map (kbd "M-l") 'org-insert-link)
(define-key evil-motion-state-map (kbd "M-l") 'org-insert-link)
