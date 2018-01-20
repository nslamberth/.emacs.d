;; keybind-settings

;; make M-o and S-o cycle windows
(global-set-key (kbd "M-o")      'other-window)
(global-set-key (kbd "C-\\")      'other-window)
(global-set-key (kbd "M-i")      '(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-|")      'other-window)

;; make 'a' go to end of line and 'e' evaluate in info-mode
(define-key Info-mode-map (kbd "a") 'move-end-of-line) 
(define-key Info-mode-map (kbd "e") 'eval-last-sexp) 

;; evaluation keybindings

(global-set-key (kbd "M-e") 'eval-last-sexp)
(global-set-key (kbd "C-<return>") 'eval-last-sexp)
(global-set-key (kbd "M-f") 'eval-defun)
(global-set-key (kbd "C-S-<return>") 'eval-defun)

;; buffer-list key commands
(define-key Buffer-menu-mode-map (kbd "r") 'revert-buffer)

;; make window commands act normally on mac
(global-set-key (kbd "M-w") 'delete-frame)
(global-set-key (kbd "M-n") 'new-frame)

(add-hook 'shell-mode-hook
	  '(lambda ()  ; remove conflict for shell-mode
	     (define-key shell-mode-map (kbd "M-n") nil)))

; set M-` to cyle frames (like OSX change application window)
(global-set-key (kbd "M-`") (lambda () (interactive) (other-frame 1)))
