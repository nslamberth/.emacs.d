;; keybind-settings

;; make M-o and S-o cycle windows
(global-set-key (kbd "M-o")      'other-window)
(global-set-key (kbd "M-i")      '(lambda () (interactive) (other-window -1)))

;; make 'a' go to end of line and 'e' evaluate in info-mode
(define-key Info-mode-map (kbd "a") 'move-end-of-line) 
(define-key Info-mode-map (kbd "e") 'eval-last-sexp) 

;; evaluation keybindings

(global-set-key (kbd "M-e") 'eval-last-sexp)
(global-set-key (kbd "M-f") 'eval-defun)

;; buffer-list key commands
(define-key Buffer-menu-mode-map (kbd "r") 'revert-buffer)

;; make window commands act normally on mac
(global-set-key (kbd "M-w") 'delete-frame)
(global-set-key (kbd "M-n") 'new-frame)
(define-key shell-mode-map (kbd "M-n") nil) ; remove conflict for shell-mode
