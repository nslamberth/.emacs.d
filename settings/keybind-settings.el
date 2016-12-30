;; keybind-settings

;; make M-o and S-o cycle windows
(global-set-key (kbd "M-o")      nil)
(global-set-key (kbd "M-o")      'other-window)
(global-set-key (kbd "M-i")      '(lambda () (interactive) (other-window -1)))

(global-set-key (kbd "s-o")      nil)
(global-set-key (kbd "s-o")      'other-window)
(global-set-key (kbd "s-i")      '(lambda () (interactive) (other-window -1)))

;; make 'a' go to end of line and 'e' evaluate in info-mode
(define-key Info-mode-map (kbd "a") 'move-end-of-line) 
(define-key Info-mode-map (kbd "e") 'eval-last-sexp) 

;; evaluation keybindings

(global-set-key (kbd "M-e") 'eval-last-sexp)
(global-set-key (kbd "s-e") 'eval-last-sexp)
(global-set-key (kbd "M-f") 'eval-defun)
(global-set-key (kbd "s-f") 'eval-defun)

;; buffer-list key commands
(define-key Buffer-menu-mode-map (kbd "r") 'revert-buffer)


