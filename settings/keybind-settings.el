;; keybind-settings

;; add some shortcuts for window/buffer management
(global-set-key (kbd "M-o")      'other-window)
(global-set-key (kbd "C-\\")      'other-window)
(global-set-key (kbd "M-i")      '(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-|")      '(lambda () (interactive) (other-window -1)))
(global-set-key  (kbd "<M-home>") 'delete-other-windows)
(global-set-key  (kbd "<M-end>") 'evil-window-delete)
(global-set-key (kbd "M-w") 'delete-frame)
(global-set-key (kbd "M-n") 'new-frame)
(global-set-key  (kbd "<C-insert>") 'evil-write)
(global-set-key  (kbd "<C-delete>") 'evil-delete-buffer)
(global-set-key (kbd "C-<prior>")      'evil-prev-buffer)
(global-set-key (kbd "C-<next>")      'evil-next-buffer)
(global-set-key (kbd "C-S-e")      'open-current-directory)
(global-set-key (kbd "C-S-<end>")      'end-of-buffer-other-window)
(global-set-key (kbd "<f12>")      'evil-write)

(global-set-key
 (kbd "C-<pause>")
 '(lambda ()
    (interactive) (evil-window-vsplit) (other-window 1)))

(global-set-key
 (kbd "C-S-<pause>")
 '(lambda ()
    (interactive) (evil-window-split) (other-window 1)))

; some more ergonomic end-of-line keys
(evil-global-set-key 'normal (kbd "S-<end>") 'evil-append-line)
(evil-global-set-key 'normal (kbd "S-<home>") 'evil-insert-line)


;; make 'a' go to end of line and 'e' evaluate in info-mode
(define-key Info-mode-map (kbd "a") 'move-end-of-line) 
(define-key Info-mode-map (kbd "e") 'eval-last-sexp) 

;; evaluation keybindings
(global-set-key (kbd "M-e") 'eval-last-sexp)
(global-set-key (kbd "C-<return>") 'eval-last-sexp)
(global-set-key (kbd "M-f") 'eval-defun)
(global-set-key (kbd "C-S-<return>") 'eval-defun)
(global-set-key (kbd "<f5>") 'eval-buffer)

;; buffer-list key commands
(define-key Buffer-menu-mode-map (kbd "r") 'revert-buffer)

(add-hook 'shell-mode-hook
	  '(lambda ()  ; remove conflict for shell-mode
	     (define-key shell-mode-map (kbd "M-n") nil)))

; set M-` to cyle frames (like OSX change application window)
(global-set-key (kbd "M-`") (lambda () (interactive) (other-frame 1)))
