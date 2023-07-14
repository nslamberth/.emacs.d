;; settings for using evil
;; todo: set up use-package to only load these when evil is activated

(evil-define-key '(visual insert) global-map (kbd "C-g") 'evil-normal-state)
(evil-define-key '(visual insert) global-map (kbd "TAB") 'evil-normal-state)
(evil-define-key '(visual normal motion) global-map (kbd "F") 'avy-goto-char-timer)
(evil-define-key '(visual normal insert motion) global-map (kbd "C-n") 'hippie-expand)
(evil-define-key '(visual normal insert motion) global-map (kbd "M-/") 'hippie-expand)
(define-key evil-motion-state-map (kbd "M-.") nil)
(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-visual-state-map (kbd "M-.") nil)
(define-key evil-motion-state-map (kbd "TAB") nil)
(define-key evil-normal-state-map (kbd "TAB") nil)
(evil-define-key '(visual normal motion) global-map (kbd "[") 'scroll-down-command)
(evil-define-key '(visual normal motion) global-map (kbd "]") 'scroll-up-command)
(evil-define-key '(visual normal motion) global-map (kbd "{") 'scroll-other-window-down)
(evil-define-key '(visual normal motion) global-map (kbd "}") 'scroll-other-window)
(evil-define-key '(visual normal motion) global-map (kbd ")") 'evil-forward-paragraph)
(evil-define-key '(visual normal motion) global-map (kbd "(") 'evil-backward-paragraph)

;; map space bar to C-x in evil normal state and RET to M-x
;; from https://emacs.stackexchange.com/questions/48540/emacs-evil-mode-c-c-c-x-to-cc-cx-key-translation
(defun my/c-x ()
  (interactive)
  (setq uFnread-command-events (listify-key-sequence (kbd "C-x"))))

(defun my/m-x ()
  (interactive)
  (setq unread-command-events (listify-key-sequence (kbd "M-x"))))

(evil-define-key '(normal visual motion) global-map (kbd "SPC") 'my/c-x)
(evil-define-key '(normal visual motion) global-map (kbd "RET") 'my/m-x)
(evil-define-key 'normal global-map (kbd "S-<end>") 'evil-append-line)
(evil-define-key 'normal global-map (kbd "S-<home>") 'evil-insert-line)

(evil-define-key '(normal visual) org-mode-map
  (kbd "g h") 'org-up-element
  (kbd "C-c a") 'org-agenda
  (kbd "<prior>") 'org-move-subtree-up
  (kbd "<next>") 'org-move-subtree-down
  )
