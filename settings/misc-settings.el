;; misc-settings

;; activate web-mode
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; bind tab in web-mode to fold/unfold in normal/visual
(add-hook 'web-mode-hook
	  '(lambda ()
	     (define-key evil-normal-state-map (kbd "TAB") 'web-mode-fold-or-unfold)
	     (define-key evil-visual-state-map (kbd "TAB") 'web-mode-fold-or-unfold))) 


;; cider-mode settings
(add-hook 'cider-mode-hook (lambda ()
			     (define-key cider-mode-map (kbd "M-e") 'cider-eval-last-sexp)
			     (add-hook 'cider-popup-buffer-mode-hook 'evil-motion-state)
			     ))
