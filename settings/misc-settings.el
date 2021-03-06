;; misc-settings

;; load reddit.el
(load-file "~/.emacs.d/elisp/reddit.el")

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

;; enable "cls" clear screen for all comint modes
(defadvice comint-send-input (around comint-clear-if-cls)
  "If the input being sent is just 'cls', then run comint-clear-buffer."
  (if (equal "cls" (thing-at-point 'word))
      (progn
	(evil-delete-backward-word)
	(comint-clear-buffer))
    ad-do-it))

(ad-activate 'comint-send-input)

;; define weather-forecast function
(defun weather-forecast ()
  (interactive)
  (setq weather-dir (expand-file-name "~/.emacs.d/python"))

  (setq python
	(if (equal system-type 'darwin) "python3" "python"))

  (shell-command-to-string (concat python " " weather-dir "/weather.py")))

; *eww* mode settings
(evil-define-key 'normal eww-mode-map
  "q" 'evil-delete-buffer
  "u" 'eww-back-url
  "d" 'eww-follow-link
  )

(defadvice eww-back-url (around quit-if-no-history)
  "If there are no pages to go back to, kill buffer."
  (if (>= eww-history-position (length eww-history))
      (if (get-buffer "reddit")
	  (progn
	    (switch-to-buffer "reddit")
	    (kill-buffer "*eww*"))
	(call-interactively 'evil-delete-buffer))
    ad-do-it
    ))

(ad-activate 'eww-back-url)
