 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-verbose nil)
 '(byte-compile-warnings nil)
 '(cape-dabbrev-check-other-buffers nil)
 '(cape-dabbrev-min-length 2)
 '(completion-auto-select 'second-tab)
 '(custom-enabled-themes '(tsdh-dark))
 '(delete-selection-mode t)
 '(desktop-save-mode t)
 '(dired-dwim-target 'dired-dwim-target-next)
 '(duplicate-line-final-position -1)
 '(electric-pair-mode t)
 '(embark-verbose-indicator-display-action '(display-buffer-at-bottom))
 '(global-undo-tree-mode t)
 '(help-window-select t)
 '(hippie-expand-try-functions-list
   '(try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol try-expand-line))
 '(hledger-currency-string "$")
 '(ignored-local-variable-values
   '((Package . HUNCHENTOOT)
     (Base . 10)
     (Package . CL-WHO)
     (Syntax . COMMON-LISP)))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(isearch-wrap-pause 'no-ding)
 '(ledger-reports
   '(("bank and credit card register" "ledger [[ledger-mode-flags]] -f /home/nicklamberth/Sync/projects/ledger/main.ledger -p \"last month\" reg \\(assets or liabilities\\) and not \\(loans or house or 401k or vanguardira\\) -S date")
     ("balance income and expenses" "ledger [[ledger-mode-flags]] -f /home/nicklamberth/Sync/projects/ledger/main.ledger bal income expenses loans -p \"this month\"")
     ("bank+credit card balance" "ledger [[ledger-mode-flags]] -f /home/nicklamberth/Sync/projects/ledger/main.ledger  -e \"tomorrow\" bal \\(assets or liabilities\\) and not \\(loans or house or 401k or vanguardira\\)")
     ("visa register" "ledger [[ledger-mode-flags]] -f /home/nicklamberth/Sync/projects/ledger/main.ledger reg visa")
     ("asset register" "ledger [[ledger-mode-flags]] -f /home/nicklamberth/Sync/projects/ledger/main.ledger reg assets")
     ("credit register" "ledger [[ledger-mode-flags]] -f /home/nicklamberth/Sync/projects/ledger/main.ledger reg visa")
     ("asset register" "ledger [[ledger-mode-flags]] -f /home/nicklamberth/Sync/projects/ledger/main.ledger reg assets")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(line-move-visual nil)
 '(magit-branch-read-upstream-first 'fallback)
 '(mc/always-run-for-all t)
 '(menu-bar-mode nil)
 '(next-line-add-newlines nil)
 '(orderless-matching-styles '(orderless-regexp orderless-literal orderless-prefixes))
 '(org-agenda-files '("/home/nick/org/notes.org"))
 '(org-agenda-sticky t)
 '(org-capture-templates
   '(("j" "default capture" entry
      (file "~/Sync/org/Capture.org")
      "")))
 '(org-fold-catch-invisible-edits 'smart)
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-use-outline-path t)
 '(org-startup-truncated nil)
 '(org-use-speed-commands t)
 '(package-selected-packages
   '(cape corfu-terminal corfu quelpa iedit ledger-mode embark-consult embark sql-indent request goto-chg org-jira drag-stuff markdown-mode yaml-mode jump-char puni expand-region consult ace-window avy try orderless vertico multiple-cursors yasnippet-snippets which-key use-package undo-tree olivetti magit dumb-jump))
 '(repeat-mode t)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(set-mark-command-repeat-pop t)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-trailing-whitespace nil)
 '(tab-always-indent t)
 '(tab-first-completion 'eol)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(truncate-lines nil)
 '(truncate-partial-width-windows nil)
 '(undo-tree-auto-save-history nil)
 '(undo-tree-enable-undo-in-region nil)
 '(view-read-only t)
 '(window-combination-resize t)
 '(winner-mode t)
 '(word-wrap t)
 '(yas-indent-line 'fixed))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completions-annotations ((t (:inherit shadow))))
 '(ledger-font-xact-highlight-face ((t (:inherit nil :extend t)))))
