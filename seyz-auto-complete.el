(require 'auto-complete-config)
(global-auto-complete-mode t)

;; ------ Shortcuts ------
(add-hook 'auto-complete-mode-hook
	  (lambda ()
	    (local-set-key (kbd "M-RET") 'auto-complete)))
