;; ------ Shortcuts ------

(add-hook 'hs-minor-mode-hook
	  (lambda () 
	    (local-set-key (kbd "M-+") 'toggle-hiding-all)
	    (local-set-key (kbd "M-=") 'toggle-hiding-block)
	    (local-set-key (kbd "C-=") 'hs-hide-level)))
