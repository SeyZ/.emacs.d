(require 'smex)

(global-set-key (kbd "M-x")  (lambda ()
			       (interactive)
			       (or (boundp 'smex-cache)
				   (smex-initialize))
			       (global-set-key [(meta x)] 'smex)
			       (smex)))

(global-set-key (kbd "S-M-x") (lambda ()
				(interactive)
				(or (boundp 'smex-cache)
				    (smex-initialize))
				(global-set-key [(meta x)] 'smex)
				(smex)))
