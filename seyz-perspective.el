(require 'perspective)

;; ------ Enable perspective mode globally ------
(persp-mode)

;; ------ Shortcuts ------
(global-set-key (kbd "C-z C-v") 'persp-switch)
(global-set-key (kbd "C-z v") 'persp-switch-quick)
(global-set-key (kbd "C-z k") 'persp-kill)
