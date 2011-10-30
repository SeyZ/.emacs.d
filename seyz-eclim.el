;; ------ Eclim ------
(require 'eclim)
(setq eclim-auto-save t)
(eclim-mode t)

;; ------ Company mode ------
(require 'company)
(require 'company-emacs-eclim)
(setf company-backends nil)
(company-emacs-eclim-setup)
(company-mode t)

(message "TESSTTTTTTTTTTTTTT")

;; ------ Shortcuts -----
(local-set-key (kbd "C-c i") 'eclim-java-format)
(local-set-key (kbd "M-RET") 'company-complete)
