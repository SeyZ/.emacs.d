(require 'auto-complete-config)
(global-auto-complete-mode t)

(setq ac-sources '(ac-source-yasnippet
                   ac-source-abbrev
                   ac-source-dictionary
                   ac-source-words-in-same-mode-buffers))


;; ------ Shortcuts ------
;; (add-hook 'auto-complete-mode-hook
;;        (lambda ()
;;          (local-set-key (kbd "M-RET") 'auto-complete)))
