;; (setq my-snippets-dir (concat my-vendor-emacs-dir "yasnippet/snippets/text-mode/"))
(setq my-snippets-dir (concat my-vendor-emacs-dir "yasnippet/snippets/"))

(require 'yasnippet)
(yas/initialize)
(yas/load-directory my-snippets-dir)


;; (require 'yas-jit)
;; (setq yas/root-directory my-snippets-dir)
;; (yas/jit-load)

(yas/global-mode t)
