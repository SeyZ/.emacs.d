(setq my-snippets-dir (concat my-vendor-emacs-dir "yasnippet/snippets/text-mode/"))

(require 'yas-jit)
(setq yas/root-directory my-snippets-dir)
(yas/jit-load)

(yas/global-mode)
