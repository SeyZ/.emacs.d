(add-to-list 'load-path "/home/seyz/.emacs.d/vendor/yasnippet/")

(require 'yas-jit)
(setq yas/root-directory "/home/seyz/.emacs.d/vendor/yasnippet/snippets/text-mode/")
(yas/jit-load)

(yas/global-mode)
