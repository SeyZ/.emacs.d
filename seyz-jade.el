;; ------ Jade template engine ------
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . jade-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
