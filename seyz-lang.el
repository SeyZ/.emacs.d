;; Languages

;; ------ C mode (common) ------
(load-library "seyz-c-common")

;; ------ Java ------
(load-library "seyz-java")

;; ------ Javascript ------
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
