(autoload 'magit-status "magit" nil t)

;; ------ Configurations ------
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "lime green")
     (set-face-foreground 'magit-diff-del "brown2")))

;; ------ Hooks ------

;; add the -w option to the diff command when the java-mode is used
(add-hook 'java-mode-hook (lambda () (setq magit-diff-options '("-w"))))

;; ------ Shortcuts ------
(global-set-key (kbd "C-c C-g s") 'magit-status)
