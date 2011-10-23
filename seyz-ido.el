(ido-mode t)                            ; Enable the ido mode globally
(setq ido-everywhere t)                 ; Enable ido everywhere
(setq ido-enable-flex-matching t)       ; Enable the flex matching
(setq tramp-mode nil)                   ; Disable the tramp mode

(setq ido-auto-merge-delay-time 0)      ; No delay to the complete feature

(define-key ido-file-dir-completion-map (kbd "C-c C-s")
  (lambda()
    (interactive)
    (ido-initiate-auto-merge (current-buffer))))
