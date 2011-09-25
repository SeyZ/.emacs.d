; ------ suppress startup message ------
(setq inhibit-startup-message t
      initial-scratch-message nil)

;; ------ Fullscreen ------
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

;; turn on the fullscreen
(run-with-idle-timer 0.1 nil 'toggle-fullscreen)

;; ------ Get rid of bars ------
(progn
  (menu-bar-mode)
  (tool-bar-mode)
  (scroll-bar-mode))

;; ------ Configure the cursor ------
(blink-cursor-mode -1)
(set-default 'cursor-type 'hbar)
(setq ring-bell-function 'ignore)

;; ------ Color theme ------
(autoload 'color-theme-zenburn "color-theme-zenburn" "Zenburn Color Theme" t)
(color-theme-zenburn)
