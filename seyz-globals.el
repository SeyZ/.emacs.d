;; ------ Global variables ------

(setq my-vendor-emacs-dir (concat user-emacs-directory "/vendor/"))
(setq my-lang-emacs-dir (concat user-emacs-directory "/lang/"))
(setq my-snippets-emacs-dir (concat user-emacs-directory "/snippets/"))

;; ------ Stuff ------

(setq initial-major-mode 'text-mode)
(setq default-major-mode 'text-mode)

(auto-compression-mode 1)
(setq make-backup-files nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq x-select-enable-clipboard t)      ; Enable the X clipboard

(when window-system
  (global-unset-key "\C-z"))            ; Disable the C-z shortcut

;; ------ Delete <TAB> and trailing whitespaces ------
(add-hook 'before-save-hook
          '(lambda ()
             (delete-trailing-whitespace)
             (untabify (point-min) (point-max))))
