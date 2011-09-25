;; ------ All top-level subdirs of user-emacs-directory to the path ------
(progn (cd user-emacs-directory)
       (normal-top-level-add-subdirs-to-load-path))

;; ------ All third party libraries ------
(add-to-list 'load-path my-vendor-emacs-dir)
(progn (cd my-vendor-emacs-dir)
       (normal-top-level-add-subdirs-to-load-path))
