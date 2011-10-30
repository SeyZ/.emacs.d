;; ------ Configuration ------
(add-hook 'java-mode-hook (lambda()
                            (subword-mode)
                            (load-library "seyz-eclim")))

;; ------ Indentation ------
(setq c-basic-offset 2)

;; Fixes the indentation inside an anonymous class
(c-set-offset 'substatement-open 0)
(if (assoc 'inexpr-class c-offsets-alist)
    (c-set-offset 'inexpr-class 0))

;; Deals with java annotations
(setq c-comment-start-regexp
      "\\(@\\|/\\(/\\|[*][*]?\\)\\)")
(modify-syntax-entry ?@ "< b"
                     java-mode-syntax-table)

;; ------ Highlight the FIXME/TODO/BUG keywords ------
(font-lock-add-keywords nil
                        '(("\\<\\(FIXME\\|TODO\\|BUG\\)"
                           1 font-lock-warning-face t)))
