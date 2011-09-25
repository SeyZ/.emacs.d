(autoload 'highlight-parentheses-mode "highlight-parentheses"
  "Highlight parentheses" t)

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))

(global-highlight-parentheses-mode t)
