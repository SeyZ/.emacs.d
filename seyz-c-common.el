(defun autopair-close-block (arg)
  (interactive "P")
  (cond
   (mark-active
    (autopair-close arg))
   ((not (looking-back "^[[:space:]]*"))
    (newline-and-indent)
    (autopair-close arg))
   (t
    (autopair-close arg))))

(add-hook 'c-mode-common-hook
          '(lambda ()
             (local-set-key "(" 'autopair-insert)
             (local-set-key ")" 'autopair-insert)
             (local-set-key "{" 'autopair-insert)
             (local-set-key "}" 'autopair-close-block)))