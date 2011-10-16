(setq skeleton-pair t)
(setq skeleton-pair-alist
      '((?\( _ ?\))
        (?[  _ ?])
        (?{  _ ?})
        (?\" _ ?\")
        (?\' _ ?\')))

(defun autopair-insert (arg)
  (interactive "P")
  (let (pair)
    (cond
     ((assq last-command-char skeleton-pair-alist)
      (autopair-open arg))
     (t
      (autopair-close arg)))))

(defun autopair-open (arg)
  (interactive "P")
  (let ((pair (assq last-command-char
                    skeleton-pair-alist)))
    (cond
     ((and (not mark-active)
           (eq (car pair) (car (last pair)))
           (eq (car pair) (char-after)))
      (autopair-close arg))
     (t
      (skeleton-pair-insert-maybe arg)))))

(defun autopair-close (arg)
  (interactive "P")
  (cond
   (mark-active
    (let (pair open)
      (dolist (pair skeleton-pair-alist)
        (when (eq last-command-char (car (last pair)))
          (setq open (car pair))))
      (setq last-command-char open)
      (skeleton-pair-insert-maybe arg)))
   ((looking-at
     (concat "[ \t\n]*"
             (regexp-quote (string last-command-char))))
    (replace-match (string last-command-char))
    (indent-according-to-mode))
   (t
    (self-insert-command (prefix-numeric-value arg))
    (indent-according-to-mode))))

(defadvice delete-backward-char (before autopair activate)
  (when (and (char-after)
             (eq this-command 'delete-backward-char)
             (eq (char-after)
                 (car (last (assq (char-before) skeleton-pair-alist)))))
    (delete-char 1)))

(global-set-key "("  'autopair-insert)
(global-set-key ")"  'autopair-insert)
(global-set-key "["  'autopair-insert)
(global-set-key "]"  'autopair-insert)
(global-set-key "{"  'autopair-insert)
(global-set-key "}"  'autopair-insert)
(global-set-key "\"" 'autopair-insert)
(global-set-key "'"  'autopair-insert)
