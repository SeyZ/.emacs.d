;; ----------------------------------------------------------------------------
;; Uses the eclim/java-complete to fill the auto-complete suggestions
;; with yasnippet support
;; ----------------------------------------------------------------------------

(defvar ac-eclim-yas-mark 'nil)

(defun ac-eclim-yas-candidates ()
  (let (lista)
    (with-no-warnings
      (when eclim-auto-save (save-buffer))
      (loop for c in (eclim/java-complete)
            do
            (if (not (equal (nth 0 c) "f"))
                (push (nth 1 c) lista)
              (let ((texto (nth 2 c))
                    aux)
                (push (concat
                       (replace-regexp-in-string "^\\([^(]*(\\).*" "\\1" texto)
                       (replace-regexp-in-string "[^ <]+\\(?:<[^>]+>\\)? \\([^ ,]+\\)\\(,?\\)" "${\\1}\\2"
                                                 (replace-regexp-in-string "^[^(]*(\\([^)]*\\)).*$" "\\1"  texto))
                       ")")
                      lista)))))
    lista))

(defun yas/expand-eclim-snippet ()
  (let ((snip (buffer-substring-no-properties ac-eclim-yas-mark (point))))
    (delete-region ac-eclim-yas-mark (point))
    (yas/expand-snippet snip)))

(defun eclim-yas-init ()
  (setq ac-eclim-yas-mark (point)))

(defvar ac-source-eclim
  '((candidates . ac-eclim-yas-candidates)
    (init . eclim-yas-init)
    (requires . 0)
    (prefix . c-dot)
    (action . yas/expand-eclim-snippet)
    (symbol . "f")))

(setf ac-sources nil)
(setq ac-sources '(ac-source-eclim))
