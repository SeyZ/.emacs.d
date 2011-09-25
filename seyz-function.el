;; ----------------------------------------------------------------------------
;; Private functions
;; ----------------------------------------------------------------------------

(defun seyz--kill-buffers (list)
  "kill buffers in the list"
  (mapc 'kill-buffer list))

;; ----------------------------------------------------------------------------
;; Public functions
;; ----------------------------------------------------------------------------

;; ------ Indent whole buffer ------
(defun indent-whole-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  (message "Buffer indented"))

;; ------ Revert all buffers ------
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
	 (buffer (car list)))
    (while buffer
      (when (and (buffer-file-name buffer) 
		 (not (buffer-modified-p buffer)))
	(set-buffer buffer)
	(revert-buffer t t t))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "Refreshed open files"))

;; ------ Kill all buffers ------
(defun kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (seyz--kill-buffers (buffer-list))
  (message "All buffers are killed"))

;; ------ Kill other buffers ------
(defun kill-other-buffers ()
  "Kill other buffers"
  (interactive)
  (seyz--kill-buffers (delq (current-buffer) (buffer-list)))
  (message "All other buffers are killed"))