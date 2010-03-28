;; C-z で最小化しない
;(defun iconify-or-deiconify-frame ()
;  "Iconify the selected frame, or deiconify if it's currently an icon."
;  (interactive)
;  (if (eq (cdr (assq 'visibility (frame-parameters))) t)
;      (iconify-frame)
;    (make-frame-visible)))

;; -------------------------- multi-TTY Settings ---------------------
(server-start)

(provide 'berkeley-unix-init)
