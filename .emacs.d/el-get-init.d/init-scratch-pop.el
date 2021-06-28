(global-set-key (kbd "C-c c") 'scratch-pop)
(makunbound 'scratch-ext-minor-mode-map)
(define-minor-mode scratch-ext-minor-mode
  "Minor mode for *scratch* buffer."
  nil ""
  '(("\C-c\C-c" . scratch-pop-kill-ring-save-exit)
    ("\C-c\C-e" . erase-buffer)))

(with-current-buffer (get-buffer-create "*scratch*")
  (erase-buffer)
  (ignore-errors
    (insert-file-contents auto-save-buffers-enhanced-file-related-with-scratch-buffer))
  (setq header-line-format "scratch!!")
  (scratch-ext-minor-mode 1))
(defun scratch-pop-kill-ring-save-exit ()
  "Save after close the contents of buffer to killring."
  (interactive)
  (kill-new (buffer-string))
  (erase-buffer)
  (funcall (if (fboundp 'popwin:close-popup-window)
               'popwin:close-popup-window
             'quit-window)))
