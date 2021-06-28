(setq auto-save-buffers-enhanced-interval 30)
(setq auto-save-buffers-enhanced-save-scratch-buffer-to-file-p t)
(setq auto-save-buffers-enhanced-file-related-with-scratch-buffer
      (concat howm-directory "scratch.txt"))
(auto-save-buffers-enhanced t)
(global-set-key "\C-xas" 'auto-save-buffers-enhanced-toggle-activity)
