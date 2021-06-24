(modify-coding-system-alist 'file "\\.\\(db\\|sqlite\\)\\'" 'raw-text-unix)
(add-to-list 'auto-mode-alist '("\\.\\(db\\|sqlite\\)\\'" . sqlite-dump))
