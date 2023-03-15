(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js-mode-hook 'editorconfig-apply)
(with-eval-after-load 'js2-mode
  (electric-indent-local-mode 0)
  (define-key js2-mode-map (kbd "RET") 'js2-line-break))
