(add-hook 'emacs-startup-hook 'yas-global-mode)
(with-eval-after-load 'yasnippet
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil))
