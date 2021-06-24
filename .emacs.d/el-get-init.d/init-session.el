(with-eval-after-load-feature 'session
  (setq session-save-print-spec '(t nil 40000)))
(add-hook 'after-init-hook 'session-initialize)
