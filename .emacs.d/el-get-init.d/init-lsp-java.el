(with-eval-after-load-feature 'cc-mode
  (add-hook 'java-mode-hook #'eglot-ensure))
