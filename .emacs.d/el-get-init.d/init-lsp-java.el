(with-eval-after-load-feature 'cc-mode
  (add-hook 'java-mode-hook #'company-backends-with-yas)
  (add-hook 'java-mode-hook #'lsp-deferred))
