(with-eval-after-load-feature 'typescript-mode
  (add-hook 'typescript-mode-hook #'lsp-deferred)
  (with-eval-after-load-feature 'editorconfig
    (add-hook 'typescript-mode-hook #'editorconfig-mode-apply)))
