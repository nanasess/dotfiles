(with-eval-after-load-feature 'typescript-mode
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook #'lsp-deferred)
  (with-eval-after-load-feature 'editorconfig
    (add-hook 'typescript-mode-hook #'editorconfig-mode-apply)))
