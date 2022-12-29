(with-eval-after-load-feature 'typescript-mode
  (setq typescript-indent-level 2)
  (with-eval-after-load-feature 'editorconfig
    (add-hook 'typescript-mode-hook #'editorconfig-mode-apply)))
