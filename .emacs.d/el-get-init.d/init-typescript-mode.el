(with-eval-after-load 'typescript-mode
  (setq typescript-indent-level 2)
  (with-eval-after-load 'editorconfig
    (add-hook 'typescript-mode-hook #'editorconfig-mode-apply)))
