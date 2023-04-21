(with-eval-after-load 'haskell-mode
  (setq haskell-stylish-on-save t)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))
