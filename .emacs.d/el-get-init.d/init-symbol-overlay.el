(with-eval-after-load-feature 'symbol-overlay
  (global-set-key (kbd "M-i") 'symbol-overlay-put)
  (global-set-key (kbd "<f7>") 'symbol-overlay-mode)
  (global-set-key (kbd "<f8>") 'symbol-overlay-remove-all))
