(add-to-list 'load-path (concat user-emacs-directory ".mew.d"))
(load "mew-config" t t)
(setq mew-rc-file ".mew")
(with-eval-after-load-feature 'mew
  (require 'mm-version))
