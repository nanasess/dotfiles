(with-eval-after-load 'consult
  (with-eval-after-load 'embark
    (require 'embark-consult)
    (global-set-key (kbd "C-,") 'embark-act)))
