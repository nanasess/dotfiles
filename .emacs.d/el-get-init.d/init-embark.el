(global-set-key (kbd "C-,") 'embark-act)
(with-eval-after-load 'consult
  (with-eval-after-load 'embark
    (require 'embark-consult)
    (define-key embark-file-map "s" #'sudo-edit)))
