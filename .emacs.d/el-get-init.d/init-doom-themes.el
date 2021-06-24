(add-hook 'after-init-hook
          #'(lambda ()
              (require 'doom-themes)
              ;; use solarized.
              (load-theme 'doom-solarized-light t)))
