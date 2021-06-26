(add-hook 'after-init-hook
          #'(lambda ()
              (require 'doom-themes)
              ;; use solarized.
              (load-theme 'doom-solarized-light t)
              (set-face-attribute 'font-lock-comment-face nil :slant 'normal)
              (set-face-attribute 'font-lock-type-face nil :slant 'normal :weight 'bold)
              (set-face-attribute 'font-lock-builtin-face nil :slant 'normal :weight 'bold)))
