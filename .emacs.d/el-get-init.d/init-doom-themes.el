(add-hook
 'after-init-hook
 #'(lambda ()
     (require 'doom-themes)
     ;; use solarized.
     (load-theme 'doom-solarized-light t)
     (with-eval-after-load 'vertico
       (custom-set-faces
        `(vertico-group-title ((t (:foreground ,(doom-color 'base7)))))))
     (with-eval-after-load 'corfu
       (custom-set-faces
        `(corfu-annotations ((t (:foreground ,(doom-color 'green)))))))))
