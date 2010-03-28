;; input method settings
;(mac-input-method-mode 0)
(setq mac-pass-control-to-system nil)

;; font settings
(require 'carbon-font)
(fixed-width-set-fontset "hirakaku_w3" 12)

;; Window settings
(if (boundp 'window-system)
    (setq initial-frame-alist
          (append (list
                   '(font . "-*-*-medium-r-normal--12-*-*-*-*-*-fontset-hirakaku_w3")
                   '(width . 82)
                   '(height . 50)
                   )
                  initial-frame-alist)))
(setq default-frame-alist initial-frame-alist)

(provide 'mac-init)
