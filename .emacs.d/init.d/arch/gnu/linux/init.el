;;----------------------- window settings ---------------------------
;; sudo apt-get install xfonts-mplus
;; (if (display-graphic-p)
;;     (progn
;;             (create-fontset-from-ascii-font "M+ 1m-12:weight=normal:slant=normal" nil "linux")
;;             (set-fontset-font "fontset-linux" 'unicode
;;                               (font-spec :family "M+ 1m" ) nil 'append)
;;             (set-fontset-font "fontset-linux" '(#x0080 .  #x024F)
;;                               (font-spec :family "M+ 1m") nil 'prepend)
;;             (setq initial-frame-alist
;;                   (append (list
;;                            '(height . 32)
;;                            '(width  . 82)
;;                            '(font . "fontset-linux")
;;                            initial-frame-alist)))
;;             (setq default-frame-alist initial-frame-alist)))
(provide 'linux-init)
