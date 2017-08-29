;;----------------------- window settings ---------------------------
(if (boundp 'window-system)
    (create-fontset-from-ascii-font "Circle M+ 1m-12:weight=normal:slant=normal" nil "linux")
  (set-fontset-font "fontset-linux" 'unicode
		    (font-spec :family "Circle M+ 1m" ) nil 'append)
  (set-fontset-font "fontset-linux" '(#x0080 .  #x024F)
		    (font-spec :family "Circle M+ 1m") nil 'prepend)

  (setq initial-frame-alist
	(append (list
		 '(height . 50)
		 '(width  . 82)
		 '(font . "fontset-linux")
		 initial-frame-alist)))
  (setq default-frame-alist initial-frame-alist))

(provide 'linux-init)
