(create-fontset-from-ascii-font "Circle M+ 1m-12:weight=normal:slant=normal" nil "solaris")
(set-fontset-font "fontset-solaris" 'unicode
		  (font-spec :family "Circle M+ 1m" ) nil 'append)
(set-fontset-font "fontset-solaris" '(#x0080 .  #x024F)
		  (font-spec :family "Circle M+ 1m") nil 'prepend)

;; Window settings
(if (boundp 'window-system)
    (setq initial-frame-alist
	  (append (list
		   '(width . 82)
		   '(height . 50)
		   '(font . "fontset-solaris"))
		  initial-frame-alist)))
(setq default-frame-alist initial-frame-alist)

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'usg-unix-v-init)
