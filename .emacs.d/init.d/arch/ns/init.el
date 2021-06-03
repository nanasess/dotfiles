(when (require 'ucs-normalize nil t)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs)
  (prefer-coding-system 'utf-8-hfs))

;; font settings
(create-fontset-from-ascii-font "Menlo-12:weight=normal:slant=normal" nil "ns")
(set-fontset-font "fontset-ns" 'unicode
		  (font-spec :family "Hiragino Kaku Gothic ProN" ) nil 'append)
(set-fontset-font "fontset-ns" '(#x0080 .  #x024F)
		  (font-spec :family "Menlo") nil 'prepend)
(setq face-font-rescale-alist '((".*Hiragino.*" . 1.2) (".*Menlo.*" . 1.0)))

(setq mac-allow-anti-aliasing t)

(define-key global-map [ns-drag-file] 'ns-find-file)

(setq ns-alternate-modifier 'super)
(setq ns-command-modifier 'meta)

(setq ns-pop-up-frames nil)

;; Window settings
(if (boundp 'window-system)
    (setq initial-frame-alist
	  (append (list
		   '(width . 82)
		   '(height . 50)
		   '(font . "fontset-ns"))
		  initial-frame-alist)))
(setq default-frame-alist initial-frame-alist)
(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'ns-init)
