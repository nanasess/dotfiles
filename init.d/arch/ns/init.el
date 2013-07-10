(when (require 'ucs-normalize nil t)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs)
  (prefer-coding-system 'utf-8-hfs))

;; grep-find で .svn を除外
(setq grep-find-command
      (cons (concat "find . -type f -path '*.svn*' -prune -o -exec "
		    "grep -nH -e  {} /dev/null \\;") 59))

;; font settings

(cond ((find-font (font-spec :family "Ricty"))
       (create-fontset-from-ascii-font "Ricty-15:weight=normal:slant=normal" nil
				       "ns")
       (set-fontset-font "fontset-ns"
			 'unicode (font-spec :family "Ricty" ) nil
			 'append))
      ((create-fontset-from-ascii-font "Menlo-12:weight=normal:slant=normal" nil
				       "ns")
       (set-fontset-font "fontset-ns"
			 'unicode
			 (font-spec :family "Hiragino Kaku Gothic ProN" ) nil
			 'append)
       (setq face-font-rescale-alist '((".*Hiragino.*" . 1.2)
				       (".*Menlo.*" . 1.0)))))

(setq mac-allow-anti-aliasing t)

(define-key global-map [ns-drag-file] 'ns-find-file)

(setq ns-alternate-modifier 'super)
(setq ns-command-modifier 'meta)

(setq ns-pop-up-frames nil)

;; XXX Tiger ではバックスラッシュが円マークになってしまう...
(keyboard-translate 165 92)

;; Window settings
(if (boundp 'window-system)
    (setq initial-frame-alist
	  (append (list
		   '(width . 82)
		   '(height . 45)
		   '(font . "fontset-ns"))
		  initial-frame-alist)))
(setq default-frame-alist initial-frame-alist)
(server-start)

(provide 'ns-init)
