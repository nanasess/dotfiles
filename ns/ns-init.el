(when (require 'ucs-normalize nil t)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs)
  (prefer-coding-system 'utf-8-hfs))

;; grep-find で .svn を除外
(setq grep-find-command 
      (cons (concat "find . -type f -path '*.svn*' -prune -o -exec "
		    "grep -nH -e  {} /dev/null \\;") 59))

;; font settings
(setq my-font "-*-*-medium-r-normal--12-*-*-*-*-*-fontset-hirakaku")
(setq fixed-width-use-QuickDraw-for-ascii t)
(setq mac-allow-anti-aliasing t)
;(set-default-font my-font)
(add-to-list 'default-frame-alist `(font . ,my-font))
(when (>= emacs-major-version 23)
  (set-fontset-font
   (frame-parameter nil 'font)
   'japanese-jisx0208
   '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
  (setq face-font-rescale-alist
	'(("^-apple-hiragino.*" . 1.2)
	  (".*osaka-bold.*" . 1.2)
	  (".*osaka-medium.*" . 1.2)
	  (".*courier-bold-.*-mac-roman" . 1.0)
	  (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
	  (".*monaco-bold-.*-mac-roman" . 0.9)
	  ("-cdac$" . 1.3))))

(define-key global-map [ns-drag-file] 'ns-find-file)

(setq ns-alternate-modifier 'alt)
(setq ns-command-modifier 'meta)

(setq ns-pop-up-frames nil)

;; XXX Tiger ではバックスラッシュが円マークになってしまう...
(keyboard-translate 165 92)

;; Window settings
(if (boundp 'window-system)
    (setq initial-frame-alist
          (append (list
                   '(width . 82)
                   '(height . 50)
                   )
                  initial-frame-alist)))
(setq default-frame-alist initial-frame-alist)
(server-start)

(provide 'ns-init)
