;;----------------------- window settings ---------------------------
(if (boundp 'window-system)
    (setq initial-frame-alist
	  (append (list
		   '(height . 28)
		   '(width  . 82))
		  initial-frame-alist)))
(setq default-frame-alist initial-frame-alist)

(setq exec-path (cons "C:/cygwin/bin/" exec-path))
(setq exec-path (cons "c:/Program Files/Subversion/bin/" exec-path))

;; ;;; Cygwin の zsh を使う場合
(setq explicit-shell-file-name "zsh.exe")
(setq shell-file-name "sh.exe")
(setq shell-command-switch "-c -i")
(modify-coding-system-alist 'process ".*sh\\.exe" '(undecided-dos . utf-8-unix))
;;; argument-editing の設定
;(require 'mw32script)
;(mw32script-init)

;;; fixed-width-fontset
(fixed-width-set-fontset "msgothic" 14)
(provide 'win32-init)
