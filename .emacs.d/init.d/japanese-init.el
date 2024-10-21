;; A. language environment
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(cond ((eq system-type 'darwin)
       (set-keyboard-coding-system
	(if (eq window-system 'mac) 'sjis-mac 'utf-8))
       (set-clipboard-coding-system 'utf-8)
       (prefer-coding-system 'utf-8-unix))
      ((eq system-type 'berkeley-unix))
      ((eq window-system 'w32)))
(set-terminal-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(setenv "LANG" "ja_JP.UTF-8")

;; D. fix yen key problem on JIS keyboard
;; Ando-san's code (see [Macemacsjp-users 1126])
(define-key global-map [2213] nil)
(define-key global-map [67111077] nil)
(define-key function-key-map [2213] [?\\])
(define-key function-key-map [67111077] [?\C-\\])

(define-key global-map [3420] nil)
(define-key global-map [67112284] nil)
(define-key function-key-map [3420] [?\\])
(define-key function-key-map [67112284] [?\C-\\])

(provide 'japanese-init)
