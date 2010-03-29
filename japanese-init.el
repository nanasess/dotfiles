;; A. language environment
(set-language-environment 'utf-8)
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

  ;; A. end

  ;; B. inline input method (window-system)
;(when (eq window-system 'mac)
;  (add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)
;  )
  ;; B. end

;; C. fix: Unicode => Japanese mapping
;; Thanks to saiki-san (see [macemacsjp-users 870])
;; register circle around digits to cjk table (by Ando-san)
(if (eq window-system 'mac)
    (defadvice utf-translate-cjk-load-tables
      (after my-ad-circled-digit activate)
      (dotimes (i 20)
	(let ((unicode (+ #x2460 i))
	      (char (+ 54433 i)))
	  (if (utf-translate-cjk-substitutable-p unicode)
	      (puthash unicode char ucs-unicode-to-mule-cjk))
	  (puthash char unicode ucs-mule-cjk-to-unicode)))
      ;; prevent to use half-width marks (by Nanba-san)
      (utf-translate-cjk-set-unicode-range
       '((#x2e80 . #xd7a3)
	 (#xff00 . #xffef)
	 (#xa7 . #xa7)                        ;
	 (#xb0 . #xb1)                        ;
	 (#xb4 . #xb4)                        ;
	 (#xb6 . #xb6)                        ;
	 (#xd7 . #xd7)                        ;
	 (#xf7 . #xf7)                        ;
	 (#x370 . #x3ff)                      ; ギリシャ
	 (#x400 . #x4ff)                      ; キリル
	 (#x2000 . #x206f)                    ; 一般句読点
	 (#x2103 . #x2103)                    ; ℃
	 (#x212b . #x212b)                    ; Å
	 (#x2190 . #x21ff)                    ; 矢印
	 (#x2200 . #x22ff)                    ; 数学記号
	 (#x2300 . #x23ff)                    ; 技術記号
	 (#x2460 . #x2473)                    ; 円囲み数字
	 (#x2500 . #x257f)                    ; 罫線
	 (#x25a0 . #x25ff)                    ; 幾何学模様
	 (#x2600 . #x26ff)                    ; その他の記号
	 ))))
;; C. end

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