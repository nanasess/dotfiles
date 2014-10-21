;; see also.
;; http://www49.atwiki.jp/ntemacs/pages/16.html

(setenv "LANG" "ja_JP.UTF-8")

(add-to-list 'load-path (expand-file-name user-emacs-directory))
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "site-lisp")))

(prefer-coding-system 'utf-8-unix)
(set-file-name-coding-system 'cp932)

   (require 'cp5022x)
   (define-coding-system-alias 'euc-jp 'cp51932)

   ;; decode-translation-table の設定
   (coding-system-put 'euc-jp :decode-translation-table
              (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
   (coding-system-put 'iso-2022-jp :decode-translation-table
              (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
   (coding-system-put 'utf-8 :decode-translation-table
              (get 'japanese-ucs-jis-to-cp932-map 'translation-table))

   ;; encode-translation-table の設定
   (coding-system-put 'euc-jp :encode-translation-table
              (get 'japanese-ucs-cp932-to-jis-map 'translation-table))
   (coding-system-put 'iso-2022-jp :encode-translation-table
              (get 'japanese-ucs-cp932-to-jis-map 'translation-table))
   (coding-system-put 'cp932 :encode-translation-table
              (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
   (coding-system-put 'utf-8 :encode-translation-table
              (get 'japanese-ucs-jis-to-cp932-map 'translation-table))

   ;; charset と coding-system の優先度設定
   (set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
             'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)
   (set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)

   ;; PuTTY 用の terminal-coding-system の設定
   (apply 'define-coding-system 'utf-8-for-putty
      "UTF-8 (translate jis to cp932)"
      :encode-translation-table
      (get 'japanese-ucs-jis-to-cp932-map 'translation-table)
      (coding-system-plist 'utf-8))
   (set-terminal-coding-system 'utf-8-for-putty)

   ;; East Asian Ambiguous
   (defun set-east-asian-ambiguous-width (width)
     (while (char-table-parent char-width-table)
       (setq char-width-table (char-table-parent char-width-table)))
     (let ((table (make-char-table nil)))
       (dolist (range
            '(#x00A1 #x00A4 (#x00A7 . #x00A8) #x00AA (#x00AD . #x00AE)
             (#x00B0 . #x00B4) (#x00B6 . #x00BA) (#x00BC . #x00BF)
             #x00C6 #x00D0 (#x00D7 . #x00D8) (#x00DE . #x00E1) #x00E6
             (#x00E8 . #x00EA) (#x00EC . #x00ED) #x00F0
             (#x00F2 . #x00F3) (#x00F7 . #x00FA) #x00FC #x00FE
             #x0101 #x0111 #x0113 #x011B (#x0126 . #x0127) #x012B
             (#x0131 . #x0133) #x0138 (#x013F . #x0142) #x0144
             (#x0148 . #x014B) #x014D (#x0152 . #x0153)
             (#x0166 . #x0167) #x016B #x01CE #x01D0 #x01D2 #x01D4
             #x01D6 #x01D8 #x01DA #x01DC #x0251 #x0261 #x02C4 #x02C7
             (#x02C9 . #x02CB) #x02CD #x02D0 (#x02D8 . #x02DB) #x02DD
             #x02DF (#x0300 . #x036F) (#x0391 . #x03A9)
             (#x03B1 . #x03C1) (#x03C3 . #x03C9) #x0401
             (#x0410 . #x044F) #x0451 #x2010 (#x2013 . #x2016)
             (#x2018 . #x2019) (#x201C . #x201D) (#x2020 . #x2022)
             (#x2024 . #x2027) #x2030 (#x2032 . #x2033) #x2035 #x203B
             #x203E #x2074 #x207F (#x2081 . #x2084) #x20AC #x2103
             #x2105 #x2109 #x2113 #x2116 (#x2121 . #x2122) #x2126
             #x212B (#x2153 . #x2154) (#x215B . #x215E)
             (#x2160 . #x216B) (#x2170 . #x2179) (#x2190 . #x2199)
             (#x21B8 . #x21B9) #x21D2 #x21D4 #x21E7 #x2200
             (#x2202 . #x2203) (#x2207 . #x2208) #x220B #x220F #x2211
             #x2215 #x221A (#x221D . #x2220) #x2223 #x2225
             (#x2227 . #x222C) #x222E (#x2234 . #x2237)
             (#x223C . #x223D) #x2248 #x224C #x2252 (#x2260 . #x2261)
             (#x2264 . #x2267) (#x226A . #x226B) (#x226E . #x226F)
             (#x2282 . #x2283) (#x2286 . #x2287) #x2295 #x2299 #x22A5
             #x22BF #x2312 (#x2460 . #x24E9) (#x24EB . #x254B)
             (#x2550 . #x2573) (#x2580 . #x258F) (#x2592 . #x2595)
             (#x25A0 . #x25A1) (#x25A3 . #x25A9) (#x25B2 . #x25B3)
             (#x25B6 . #x25B7) (#x25BC . #x25BD) (#x25C0 . #x25C1)
             (#x25C6 . #x25C8) #x25CB (#x25CE . #x25D1)
             (#x25E2 . #x25E5) #x25EF (#x2605 . #x2606) #x2609
             (#x260E . #x260F) (#x2614 . #x2615) #x261C #x261E #x2640
             #x2642 (#x2660 . #x2661) (#x2663 . #x2665)
             (#x2667 . #x266A) (#x266C . #x266D) #x266F #x273D
             (#x2776 . #x277F) (#xE000 . #xF8FF) (#xFE00 . #xFE0F)
             #xFFFD
             ))
     (set-char-table-range table range width))
       (optimize-char-table table)
       (set-char-table-parent table char-width-table)
       (setq char-width-table table)))
   (set-east-asian-ambiguous-width 2)

   ;; emacs-w3m
   (eval-after-load "w3m"
     '(when (coding-system-p 'cp51932)
        (add-to-list 'w3m-compatible-encoding-alist '(euc-jp . cp51932))))

   ;; Gnus
   (eval-after-load "mm-util"
     '(when (coding-system-p 'cp50220)
        (add-to-list 'mm-charset-override-alist '(iso-2022-jp . cp50220))))

   ;; SEMI (cf. http://d.hatena.ne.jp/kiwanami/20091103/1257243524)
   (eval-after-load "mcs-20"
     '(when (coding-system-p 'cp50220)
        (add-to-list 'mime-charset-coding-system-alist
             '(iso-2022-jp . cp50220))))

   ;; 全角チルダ/波ダッシュをWindowsスタイルにする
   (let ((table (make-translation-table-from-alist '((#x301c . #xff5e))) ))
     (mapc
      (lambda (coding-system)
        (coding-system-put coding-system :decode-translation-table table)
        (coding-system-put coding-system :encode-translation-table table)
        )
      '(utf-8 cp932 utf-16le)))

   ;; cp932エンコード時の表示を「P」とする
   (coding-system-put 'cp932 :mnemonic ?P)
   (coding-system-put 'cp932-dos :mnemonic ?P)
   (coding-system-put 'cp932-unix :mnemonic ?P)
   (coding-system-put 'cp932-mac :mnemonic ?P)

 ;;; 標準フォント
(set-default-font "Consolas 11")
(set-fontset-font (frame-parameter nil 'font)
                   'japanese-jisx0208
                   '("ＭＳ ゴシック" . "unicode-bmp"))
(set-fontset-font (frame-parameter nil 'font)
                   'katakana-jisx0201
                   '("ＭＳ ゴシック" . "unicode-bmp"))

;; ------------------------------------------------------------------------
;; @ image-library
   (setq image-library-alist
         '((xpm "libxpm.dll")
           (png "libpng14.dll")
           (jpeg "libjpeg.dll")
           (tiff "libtiff3.dll")
           (gif "libungif4.dll")
           (svg "librsvg-2-2.dll")
           (gdk-pixbuf "libgdk_pixbuf-2.0-0.dll")
           (glib "libglib-2.0-0.dll")
           (gobject "libgobject-2.0-0.dll"))
         )
;; ------------------------------------------------------------------------
;; @ print

   ;; (require 'cl)
   ;; (defun listsubdir (basedir)
   ;;   (remove-if (lambda (x) (not (file-directory-p x)))
   ;;              (directory-files basedir t "^[^.]")))

   ;; (setq ps-print-color-p t
   ;;       ps-lpr-command "gswin32c.exe"
   ;;       ps-multibyte-buffer 'non-latin-printer
   ;;       ps-lpr-switches '("-sDEVICE=mswinpr2" "-dNOPAUSE" "-dBATCH" "-dWINKANJI")
   ;;       printer-name nil
   ;;       ps-printer-name nil
   ;;       ps-printer-name-option nil
   ;;       ps-print-header nil          ; ヘッダの非表示
   ;;       )
;; ------------------------------------------------------------------------
;; @ setup-cygwin
   ;; (setq cygwin-mount-cygwin-bin-directory
   ;;       (concat (getenv "CYGWIN_DIR") "\\bin"))
   ;; (require 'setup-cygwin)

;; ------------------------------------------------------------------------
;; @ shell
   (require 'shell)
   (setq explicit-shell-file-name "bash.exe")
   (setq shell-command-switch "-c")
   (setq shell-file-name "bash.exe")

   ;; (M-! and M-| and compile.el)
   (setq shell-file-name "bash.exe")
   (modify-coding-system-alist 'process ".*sh\\.exe" 'cp932)

   ;; shellモードの時の^M抑制
   (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

   ;; shell-modeでの補完 (for drive letter)
   (setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@'`.,;()-")

   ;; エスケープシーケンス処理の設定
   (autoload 'ansi-color-for-comint-mode-on "ansi-color"
             "Set `ansi-color-for-comint-mode' to t." t)

   (setq shell-mode-hook
         (function
          (lambda ()

            ;; シェルモードの入出力文字コード
            (set-buffer-process-coding-system 'sjis-dos 'sjis-unix)
            (set-buffer-file-coding-system    'sjis-unix)
            )))

;; ------------------------------------------------------------------------
;; @ menu-tree
   ;; (setq menu-tree-coding-system 'utf-8)
   ;; (require 'menu-tree)

;;----------------------- window settings ---------------------------
(if (boundp 'window-system)
    (setq initial-frame-alist
	  (append (list
		   '(height . 38)
		   '(width  . 82))
		  initial-frame-alist)))
(setq default-frame-alist initial-frame-alist)

;; (setq exec-path (cons "C:/cygwin64/bin" exec-path))
(setq shell-file-name "bash.exe")

;; プロセスが出力する文字コードを判定して、process-coding-system の DECODING の設定値を決定する
(setq default-process-coding-system '(undecided-dos . utf-8-unix))

;; サブプロセスに渡すパラメータの文字コードを cp932 にする
(defmacro set-process-args-coding-system (function args-number)
  `(defadvice ,function (before ,(intern (concat
                                          "ad-"
                                          (symbol-name function)
                                          "-to-set-process-args-coding-sytem"))
                                activate)
     (ad-set-args ,args-number
                  (mapcar (lambda (arg)
                            (if (multibyte-string-p arg)
                                (encode-coding-string arg 'cp932)
                              arg))
                          (ad-get-args ,args-number)))))

(set-process-args-coding-system call-process 4)
(set-process-args-coding-system call-process-region 6)
(set-process-args-coding-system start-process 3)

;; lgrep で Shift_JIS を使うように設定
(setq grep-host-defaults-alist nil) ;; これはおまじないだと思ってください
(setq grep-template "grep -Ia -Os <C> -n <R> <F>")
(setq grep-find-template "xfind . <X> -name '*.howm' -print0 | xargs -0 -e lgrep -Ia -Os <C> -n <R>")

;; ;;; Cygwin の zsh を使う場合
;; (setq explicit-shell-file-name "zsh.exe")
;; (setq shell-file-name "sh.exe")
;; (setq shell-command-switch "-c -i")
;; (modify-coding-system-alist 'process ".*sh\\.exe" '(undecided-dos . utf-8-unix))
;;; argument-editing の設定
;(require 'mw32script)
;(mw32script-init)

(provide 'win32-init)
