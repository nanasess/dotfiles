(provide 'hatena-kw)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; kw-cheating-section.

;;hatena-kw-cheating を使う場合。`hatena-tools.pl' のあるディレクトリ。
;;hatena-mode.el と同じ。
(defvar hatena-plugin-directory nil)

(defconst hatena-kw-if nil
  "デフォルトで kw-cheating(他人の日記の流し読み) をするかどうか")

(defvar hatena-kw-list-buf nil)
(defvar hatena-kw-cheating-buf nil)
(defvar hatena-edit-buf nil)

(defvar hatena-kw-process nil
  "perl のプロセス")

(defun hatena-kw-init()
  (interactive)
  ;;まず窓を一つにリセットしてから、二つに割る。
  ;;こうすると、いつ `M-x hatena' しても窓が二つある状況になる。
  (delete-other-windows)
  (setq hatena-edit-buf (current-buffer))
  ;;hatena-edit-wdw 日記を書くバッファ
  ;;hatena-keyword-cheating-wdw 他の日記を見るウィンドウ
  (setq hatena-edit-wdw (selected-window))
  (setq hatena-kw-cheating-wdw 
	(split-window hatena-edit-wdw 
		      (floor (* (window-height)
				hatena-kw-window-split-ratio))))
  (save-selected-window 
    (select-window hatena-kw-cheating-wdw)
    (setq hatena-kw-cheating-buf
	  (switch-to-buffer "*hatena-keyword-cheating*"))
    (setq mode-name "Hatena kw-cheating"))
  ;;タイマースタート
  (if hatena-kw-post-timer
      (cancel-timer hatena-kw-post-timer))
  (if hatena-kw-ruby-timer
      (cancel-timer hatena-kw-ruby-timer))
  (if hatena-kw-get-timer
      (cancel-timer hatena-kw-get-timer))
  (setq hatena-kw-post-timer
	(run-at-time 0 
		     hatena-kw-repeat
		     'hatena-kw-post-func))
  (setq hatena-kw-ruby-timer
	(run-at-time (/ hatena-kw-repeat 4)
		     hatena-kw-repeat
		     'hatena-kw-ruby-func))
  (setq hatena-kw-get-timer
	(run-at-time (/ hatena-kw-repeat 2);; get は post より少し遅らせる
		     hatena-kw-repeat
		     'hatena-kw-get-func))
  ;; はてなにキーワードを問い合わせるためのページを作る
  (hatena-kw-submit hatena-kw-temp-diary t)
  (message (concat "creating diary on " hatena-url "for kw-cheating"))
  )

(defun hatena-kw-final()
  (interactive)
  (if hatena-kw-post-timer
      (cancel-timer hatena-kw-post-timer))
  (if hatena-kw-ruby-timer
      (cancel-timer hatena-kw-ruby-timer))
  (if hatena-kw-get-timer
      (cancel-timer hatena-kw-get-timer)))

(defun hatena-kw-post-func()
  "`hatena-kw-temp-diary' に非同期ポスト. "
;  (if (string-match "Hatena" mode-name)
      (progn
	(hatena-kw-submit hatena-kw-temp-diary))
;    (cancel-timer hatena-kw-post-timer));; hatena-mode でなければ、直ちに停止する。
  )

(defun hatena-kw-get-func()
  "ruby の output を hatena-kw-cheating-buf に表示する。"
;  (if (string-match "Hatena" mode-name)
;      (progn
	  (with-current-buffer (current-buffer)
	    (set-buffer hatena-kw-cheating-buf)
	    (delete-region (point-min) (point-max)) ;;全部消して
	    ;;from insert-file-contents-as-coding-system
	    (let ((coding-system-for-read 'euc-jp)
		  format-alist)
	      (insert-file-contents hatena-kw-result-file))
	    ;;簡易レンダリング
	    (goto-char (point-min))
	    (while (re-search-forward "<[^>]+>" nil t)
			       (replace-match "" nil nil))
	    (goto-char (point-min))
	    (while (re-search-forward "\n\t+" nil t)
			       (replace-match "\n" nil nil))
	    (goto-char (point-min))
	    (while (re-search-forward "\n+" nil t)
			       (replace-match "\n" nil nil))
	    
	    )
;	  )
;    (cancel-timer hatena-kw-get-timer) ;; hatena-mode でなければ、直ちに停止する。
 ;   (message "Hatena get-timer cancelled"))
  )

(defvar hatena-kw-process nil)
(defun hatena-kw-perl-func(today)
  "  hatena-tools.pl に処理を渡す。 "
      (start-process "hatena-kw-process" 
		 "*hatena keyword*"
		 "perl"
		 (concat hatena-plugin-directory "hatena-tools.pl")
		 (concat hatena-directory today)
		 "firefox"
		 "10"
		 "10"
		 )
)
;(hatena-kw-perl-func "20051016")


(defun hatena-current-second(number)
  "現在までの秒数を返す。emacs では整数がケタ溢れするので、浮動小数点で"
  (let* ((ct (current-time))
	 (high (float (car ct)))
	 (low (float (car (cdr ct))))
	 str)
    (setq str (format "%f"(+ 
			   (+ (* high (lsh 2 15)) low)
			   number)))
    (substring str 0 10) ;;
    ))

