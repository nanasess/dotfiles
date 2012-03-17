;; hatena-mode.el --- major mode for Hatena::Diary (http://d.hatena.ne.jp)

;; Created:     Thu Jun 17  2004
;; Keywords:    blog emacs 
;; author:      http://d.hatena.ne.jp/hikigaeru/
;; 公開ページ:    http://d.hatena.ne.jp/hikigaeru/20040617
;; Special Thanks to :
;;              http://d.hatena.ne.jp/dev-null 
;;              and all users

(defconst hatena-version "1.0" "Version number of hatena.el")

;; ■インストール方法
;; 1) 適当なディレクトリにこのファイルをおく.
;;    (~/elisp/ 内においたとする). 
;;
;; 2) .emacs に次の 4 行を追加する.
;; (setq load-path (cons (expand-file-name "~/elisp") load-path))
;; (load "hatena-mode")
;; (setq hatena-usrid "your username on Hatena::Diary")
;; (setq hatena-plugin-directory "~/elisp")
;;    `hatena-use-file' を non-nil にするとパスワードを base64 で
;;    暗号化してファイルに保存しますが、"人間が見てすぐわからない"ぐらいの
;;    意味しかないので注意して下さい。
;;
;; ■使い方
;; 
;; 1)日記を書く
;;    `M-x hatena' で今日の日記が開きます. ただのテキストファイルです。
;;    タイトル を付けたい場合は、一行目に "title" と書いて、その後にテキストを
;;    続けてください。
;;
;; 2)ポストする
;;    日記を書いたら, \C-c\C-p で send できます.
;;    マークアップは、はてなの記法に従います。
;;    \C-ct で「更新」と「ちょっとした更新」を切りかえます。
;; 
;; 3)変数や関数
;;
;;    `hatena-change-trivial' "ちょっとした更新"かどうかを digit に変えます。
;;    `hatena-entry-type' エントリの "*" の動作を切りかえます。
;;                        0 で *pn* に、1 で *t* (タイムスタンプ)になります。
;;
;;    `hatena-submit' (\C-c\C-p) 日記をはてなにポストします
;;    `hatena-delete-diary' その日の日記を web から削除.
;;    `hatena-find-previous' (\C-c\C-b) 
;;    `hatena-find-followings' (\C-c\C-f). それぞれ、前の日と次の日の
;;    日記ファイルを開く。引数を与えるとその日数だけジャンプ。
;;    ( 例 \C 1 2 \C-c\C-b で12日前 )
;;    `hatena-exit' 日記 buffer を save して すべて kill
;;    `hatena-browser-function' に 'browse-url とかやると日記をポスト
;;    した後その日 url を引数としてブラウザを呼びます.
;;    `hatena-insert-webdiary' はてなバッファで実行すると、現在 web に
;;    アップされているファイルを取ってくる。 o
;;
;; 4) 上位モード
;;    hatena-mode はデフォルトで html-mode に被せています。これを
;;    html-helper-mode にしたければ、
;;
;;    -(define-derived-mode hatena-mode html-mode "Hatena"
;;    +(define-derived-mode hatena-mode html-helper-mode "Hatena"
;;
;;    として `eval-buffer' して下さい。
;;
;; 5) hook について
;;    hook とはライブラリを読込んだ時、初期化する時など、特定のタイミ
;;    ングで呼び出したい関数を保持する変数です。hatena-mode には以下の
;;    hook があります
;;
;;    `hatena-mode-hooks' Hatena mode にした時に呼ばれる hook .
;;     例 .emacs に
;;    (add-hook 'hatena-mode-hooks 
;;	  '(lambda ()
;;	     (setq line-spacing 8) ;;行が詰まってるとイヤ、
;;	     ))
;;
;;    `hatena-mode-submit-hook' 日記をポスト`hatena-submit' する直前に
;;     呼び出す関数です。例えば、連続しない改行をすべて除く、などの処理が考えられます。
;;
;;    (add-hook 'hatena-mode-submit-hook
;;	  '(lambda ()
;;           (goto-char (point-min))
;;	     (replace-regexp "\\([^\n]\\)\n\\([^\n]\\)" "\\1\\2")))
;;     


(require 'hatena-vars)
(require 'hatena-kw)
(require 'font-lock)
(require 'derived)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;本体

(if hatena-mode-map
    ()
  (setq hatena-mode-map (make-keymap))
  (define-key hatena-mode-map "\C-c\C-p" 'hatena-submit)
  (define-key hatena-mode-map "\C-c\C-b" 'hatena-find-previous)
  (define-key hatena-mode-map "\C-c\C-f" 'hatena-find-following)
  (define-key hatena-mode-map "\C-ct" 'hatena-change-trivial))

(defconst hatena-today-buffer nil)
(defun hatena (&optional date)
  "Hatena::Diary ページを開く. "
  (interactive)
  (unless (file-exists-p hatena-directory)
    (make-directory hatena-directory t))
  (if (not date)
      (progn
	;;今日の日記のバッファを確認(cookie の管理のため)
	;;存在しなければ、クッキーを取得する。
	(let ((buffer-new-p t)
	      (file-new-p t))
	  (if (memq hatena-today-buffer (buffer-list))
	      (setq buffer-new-p nil)
	    (hatena-login))
	  (setq hatena-today-buffer
		(find-file 
		 (concat hatena-directory (hatena-today-date))))
	  ;;ファイル、バッファが存在しなければ、webの日記をチェック
	  (if (file-exists-p (concat hatena-directory (hatena-today-date)))
	      (setq file-new-p nil))
	  (if (and file-new-p buffer-new-p)
	      (progn 
		(message "日記ファイルもバッファもありません。Webをチェックします")
		(hatena-insert-webdiary)))
	  )
	)
    (if (string-match hatena-fname-regexp date)
        (find-file (concat hatena-directory date))
      (error "Not date"))
    
    )
  ;;keyword-cheating
  (if hatena-kw-if
      (hatena-kw-init)
    nil)
  )

(define-derived-mode hatena-mode html-mode "Hatena"
"はてなモード. "
    (font-lock-add-keywords 'hatena-mode
          (list
           (list "^\\(Title\\) \\(.*\\)$"
                 '(1 hatena-header-face t)
                 '(2 hatena-title-face t))
	   ;; 見出し
           (list  "\\(<[^\n/].*>\\)\\([^<>\n]*\\)\\(</.*>\\)"
                  '(1 hatena-html-face t)
                  '(2 hatena-link-face t)
                  '(3 hatena-html-face t))
	   ;; 見出し2
           (list  "^\\(\\*[^\n ]*\\) \\(.*\\)$"
                  '(1 hatena-markup-face t)
                  '(2 hatena-html-face t))
	   ;;特殊記法
           (list "\\(\\[?\\(a:id\\|f:id\\|i:id\\|r:id\\|map:id\\|graph:id\\|g.hatena:id\\|b:id:\\|id\\|google\\|isbn\\|asin\\|http\\|http\\|ftp\\|mailto\\|search\\|amazon\\|rakuten\\|jan\\|ean\\|question\\|tex\\):\\(\\([^\n]*\\]\\)\\|[^ 　\n]*\\)\\)"
                 '(1 hatena-markup-face t))
           (list  "^:\\([^:\n]+\\):"
                  '(0 hatena-markup-face t)
                  '(1 hatena-link-face t))
           (list  "^\\([-+]+\\)"
                  '(1 hatena-markup-face t))
           (list  "\\(((\\).*\\())\\)"
                  '(1 hatena-markup-face t)
                  '(2 hatena-markup-Face T))
           (list  "^\\(>>\\|<<\\|><!--\\|--><\\|>|?|\\||?|<\\)"
                  '(1 hatena-markup-face t))
           (list  "\\(s?https?://\[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#\]+\\)"
                  '(1 hatena-html-face t))))
    (font-lock-mode 1)
    (set-buffer-modified-p nil)
  (run-hooks 'hatena-mode-hook))

;;hatena-mode トグル
(setq auto-mode-alist
      (append 
       (list 
	(cons (concat hatena-directory hatena-fname-regexp) 'hatena-mode))
       auto-mode-alist))


(defun hatena-today-date(&optional offset date)
;; date は任意の日付、offset は任意の時間、-24 で一日進む
  (let ( (lst (if date
		  (progn
		    (string-match "\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)" date)
		    (list 0 0 0 
			  (string-to-int (match-string 3 date))
			  (string-to-int (match-string 2 date))
			  (string-to-int (match-string 1 date)) 0 nil 32400))
		  (decode-time (current-time))) ))
    (setcar 
     (nthcdr 2 lst) 
     (- (nth 2 lst) (if offset offset hatena-change-day-offset)))
    (format-time-string "%Y%m%d" 
			(apply 'encode-time lst ))))

(defun hatena-submit (&optional file userid)
 "はてな日記 http://d.hatena.ne.jp/ に post メソッドで日記を送る. curl を使う. "
  (interactive)

  (if file nil 
    (setq file buffer-file-name)
    (save-excursion
      ;;"*t*" にするか "*pn*" にするか
      (cond ( (= hatena-entry-type 0)
	      (progn
		(let ((i 0)
		      (j 0))
		  (goto-char (point-min))
		  (while (re-search-forward "^\\*p\\([0-9]\\)\\*" nil t)
		    (if (< i (setq j (string-to-int (match-string 1))))
			(setq i j)))
		  (goto-char (point-min))
		  (while (re-search-forward "^\\(\\*\\)\\([[ ]\\)" nil t)
		    (replace-match 
		     (concat "*p" (format "%d" (setq i (1+ i))) "*\\2")
		     )))))
	    ( (= hatena-entry-type 1)
	      (progn
		(goto-char (point-min))
		(while (re-search-forward "^\\(\\*\\)\\([[ ]\\)" nil t)
		  (replace-match 
		   (concat "*t*\\2")
		   ))))
    (t nil)
    )
      ;;タイトルの*t*を時間に置きかえる
      (goto-char (point-min))
      (let ((i 0))
	(while (re-search-forward "^\\*t\\*" nil t)
	  (replace-match 
	   (concat "*" (hatena-current-second i) "*")
	   (setq i (1+ i))
	   ))))
    (save-buffer))

  (if (not userid) 
      (setq userid hatena-usrid))

  (let ((filename (file-name-nondirectory file)))
    (if (string-match hatena-fname-regexp filename)
        (let* 
            ((year (match-string 1 filename))
             (month (match-string 2 filename))
             (day (match-string 3 filename)) 
             (date (concat year month day))
             ;;はてなに通知するタイムスタンプ
             (timestamp 
              (format-time-string "%Y%m%d%H%m%S" (current-time)))
             
             (baseurl (concat "http://d.hatena.ne.jp/" userid "/"))
             (referer (concat baseurl "edit?date=" date))
             (nexturl (concat baseurl (concat year month day)))
             (url (concat baseurl "edit"))

             (title "")
             (send-file file)
             (full-body 
              (with-temp-buffer
                (insert-file-contents send-file)
		;; バッファを送る前に呼ばれる hooks
		(run-hooks 'hatena-mode-submit-hook)
                (cond ( (string-match "\\`title[ 　]*\\(.*\\)?\n" (buffer-string))
			(progn 
			  (setq title (match-string 1 (buffer-string)))
			  (substring (buffer-string)
				     (length (match-string 0 (buffer-string))))
			  ))
		      ;;古い実装
		      ( (string-match hatena-header-regexp (buffer-string))
			(progn
			  (setq title (match-string 1 (buffer-string)))
			  (substring (buffer-string)
				     (1+ (length (match-string 0 (buffer-string)))))) )
		      (t (buffer-string)))))
             (body (hatena-url-encode-string full-body hatena-default-coding-system))
             (trivial (if hatena-trivial "1" "0"))
             (post-data 
              (concat "dummy=1"
		      "&mode=enter"
                      "&body=" body 
                      "&trivial=" trivial 
                      "&title=" title 
                      "&day=" day 
                      "&month=" month 
                      "&year=" year 
		      ;; session ID for POST to hatena
		      ;; this is a scheme of ensuring security in Hatena::Diary
		      (concat "&rkm="
			      (let* ((md5sum (md5 (with-temp-buffer
						    (insert-file-contents hatena-cookie)
						    (re-search-forward "rk\\s \\([0-9a-zA-Z]+\\)")
						    (concat (buffer-substring
							     (match-beginning 1)
							     (match-end 1)))) nil nil 'utf-8))
				     (p 0)
				     (temp ""))
				(while (> (length md5sum) p)
				  (setq temp
					(concat
					 temp
					 (char-to-string (string-to-number
							  (substring md5sum p (+ p 2)) 16))))
				  (setq p (+ p 2)))
				(substring (base64-encode-string temp) 0 22)))
		      ;; if "date" element exists ,
		      ;; command can't create the new page at hatena
                      (if (hatena-check-newpage referer) 
                          (concat "&date=" date))
                      "&timestamp=" timestamp )))

	  (with-temp-file hatena-tmpfile 
	    (insert post-data))

	  (message "%s => %s" filename referer)
	  (call-process hatena-curl-command nil nil nil 
			"-b" hatena-cookie
			"-x" hatena-proxy
			"--data" (concat "@" hatena-tmpfile)
			url)
	  
	  (message "posted")
	  (and (functionp hatena-browser-function)
	       (funcall hatena-browser-function nexturl))
	  )
      (error "Not Hatena file: %s" file))))

(defun hatena-login ()
  (interactive)
  (if (file-exists-p hatena-cookie)
      (delete-file hatena-cookie))
  (message (concat "logging in to \"" hatena-url "\" as \"" hatena-usrid "\""))
  (let ((password (hatena-ask-password)))

    (call-process hatena-curl-command nil nil nil 
		  "-k"  "-c" hatena-cookie
		  "-x" hatena-proxy
		  "-d" (concat "name=" hatena-usrid)
		  "-d" (concat "password=" password)
		  "-d" (concat "autologin=1")
		  "-d" (concat "mode=enter")
		  "https://www.hatena.ne.jp/login"))
    (message "Say HAPPY! to Hatena::Diary"))


(defun hatena-check-newpage (urldate)
  "ページが作成済みかどうかチェック"
  (message "checking diary ....")
  (call-process hatena-curl-command nil nil nil 
                  "-o" hatena-tmpfile2
                  "-b" hatena-cookie
                  urldate)
  (if (save-excursion
        (find-file hatena-tmpfile2)
        (prog1 
            (string-match "name=\"date\"" 
                          (buffer-string))
          (kill-this-buffer)))
      (progn  
	(message "modify diary")
	t)
    (message "make new diary") nil))

(defun hatena-diary-file-p(file)
  (let ((fname (file-name-nondirectory file)))
    (if (string-match hatena-fname-regexp fname) t nil)))

(defun hatena-get-diary-string(&optional date)
  "はてなにある日記ファイルを取り、その文字列を返す。
ログインしていなければならない。"
  (if (not date) (error "not date"))
  (message "checking diary of %s ...." date)
  (let ((urldate (concat "http://d.hatena.ne.jp/"
                         hatena-usrid
                         "/edit?date="
                         date)))
    (call-process hatena-curl-command nil nil nil 
                  "-o" hatena-tmpfile
                  "-b" hatena-cookie
                  urldate))
  (with-temp-buffer
    "*hatena-get*"
    (insert-file-contents hatena-tmpfile)
    ;;ここなんとか...
    (goto-char (point-min))(while (replace-string "&quot;" "\""))
    (goto-char (point-min))(while (replace-string "&amp;" "&"))
    (goto-char (point-min))(while (replace-string "&gt;" ">"))
    (goto-char (point-min))(while (replace-string "&lt;" "<"))
    (goto-char (point-min))(while (replace-string "&#39;" "'"))

    (if (string-match "<textarea[^>\n]*>\\(\\(\n\\|.\\)*?\\)</textarea>" 
                      (buffer-string))
          (match-string 1 (buffer-string)) nil)))

(defun hatena-insert-webdiary(&optional date)
  "web の日記を挿入する。"
  (interactive)
  (if date nil
      (setq date (file-name-nondirectory buffer-file-name)))
  (if (string-match hatena-fname-regexp date)
      (insert (hatena-get-diary-string date))
    (error "not date or hatena file")))

(defun hatena-delete-diary(&optional file userid)
  "日記を削除する。ローカルは削除しない。"
  (interactive)
  ;;バッファから読むと送信時間のところに"deleted"
    (if file nil 
      (setq file buffer-file-name))
    (if (not userid)
	(setq userid hatena-usrid))
    (let ((filename (file-name-nondirectory file)))
      (if (string-match hatena-fname-regexp filename)
	  (let* 
	      ((year (match-string 1 filename))
	       (month (match-string 2 filename))
	       (day (match-string 3 filename)) 
	       (date (concat year month day))
	       (baseurl (concat "http://d.hatena.ne.jp/" userid "/"))
	       (referer (concat baseurl "edit?date=" date))
	       (url (concat baseurl "edit"))

	       (edit (hatena-url-encode-string "この日を削除"))
	       (post-data 
		(concat "edit=" edit
			"&date=" date
			(concat "&rkm="
			      (let* 
				  ((md5sum (md5 
					    (with-temp-buffer
					      (insert-file-contents hatena-cookie)
					      (re-search-forward "rk\\s \\([0-9a-zA-Z]+\\)")
					      (concat (buffer-substring
						       (match-beginning 1)
						       (match-end 1)))) nil nil 'utf-8))
				   (p 0)
				   (temp ""))
				(while (> (length md5sum) p)
				  (setq temp
					(concat
					 temp
					 (char-to-string (string-to-number
							  (substring md5sum p (+ p 2)) 16))))
				  (setq p (+ p 2)))
				(substring (base64-encode-string temp) 0 22)))
			"&mode=delete")))
	    (message "deleting %s" referer)
	    (with-temp-file hatena-tmpfile (insert post-data))
	    
	    (call-process hatena-curl-command nil nil nil 
			  "-b" hatena-cookie
			  "-x" hatena-proxy
			  "--data" (concat "@" hatena-tmpfile)
			  url)
	    
	    (message "deleted"))
	(error "Not Hatena file: %s" file))))

(defun hatena-logout()
  (interactive)
  (call-process hatena-curl-command nil nil nil 
		"-b" hatena-cookie
		"-x" hatena-proxy
		"http://d.hatena.ne.jp/logout")
  (message "logged out from d.hatena.ne.jp"))

(defun hatena-ask-password()
  (let (pass str)
    (if (null hatena-use-file)
	(setq pass (read-passwd "password ? : "))
      ;;ファイルが無かった場合は作る。
      (if (not (file-exists-p hatena-password-file))
	  (append-to-file (point) (point) hatena-password-file))
      (setq str (with-temp-buffer nil		  
		  (insert-file-contents hatena-password-file)
		  (buffer-string)))
      (if (string-match "[^ ]+" str)
	  (setq pass (base64-decode-string (match-string 0 str)))
	(setq pass (read-passwd "password ? : "))
	(with-temp-file hatena-password-file
	  (insert (base64-encode-string
		   (format "%s" pass)))))
    pass)))

(defun hatena-exit()
  "hatena-fname-regexpにマッチするバッファをすべて保存して消去"
  (interactive)
  (if (yes-or-no-p "save all diaries and kill buffer ?")
      (progn
	(let ((buflist (buffer-list))	      
	      (i 0))
	  (while (< i (length buflist))
	    (let ((bufname (buffer-name (nth i (buffer-list)))))
	      (if (string-match hatena-fname-regexp bufname)
		  (progn 
		    (if (buffer-modified-p (nth i (buffer-list)))
			(save-buffer (nth i (buffer-list))))
		    (kill-buffer (nth i (buffer-list)))))
	      (setq i (1+ i))))))))

(defun hatena-find-previous (&optional count file)
  "count 日前の日記を開く count が nil なら一日だけ戻る"
  (interactive "p")
  (hatena-find-pf (if count (- count) -1) (buffer-name)))

(defun hatena-find-following (&optional count file)
  "count 日後の日記を開く count が nil なら一日だけすすむ"
  (interactive "p")
  (hatena-find-pf (if count count 1) (buffer-name)))

(defun hatena-find-pf(count &optional file)
  (if (equal major-mode 'hatena-mode)
      (if (not file)
	  (setq file (buffer-name)))
    (error "not hatena mode"))
  (let ((find-previous 
	 (lambda (element count lst)
	   (let* ((sublst (member element lst))
		  (result (+ (- (length lst) (length sublst))
			     count)))
	     (if (or (null sublst)
		     (< result 0)) nil
	       (nth result lst)))))
	previous)
    (setq previous
	  (funcall find-previous
		   (file-name-nondirectory file)
		   (if (not count) 1 count)
		   (directory-files 
		    hatena-directory 
		    nil hatena-fname-regexp)))
    (if previous (find-file (concat (file-name-directory file) previous))
      ;;見つからない時は、未来の日付を尋ねる。
      (let ((filename (read-string "作成したい日付を入力: " 
				   (hatena-today-date (* -24 count) (buffer-name)) nil)))
	(if (string-match hatena-fname-regexp filename)
	    (progn
	      (find-file filename)
	      (save-buffer))
	  (error "日付ファイルではありません!!"))))))

(defun hatena-get-webdiary ()
  "http://d.hatena.ne.jp/usrid/export を取ってきて変換。足りない日記分をファイルに足す。"
  (interactive)
  ;;exportをとってくる
  (call-process hatena-curl-command nil nil nil 
		"-o" hatena-tmpfile
		"-b" hatena-cookie
		(concat "http://d.hatena.ne.jp/" hatena-usrid "/export" ))

  ;;export は utf-8 なので、hatena-default-coding-system  に直す。
  (let ((filelst (directory-files 
                  hatena-directory 
                  nil hatena-fname-regexp))
	(title-regexp "<day date=\"\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)\" title=\"\\(.*\\)\">\n<body>\n")
	pt-start pt-end day title body)
    (with-temp-buffer 
      "*hatena-get*"
      (insert-file-contents hatena-tmpfile)
      (set-buffer-file-coding-system hatena-default-coding-system)
      (hatena-translate-reverse-region (point-min) (point-max))
      
      (while (re-search-forward title-regexp nil t)
	(setq day (concat (match-string 1) (match-string 2) (match-string 3)))
	(setq title (match-string 4))
	(setq pt-start (match-end 0))
	(re-search-forward "</body>\n</day>" nil t)
	(setq pt-end (match-beginning 0))
	(setq body (buffer-substring pt-start pt-end))
	(save-excursion
	  (if (null (member day filelst))
	      (progn
		(hatena day)
		(set-buffer-file-coding-system hatena-default-coding-system)
		(message "creatig %s" day)
		(insert body)
		(save-buffer)
		(kill-buffer (current-buffer))))))
      (message "finished"))))



(defun hatena-url-encode-string (str &optional coding)
  "w3m-url-encode-string からコピー"
  (apply (function concat)
	 (mapcar
	  (lambda (ch)
	    (cond
	     ((eq ch ?\n)		; newline
	      "%0D%0A")
	     ((string-match "[-a-zA-Z0-9_:/.]" (char-to-string ch)) ; xxx?
	      (char-to-string ch))	; printable
	     ((char-equal ch ?\x20)	; space
	      "+")
	     (t
	      (format "%%%02x" ch))))	; escape
	  ;; Coerce a string to a list of chars.
	  (append (encode-coding-string (or str "")
					(or coding
					    buffer-file-coding-system
					    'iso-2022-7bit))
		  nil))))

;----------------特殊文字の変換----------------
;;yahtml を改変
(defvar hatena-entity-reference-chars-alist
  '((?> . "gt") (?< . "lt") (?& . "amp") (?\" . "quot"))
  "translation table from character to entity reference")
(defvar hatena-entity-reference-chars-regexp "[><&\\]")
(defvar hatena-entity-reference-chars-reverse-regexp "&\\(gt\\|lt\\|amp\\|quot\\);")

(defun hatena-translate-region (beg end)
  "Translate inhibited literals."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let ((ct hatena-entity-reference-chars-alist))
	(goto-char beg)
	(while (re-search-forward hatena-entity-reference-chars-regexp nil t)
	  (replace-match
	   (concat "&" (cdr (assoc (preceding-char) ct)) ";")))))))

(defun hatena-translate-reverse-region (beg end)
  "Translate entity references to literals."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let ((ct hatena-entity-reference-chars-alist))
	(goto-char beg)
	(while (re-search-forward
		hatena-entity-reference-chars-reverse-regexp nil t)
	  ;(setq c (preceding-char))
	  (replace-match 
	   (string (car 
		 (rassoc (match-string 1)
			 ct)))))))))

(defun hatena-change-trivial ()
  (interactive)
  (if (not hatena-trivial)
      (progn
	(message "ちょっとした更新モード")
	(setq hatena-trivial t))
    (setq hatena-trivial nil)
    (message "更新モード")))


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

(provide 'hatena-mode)

;;;;;end of file
