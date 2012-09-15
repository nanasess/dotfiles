;;;
;;; http://d.hatena.ne.jp/suikyounohito/20120526
;;;

(defvar org-export-hatena-notation-section
  '("^\\* \\([^\t\n\r\f]*\\)\n\\[.*$" "* \\1"))
(defvar org-export-hatena-notation-subsection
  '("^\\*\\* \\([^\t\n\r\f]*\\)$" "** \\1"))
(defvar org-export-hatena-notation-subsubsection
  '("^\\*\\*\\* \\([^\t\n\r\f]*\\)$" "*** \\1"))
(defvar org-export-hatena-notation-link
  '("\\[\\[\\(.*\\)\\]\\[\\(.*\\)\\]" "[\\1:title=\\2]"))   ; 追加
(defvar org-export-hatena-notation-quote
  '("[ ]*#\\+BEGIN_QUOTE[ \t]*" ">>" "[ ]*#\\+END_QUOTE" "<<"))
(defvar org-export-hatena-notation-super-pre
  '("[ ]*#\\+BEGIN_EXAMPLE[ \t]*" ">||" "[ ]*#\\+END_EXAMPLE" "||<"))
(defvar org-export-hatena-notation-src
  '("[ ]*#\\+BEGIN_SRC[ \t]*\\([^\t\n\r\f]*\\)" ">|\\1|" "[ ]*#\\+END_SRC" "||<"))
(defvar org-export-hatena-temp-buffer-name "*Org Hatena Export*")

(defun org-export-hatena-section (notation)
  ;; buffer で取り得る最小の位置に移動する
  (goto-char (point-min))
  ;; notation[0] に match しなかったら停止する、をバッファの終わりまで繰り返す。
  ;; match しなかった場合は nil を返す
  (while (re-search-forward (nth 0 notation) nil t)
    (replace-match (nth 1 notation))))  ; 最後の検索結果を notation[1] で置換する

;;; org-export-hatena-section を２回実行する関数
;; 一回目は、notation[0] -->  notaition[1]
;; 二回目は、notation[2] -->  notaition[3]
(defun org-export-hatena-begin-to-end (notation)
  (goto-char (point-min))
  (while (re-search-forward (nth 0 notation) nil t)
    (replace-match (nth 1 notation)))
  (goto-char (point-min))
  (while (re-search-forward (nth 2 notation) nil t)
    (replace-match (nth 3 notation))))

;;; 
(defun org-export-hatena (beg end)
  (interactive "r")                     ; Region
  (let ((diary (buffer-substring beg end))                     ; local var1
        (begin-to-end `(,org-export-hatena-notation-quote
                        ,org-export-hatena-notation-super-pre
                        ,org-export-hatena-notation-src))      ; local var2
        (section `(,org-export-hatena-notation-section
                   ,org-export-hatena-notation-subsection
                   ,org-export-hatena-notation-subsubsection
                   ,org-export-hatena-notation-link            ; 追加
                   )) ; local var3
        (buffer (get-buffer-create      ;  create buffer
                 (generate-new-buffer-name
                  org-export-hatena-temp-buffer-name))))       ; local var4
    (set-buffer buffer)                                        ; make buffer
    (insert diary)                                             ; point に引数を追加
    (mapc (function org-export-hatena-begin-to-end) begin-to-end)
    (mapc (function org-export-hatena-section) section)
    (switch-to-buffer-other-window buffer)))

(provide 'hatena)
