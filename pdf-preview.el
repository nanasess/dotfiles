;; -*- mode: lisp-interaction; syntax: elisp; coding: iso-2022-7bit -*-

;; pdf-preview.el : preview text from the buffer as pdf files through PostScript
;; Version 1.0.3
;;
;; Copyright (C) 2004-2005 by T. Hiromatsu <matsuan@users.sourceforge.jp>

;;; Commentary:

;; This package defines fixed-width multilingual fontsets for Carbon Emacs
;; on Mac OS X. Comments, questions and feedback will be sent to an english
;; list <http://lists.sourceforge.jp/mailman/listinfo/macemacsjp-english>
;; of MacEmacs JP project <http://macemacsjp.sourceforge.jp/en/>.
;;----------------------------------------------------------------------
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; The GNU General Public License can be gotten from
;; the Free Software Foundation, Inc.,
;;     59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;     http://www.gnu.org/licenses/gpl.html
;;
;;----------------------------------------------------------------------
;;      本プログラムはフリー・ソフトウェアです。
;;      あなたは、Free Software Foundationが公表したGNU 一般公有使用許諾の
;;      「バージョン２」或いはそれ以降の各バージョンの中からいずれかを選択し、
;;      そのバージョンが定める条項に従って本プログラムを
;;      再頒布または変更することができます。
;;
;;      本プログラムは有用とは思いますが、頒布にあたっては、
;;      市場性及び特定目的適合性についての暗黙の保証を含めて、
;;      いかなる保証も行ないません。
;;      詳細についてはGNU 一般公有使用許諾書をお読みください。
;;
;;      GNU一般公有使用許諾は、　
;;      Free Software Foundation,
;;         59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
;;         http://www.gnu.org/licenses/gpl.html
;;      から入手可能です。
;;
;;----------------------------------------------------------------------
;; usage1:
;;     M-x pdf-preview-buffer
;;
;; usage2:
;;     C-u M-x pdf-preview-buffer
;;     You can set some factors interactively shown in follows.
;;         ps-paper-type (Paper Size)
;;         ps-landscape-mode (Paper Direction)
;;         ps-print-header (Title on/off)
;;         pdf-preview-font-rescale-factor (Font Rescale Factor)
;;         ps-line-spacing (Linse Spacing)
;;
;; usage3:
;;     (pdf-preview-buffer arg-list) 
;;         arg-list contains
;;             (ps-paper-type ps-landscape-mode
;;                            ps-print-header pdf-preview-font-rescale-factor).
;;
;;     example ; (pdf-preview-buffer '(a3 t nil 8 6))
;;
;; 1. 機能
;;     1) Emacs の バッファーのテキストを、PostScript format を経由して、
;;        pdf に変換し、適当なビュワーを使って表示します。
;;        Default では、GhostScript (ps2pdf13) を使っています。
;;     2) 日本語フォントとアスキーフォントの幅を 2:1 に設定します。
;;     3) プレフィックス付きで呼び出すと、幾つかの項目を対話的に設定できます。
;;
;; 2. 関数
;;     主な関数は、以下の8個。各々の差は、ps-print(spool系の関数と同じ
;;
;;         pdf-preview-spool-buffer
;;         pdf-preview-spool-buffer-with-faces
;;         pdf-preview-spool-region
;;         pdf-preview-spool-region-with-faces
;;     上記4関数は、interactiveに変数を取得した上、ps-spool-* でPostScript
;;     フォーマットのbuffer(*PostScript)を作成します。
;;
;;         pdf-preview-buffer
;;         pdf-preview-buffer-with-faces
;;         pdf-preview-region
;;         pdf-preview-region-with-faces
;;     上記4関数は、ps-preview-spool-* でspoolした、PostScriptフォーマットに対し、
;;         関数 pdf-preview-rescale-mule-font
;;     を使って、Mule Font のみ 1.2 倍(所謂当幅にする為)した後、
;;         関数 pdf-preview-do-despool
;;     で、pdfファイルを作成します。
;;
;; 3. 使い方
;;
;;     1) M-x pdf-preview-buffer
;;         初期設定値で、ps、pdf ファイルが作られます。
;;
;;     2) C-u M-x pdf-preview-buffer
;;         下記項目を、対話的に設定できます。
;;                 紙サイズ (ps-paper-type)
;;                     'b5, 'b4, 'a4small, 'ledger, 'lettersmall, 'legal,
;;                     'letter, 'a3, 'a4,
;;                 紙の向き (ps-landscape-mode)
;;                     'Landscape, 'Portrait,
;;                 ヘッダーの有無 (ps-preint-header)
;;                     't, 'nil,
;;                 フォントサイズの拡大比率 (pdf-preview-font-rexcale-factor)
;; 　　                任意の正の数
;;                 行間隔 (ps-line-spacing)
;;                     任意の正の数
;;
;;     3) (pdf-preview-buffer arg-list)
;;         引数付きで関数呼び出し。引数は、上記の5項目を含んだリストである事。
;;         例 ; (pdf-preview-buffer '(a3 t nil 8 10))
;;                 a3、横置きで、ヘッダー無し、フォントサイズは標準の8倍
;;                 行間隔は 10/72 inch
;;
;; 4. 設定可能な変数
;;     1) pdf-preview-ps2pdf-command
;;             ps2pdf に使うコマンド
;;             デフォルト : "ps2pdf13"
;;    
;;             * 私は、cjkps2pdf.pl を使ったりしているので、
;;                 "perl ~/bin/cjkps2pdf.pl --keepmetrics"
;;               にしています。
;;    
;;     2) pdf-preview-ps2pdf-paper-size-option
;;             pdf-preview-ps2pdf-command で、紙サイズの指定に使うオプション
;;             デフォルト : "-sPAPERSIZE="
;;    
;;             * 私は、cjkps2pdf.pl を使ったりしているので、
;;                 "--papersize "
;;               にしています。
;;    
;;     3) pdf-preview-preview-command
;;             プレビュワーを起動するコマンド
;;             デフォルト :
;;                  windows + CMD.EXE --- "start"
;;                  windows + cygwin  --- "cygstart"
;;                  carbon-emacs      --- "open"
;;                  others            --- "xpdf"
;;    
;;     4) pdf-preview-font-rescale-factor
;;             フォントの拡大率
;;             デフォルト : 1.0
;;    
;;             * 私は、1.1 にしてます。
;;    
;; 5. 履歴
;;     1.0.3 2005-05-30 bug fix
;;     1.0.2 2005-05-24 bug fix
;;                      プレビュワーコマンドのデフォルト値をOS毎に設定
;;     1.0.1 2005-05-23 .emacs の設定がなくても動くように変更
;;                      紙サイズA4, 行間隔6をデフォルトに
;;                      行間隔も対話的に設定できるように
;;     1.0.0 2005-05-20 リリース

;;; code

;;;
;;; initialize section
;;;

(if (not (boundp 'ps-paper-type)) (setq ps-paper-type 'a4))
(if (not (boundp 'ps-line-spacing)) (setq ps-line-spacing 6))

(require 'ps-print)

(require 'ps-mule)
(defalias 'ps-mule-header-string-charsets 'ignore)

(defvar pdf-preview-ps2pdf-command "ps2pdf13")

(defvar pdf-preview-ps2pdf-paper-size-option "-sPAPERSIZE=")

(defvar pdf-preview-preview-command
        (cond
         ((featurep 'dos-w32)
          (if (string-match "\\(cmdproxy\.exe$\\|cmd\.exe$\\)" shell-file-name)
              "start"
            "cygstart"))
         ((featurep 'mac-carbon) "open")
         ("xpdf")))

(defvar pdf-preview-ignored-papersize-list '("tabloid" "statement" "executive"))

(defvar pdf-preview-font-rescale-factor 1.0)

(defun pdf-preview-get-paper-size (paper-type)
  (let (size)
    (downcase
     (nth 3
          (if (setq size (assoc paper-type ps-page-dimensions-database))
              size
            (assoc ps-paper-type ps-page-dimensions-database))))))

(defvar pdf-preview-papersize-list
      (let ((papersize-list)
            (papersize-database ps-page-dimensions-database)
            (ignore-paper pdf-preview-ignored-papersize-list))
        (while papersize-database
          (setq papersize-list
                (cons
                 (cons
                  (car (car papersize-database))
                  (pdf-preview-get-paper-size (car (car papersize-database))))
                 papersize-list))
          (setq papersize-database (cdr papersize-database)))
        (while ignore-paper
          (setq papersize-list
                (delete (rassoc (car ignore-paper) papersize-list) papersize-list))
          (setq ignore-paper (cdr ignore-paper)))
        (eval 'papersize-list)))

;;;
;;; Function section
;;;

(defun pdf-preview-do-despool (&optional papersize)
  "Preview PostScript spool via PDF"
  (interactive (list (pdf-preview-papersize current-prefix-arg)))
  (let*
      ((ps-temp-file
        (concat (make-temp-name (concat temporary-file-directory "pdf")) ".ps"))
       (pdf-temp-file
        (concat (file-name-sans-extension ps-temp-file) ".pdf"))
       (ps2pdf-command
        (format "%s %s%s %s %s"
                pdf-preview-ps2pdf-command
                pdf-preview-ps2pdf-paper-size-option
                (cdr
                 (assoc
                  (if papersize
                      papersize
                    (car (assoc ps-paper-type pdf-preview-papersize-list)))
                  pdf-preview-papersize-list))
                ps-temp-file
                pdf-temp-file))
;;        (rm-command (format "rm %s" ps-temp-file))
       (preview-command
        (format "%s %s" pdf-preview-preview-command pdf-temp-file)))
    (ps-do-despool ps-temp-file)
    (shell-command (concat ps2pdf-command " && " preview-command))))

(defun pdf-preview-rescale-mule-font (&optional arg)
  "Rescale mule fonts for keeping in line with ascii"
  (interactive)
  (save-excursion
    (set-buffer "*PostScript*")
    (goto-char (point-min))
    (let (s end)
      (while
          (re-search-forward
           "/f[89][29]-[0-2] \\([0-9]+\.[0-9][0-9][0-9][0-9][0-9][0-9]\\) /\\(Ryumin-Light\\|GothicBBB-Medium\\)\\(-H\\|\\.Katakana\\|\\.Hanakaku\\) \\(DefFontMule\\)"
           nil t)
        (progn
          (goto-char (match-end 4))
          (setq end (point-marker)) 
          (goto-char (match-beginning 1))
          (setq s
                (format "%.6f"
                        (*
                         (string-to-number
                          (buffer-substring (match-beginning 1) (match-end 1)))
                         1.2)))
          (delete-region (match-beginning 1) (match-end 1))
          (insert s)
          (goto-char end))
        )))) 

(defun pdf-preview-spool-buffer (&optional arg-list)
  "Generate and spool a PostScript image of the buffer for pdf preview."
  (interactive (list (pdf-preview-factor current-prefix-arg)))
  (pdf-preview-spool 'ps-spool-buffer arg-list))

(defun pdf-preview-buffer (&optional arg-list)
  "Generate and preview a pdf file of the buffer via PostScript."
  (interactive (list (pdf-preview-factor current-prefix-arg)))
  (pdf-preview-spool-buffer arg-list)
  (save-excursion (pdf-preview-rescale-mule-font))
  (pdf-preview-do-despool (car arg-list)))
  
(defun pdf-preview-spool-buffer-with-faces (&optional arg-list)
  "Generate and spool a PostScript image of the buffer with faces for pdf preview."
  (interactive (list (pdf-preview-factor current-prefix-arg)))
  (pdf-preview-spool 'ps-spool-buffer-with-faces arg-list))

(defun pdf-preview-buffer-with-faces (&optional arg-list)
  "Generate and preview a pdf file of the buffer with faces via PostScript."
  (interactive (list (pdf-preview-factor current-prefix-arg)))
  (pdf-preview-spool-buffer-with-faces arg-list)
  (save-excursion (pdf-preview-rescale-mule-font))
  (pdf-preview-do-despool (car arg-list)))
  
(defun pdf-preview-spool-region (&optional arg-list)
  "Generate and spool a PostScript image of the region for pdf preview."
  (interactive (list (pdf-preview-factor current-prefix-arg)))
  (pdf-preview-spool 'ps-spool-region arg-list))

(defun pdf-preview-region (&optional arg-list)
  "Generate and preview a pdf file of the region via PostScript."
  (interactive (list (pdf-preview-factor current-prefix-arg)))
  (pdf-preview-spool-region arg-list)
  (save-excursion (pdf-preview-rescale-mule-font))
  (pdf-preview-do-despool (car arg-list)))
  
(defun pdf-preview-spool-region-with-faces (&optional arg-list)
  "Generate and spool a PostScript image of the region with faces for pdf preview."
  (interactive (list (pdf-preview-factor current-prefix-arg)))
  (pdf-preview-spool 'ps-spool-region-with-faces arg-list))

(defun pdf-preview-region-with-faces (&optional arg-list)
  "Generate and preview a pdf file of the region with faces via PostScript."
  (interactive (list (pdf-preview-factor current-prefix-arg)))
  (pdf-preview-spool-region-with-faces arg-list)
  (save-excursion (pdf-preview-rescale-mule-font))
  (pdf-preview-do-despool (car arg-list)))

(defun pdf-preview-papersize (prefix-arg)
  (and prefix-arg
       (or (numberp prefix-arg) (listp prefix-arg))
       (let*
           ((prompt (format "Papersize: "))
            (completion-ignore-case t)
            (default-paper (cdr (assoc ps-paper-type pdf-preview-papersize-list))))
         (car
          (rassoc
           (completing-read
            prompt
            (mapcar (lambda (list) (cdr list)) pdf-preview-papersize-list)
            nil t default-paper)
           pdf-preview-papersize-list)))))

(defun pdf-preview-factor (prefix-arg)
  (and prefix-arg
       (or (numberp prefix-arg) (listp prefix-arg))
       (list
        (let*
            ((prompt (format "Papersize : "))
             (completion-ignore-case t)
             (default-paper (cdr (assoc ps-paper-type pdf-preview-papersize-list))))
          (car
           (rassoc
            (completing-read
             prompt
             (mapcar (lambda (list) (cdr list)) pdf-preview-papersize-list)
             nil t default-paper)
            pdf-preview-papersize-list)))
        (let* ((prompt (format "Direction : "))
               (completion-ignore-case t)
               (default-direction (if ps-landscape-mode "Landscape" "Portrait")))
          (cdr
           (assoc
            (completing-read
             prompt '("Landscape" "Portrait") nil t default-direction)
            '(("Landscape" . t) ("Portrait" . nil)))))
        (let* ((prompt (format "Title : "))
               (completion-ignore-case t)
               (default (if ps-print-header "t" "nil")))
          (cdr
           (assoc (completing-read prompt '("t" "nil") nil t default)
                  '(("t" . t) ("nil" . nil)))))
        (let* ((prompt (format "Font Rescale Factor : "))
               (factor)
               (default (number-to-string pdf-preview-font-rescale-factor)))
          (while (not (numberp (setq factor (read-minibuffer prompt default)))))
          (eval factor))
        (let* ((prompt (format "Line Spacing : "))
               (spacing)
               (default (number-to-string ps-line-spacing)))
          (while (not (numberp (setq spacing (read-minibuffer prompt default)))))
          (eval spacing))
        )))

(defun pdf-preview-multiple-cons_cell (cons_cell factor)
  (if (consp cons_cell)
      (cons (* factor (car cons_cell)) (* factor (cdr cons_cell)))
    (* cons_cell factor)))

(defun pdf-preview-spool (pdf-preview-spool-function arg-list)
  (save-excursion
    (setq arg-list
          (if arg-list arg-list
            (list ps-paper-type ps-landscape-mode ps-print-header
                  pdf-preview-font-rescale-factor ps-line-spacing)))
    (let*
        ((ps-paper-type (nth 0 arg-list))
         (ps-landscape-mode (nth 1 arg-list))
         (ps-print-header (nth 2 arg-list))
         (factor (nth 3 arg-list))
         (ps-font-size (pdf-preview-multiple-cons_cell ps-font-size factor))
         (ps-header-font-size
          (pdf-preview-multiple-cons_cell ps-header-font-size factor))
         (ps-header-title-font-size
          (pdf-preview-multiple-cons_cell ps-header-title-font-size factor))
         (ps-footer-font-size
          (pdf-preview-multiple-cons_cell ps-footer-font-size factor))
         (ps-line-number-font-size
          (pdf-preview-multiple-cons_cell ps-line-number-font-size factor))
         (ps-line-spacing (nth 4 arg-list))
         (ps-multibyte-buffer 'non-latin-printer))
      (funcall pdf-preview-spool-function))
    ))

(provide 'pdf-preview)

;;; pdf-preview.el ends here