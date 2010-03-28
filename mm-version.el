;;; mm-version.el --- Mule and Meadow convert code-name

;; Copyright

;; Author:
;; Keywords: Mule Meadow version code-name

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; To convert Mule and Meadow code-name to Japanese-Kanji.

;; Mule と Meadow の code-name を漢字に変換します。
;; http://www.ki.nu/software/emacs-20/mule-version.el
;; にちょっと手を入れただけで、とても冗長です。(^^;

;; SEMI FLIM などには対応していませんので、使える MUA は Mew くらい
;; でしょう。
;; 不満な方は、ftp://ftp.fan.gr.jp/pub/elisp/rail をどうぞ。(^^;;

;; ~/.emacs に
;; (require 'mm-version)
;; とするだけです。

;; code-name が追加された場合は
;; 1) 追加された code-name のみを 下記の要領で ~/.emacs に 加えるか、
;; (setq meadow-code-name-alist
;;       (append '(("foo" . "風")
;;                 ("bar" . "婆"))
;;              meadow-code-name-alist))
;;
;; 2) meadow-code-name-alist に直接追加して下さい。

;;; Code:

(defvar meadow-code-name-alist
  '(("MIDORI"             . "翠")
    ("MIDORI-NO-TAMA"     . "翠之珠")
    ("TSUYU"              . "露")
    ("KASURI"             . "絣")
    ("FUDI-GASANE"	  . "藤襲")
    ("TSUTSUJI"           . "躑躅")
    ("TACHIBANA"          . "橘")
    ("HIINA"              . "雛")
    ("FUTAAWI"            . "二藍")
    ("EBIZOME"            . "葡萄染")
    ("HACHISU"            . "蓮")
    ("SASHINUKI"          . "指貫")
    ("TANAHASHI"          . "棚橋")
    ("KAKITSUBATA"        . "杜若")
    ("UKIHASHI"           . "浮橋")
    ("AWSAKA"             . "逢坂")
    ("SHOUBU"             . "菖蒲")
    ("NADESHIKO"          . "撫子")
    ("WOMINAHESHI"        . "女郎花")
    ("KIKYOU"             . "桔梗")
    ("ASAGAO"             . "朝顔")
    ))

(defvar mule-code-name-alist
  '(("KIRITSUBO"          . "桐壺")
    ("HAHAKIGI"           . "帚木")
    ("UTSUSEMI"           . "空蝉")
    ("YUUGAO"             . "夕顔")
    ("WAKAMURASAKI"       . "若紫")
    ("SUETSUMUHANA"       . "末摘花")
    ("MOMIJINOGA"         . "紅葉賀")
    ("HANANOEN"           . "花宴")
    ("AOI"                . "葵")
    ("SAKAKI"             . "賢木")
    ("HANACHIRUSATO"      . "花散里")
    ("SUMA"               . "須磨")
    ("AKASHI"             . "明石")
    ("MIOTSUKUSHI"        . "澪標")
    ("YOMOGIU"            . "蓬生")
    ("SEKIYA"             . "関屋")
    ("EAWASE"             . "絵合")
    ("MATSUKAZE"          . "松風")
    ("USUGUMU"            . "薄雲")
    ("ASAGAO"             . "朝顔")
    ("OTOME"              . "少女")
    ("TAMAKAZURA"         . "玉鬘")
    ("HATSUNE"            . "初音")
    ("KOCHOU"             . "胡蝶")
    ("HOTARU"             . "蛍")
    ("TOKONATSU"          . "常夏")
    ("KAGARIBI"           . "篝火")
    ("NOWAKE"             . "野分")
    ("MIYUKI"             . "行幸")
    ("FUJIBAKAMA"         . "藤袴")
    ("MAKIBASHIRA"        . "真木柱")
    ("UMEGAE"             . "梅枝")
    ("FUJINOURABA"        . "藤裏葉")
    ("WAKANA-KAMI"        . "若菜上")
    ("WAKANA-SHIMO"       . "若菜下")
    ("KASHIWAGI"          . "柏木")
    ("YOKOBUE"            . "横笛")
    ("SUZUMUSHI"          . "鈴虫")
    ("YUUGIRI"            . "夕霧")
    ("MINORI"             . "御法")
    ("MABOROSHI"          . "幻")
    ("KUMOGAKURE"         . "雲隠")
    ("NIOUMIYA"           . "匂宮")
    ("KOUBAI"             . "紅梅")
    ("TAKEGAWA"           . "竹河")
    ("HASHIHIME"          . "橋姫")
    ("SHIIGAMOTO"         . "椎本")
    ("AGEMAKI"            . "総角")
    ("SAWARABI"           . "早蕨")
    ("YADORIGI"           . "宿木")
    ("AZUMAYA"            . "東屋")
    ("UKIFUNE"            . "浮舟")
    ("KAGEROU"            . "蜻蛉")
    ("TENARAI"            . "手習")
    ("YUMENOUKIHASHI"     . "夢浮橋")
    ))

(if (stringp mule-version)
  (progn
    (mapcar (function
      (lambda (args)
        (if (string-match (car args) mule-version)
            (setq mule-version
              (concat (substring mule-version 0
                             (match-beginning 0))
                           (cdr args)
                           (substring mule-version (match-end 0)))))))
	    mule-code-name-alist)))

(if (featurep 'meadow)
    (if (stringp (Meadow-version))
	(progn
	  (mapcar (function
		   (lambda (args)
		     (if (string-match (car args) (Meadow-version))
			 (setq meadow-version
			       (concat (substring (Meadow-version) 0
						  (match-beginning 0))
				       (cdr args)
				       (substring (Meadow-version) (match-end 0)))))))
		  meadow-code-name-alist))))

(provide 'mm-version)

;;; mm-version.el ends here