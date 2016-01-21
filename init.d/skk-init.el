;; -*- emacs-lisp -*-
;; dot.skk
;;

(setq skk-cdb-large-jisyo nil)
(setq skk-large-jisyo (concat external-directory "ddskk/SKK-JISYO.L"))

(setq skk-extra-jisyo-file-list
      (list
       ;; XXX external-directory をうまく展開できない
       '("~/GoogleDrive/share/ddskk/SKK-JISYO.JIS3_4" . euc-jisx0213)
       '("~/GoogleDrive/share/ddskk/SKK-JISYO.JIS2004" . euc-jisx0213)
       (format "%s" (concat external-directory "ddskk/SKK-JISYO.JIS2"))
       (format "%s" (concat external-directory "ddskk/SKK-JISYO.assoc"))
       (format "%s" (concat external-directory "ddskk/SKK-JISYO.edict"))
       (format "%s" (concat external-directory "ddskk/SKK-JISYO.fullname"))
       (format "%s" (concat external-directory "ddskk/SKK-JISYO.geo"))
       (format "%s" (concat external-directory "ddskk/SKK-JISYO.itaiji"))
       (format "%s" (concat external-directory "ddskk/SKK-JISYO.jinmei"))
       (format "%s" (concat external-directory "ddskk/SKK-JISYO.law"))
       (format "%s" (concat external-directory "ddskk/SKK-JISYO.lisp"))
       (format "%s" (concat external-directory "ddskk/SKK-JISYO.mazegaki"))
       (format "%s" (concat external-directory "ddskk/SKK-JISYO.okinawa"))
       (format "%s" (concat external-directory "ddskk/SKK-JISYO.propernoun"))
       (format "%s" (concat external-directory "ddskk/SKK-JISYO.pubdic+"))
       (format "%s" (concat external-directory "ddskk/SKK-JISYO.station"))
       (format "%s" (concat external-directory "ddskk/zipcode/SKK-JISYO.zipcode"))
       (format "%s" (concat external-directory "ddskk/zipcode/SKK-JISYO.office.zipcode"))))

(setq-default skk-kutouten-type 'en)
(setq-default skk-kuten-touten-alist
	      '((jp "。" . "、")
		(en "." . ",")
		(jp-en "。" . "，")
		(en-jp "．" . "、")))
(setq skk-show-tooltip t)
(setq skk-verbose t)
(setq skk-show-inline 'vertical)
(setq skk-use-jisx0201-input-method t)
(setq skk-share-private-jisyo t)
(setq skk-use-face t)
(setq skk-use-color-cursor t)
(setq skk-japanese-message-and-error nil)
(setq skk-show-annotation t)
(setq skk-auto-start-henkan t)
(setq skk-show-icon t)
(setq skk-dcomp-activate nil)
(setq skk-egg-like-newline nil)
(setq skk-auto-insert-paren t)
(setq skk-rom-kana-rule-list
           (append skk-rom-kana-rule-list
                   '(("@" nil "@"))))
(defvar skk-auto-save-interval 30)
(defun toggle-skk-kutouten ()
  "toggle skk-kutoten-type."
  (interactive)
  (cond ((eq skk-kutouten-type 'en)
	 (setq skk-kutouten-type 'jp))
	((setq skk-kutouten-type 'en)))
  (message (format "skk-kutoten-type on set to the %s." skk-kutouten-type)))
(defun skk-auto-save ()
  "auto save of the skk-jisyo and skk-study."
  (skk-save-jisyo))
  ;; (skk-study-save))
(run-with-idle-timer skk-auto-save-interval t 'skk-auto-save)
