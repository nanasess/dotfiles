;; -*- emacs-lisp -*-
;; dot.skk
;;

(setq skk-cdb-large-jisyo nil)
(setq skk-large-jisyo (concat external-directory "ddskk/SKK-JISYO.all.utf8"))
(setq skk-jisyo-code 'utf-8)

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
(setq skk-save-jisyo-instantly t)
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
(defun toggle-skk-kutouten ()
  "toggle skk-kutoten-type."
  (interactive)
  (cond ((eq skk-kutouten-type 'en)
	 (setq skk-kutouten-type 'jp))
	((setq skk-kutouten-type 'en)))
  (message (format "skk-kutoten-type on set to the %s." skk-kutouten-type)))

;;; skk-search-web
(require 'skk-search-web)
(add-to-list 'skk-search-prog-list
	     '(skk-search-web 'skk-google-cgi-api-for-japanese-input)
	     t)
;;(add-to-list 'skk-completion-prog-list '(skk-comp-google) t)
