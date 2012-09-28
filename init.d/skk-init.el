;; -*- emacs-lisp -*-
;; dot.skk
;;

(require 'skk-study)
;; (setq skk-server-inhibit-startup-server nil)
;; (setq skk-server-prog "dbskkd-cdb-tcp.sh")
;; (setq skk-server-host "localhost")
;; (setq skk-server-portnum 1178)
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
(setq skk-dcomp-activate t)
(setq skk-egg-like-newline nil)
(setq skk-auto-insert-paren t)
(setq skk-rom-kana-rule-list
           (append skk-rom-kana-rule-list
                   '(("@" nil "@"))))
(setq skk-jisyo-code "euc-jisx0213-unix")
