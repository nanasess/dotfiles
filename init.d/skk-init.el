;; -*- emacs-lisp -*-
;; dot.skk
;;

(require 'skk-study)
;; (setq skk-server-inhibit-startup-server nil)
;; (setq skk-server-prog "dbskkd-cdb-tcp.sh")
(setq skk-server-host "localhost")
(setq skk-server-portnum 1178)
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
  (skk-save-jisyo)
  (skk-study-save))
(run-with-idle-timer skk-auto-save-interval t 'skk-auto-save)
