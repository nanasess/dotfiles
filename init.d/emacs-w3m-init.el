;; -*- emacs-lisp -*-
;; dot.emacs-w3m
;;

(setq w3m-use-cookies t)
(setq w3m-cookie-accept-bad-cookies t)
(setq w3m-broken-proxy-cache t)
(setq w3m-bookmark-file (concat howm-directory "bookmark.html"))
(setq w3m-bookmark-file-coding-system "utf-8-unix")
(define-key w3m-mode-map (kbd "C-t R") nil)
(define-key w3m-mode-map (kbd "C-t a") nil)
(define-key w3m-mode-map (kbd "C-t") nil)
