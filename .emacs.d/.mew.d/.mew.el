;; Mew Setting

 ;; format-flowed
 (setq mew-use-format-flowed t)

 ;; 未読マーク
 (setq mew-use-unread-mark t)

 ;; summary-mode
 (setq mew-summary-form
       '(type (5 date) " " (19 from) " " t (30 subj) "|" (20 body)))
 (setq mew-addrbook-for-summary 'name)

 ;; マスターパスワード
 (setq mew-use-master-passwd t)

 ;; draft mode は auto-save しない
 (setq mew-draft-mode-auto-save -1)

;; マークがついていない場合のみ refile
(setq mew-refile-auto-refile-skip-any-mark t)

(defun mew-browse-url-open-gmail()
  (interactive)
  (message (mew-sinfo-get-case))
  (browse-url (concat "https://mail.google.com/a/"
		      (mew-mail-domain (mew-sinfo-get-case)))))
;; URL クリック有効
(define-key mew-message-mode-map [M-down-mouse-1] 'mew-browse-url-at-mouse)
(define-key mew-draft-mode-map [M-down-mouse-1] 'mew-browse-url-at-mouse)
(define-key mew-draft-mode-map [ns-drag-file] 'mew-draft-dnd-handle-file)
(define-key mew-message-mode-map [return] 'mew-browse-url-at-point)
(define-key mew-summary-mode-map (kbd "C-c RET") 'mew-browse-url-open-gmail)

;; 検索エンジンは Hyper Estraier
(setq mew-search-method 'est)
(setq mew-use-suffix t)

;; SSL settings
(setq mew-prog-ssl "stunnel")
(setq mew-ssl-verify-level 0)
;; (setq mew-prog-pgp "C:/cygwin64/usr/local/bin/gpg.exe")

;; emacs-w3m settings
;; (condition-case nil
;;     (require 'mew-w3m)
;;   (file-error nil))


;; (setq mew-mime-multipart-alternative-list
;;       '("Text/Html" "Text/Plain" ".*"))

;; 起動時に自動取得しない
;(setq mew-auto-get nil)

(setq mew-thread-indent-strings [" +" " +" " |" "  "])
(setq mew-mailbox-type 'mbox)
(setq mew-mbox-command "incm")
(setq mew-mbox-command-arg "-u -d $HOME/Maildir")

(setq mew-use-biff t)
(setq mew-pop-biff-interval 1)
(setq mew-use-cached-passwd t)
(setq mew-passwd-lifetime 30)

;; dnd
(define-key mew-draft-mode-map [ns-drag-file]
  (lambda ()
    (interactive)
    (let ((f))
      (setq f (car ns-input-file))
      (setq ns-input-file (cdr ns-input-file))
      (dnd-handle-one-url (get-buffer-window)
			  'copy
			  (concat "file://" f)))))

;; See Also.
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=9610#31
(add-hook 'mew-summary-mode-hook
	  '(lambda ()
	     (setq bidi-paragraph-direction 'left-to-right)))
;; http://www.mew.org/pipermail/mew-dist/2012-June/029210.html
