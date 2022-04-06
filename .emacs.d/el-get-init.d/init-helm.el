(with-eval-after-load 'helm
  (define-key helm-map (kbd "C-v") 'helm-next-source)
  (define-key helm-map (kbd "M-v") 'helm-previous-source)
  (define-key helm-map (kbd "C-x C-j") 'skk-kakutei)
  (define-key helm-map (kbd "C-z") 'helm-execute-persistent-action)

  (defun helm-mac-spotlight ()
    "Preconfigured `helm' for `mdfind'."
    (interactive)
    (let ((helm-ff-transformer-show-only-basename nil))
      (helm-other-buffer 'helm-source-mac-spotlight "*helm mdfind*")))
  (with-eval-after-load 'migemo
    (helm-migemo-mode 1)))

(defconst helm-for-files-preferred-list
  '(helm-source-buffers-list
    helm-source-recentf
    helm-source-file-cache
    helm-source-files-in-current-dir
    helm-source-mac-spotlight))
(setq helm-buffer-max-length 40
      helm-c-ack-thing-at-point 'symbol
      helm-ff-auto-update-initial-value nil
      ;; helm-grep-default-recurse-command "ggrep -a -d recurse %e -n%cH -e %p %f"
      helm-input-idle-delay 0.2
      helm-mode t
      helm-truncate-lines t)

(with-eval-after-load 'helm-grep
  ;; use ripgrep https://github.com/BurntSushi/ripgrep
  (when (executable-find "rg")
    (setq helm-grep-ag-command "rg --color=always -S --no-heading --line-number %s %s %s")))
(defadvice helm-grep-highlight-match
    (around ad-helm-grep-highlight-match activate)
  (ad-set-arg 1 t)
  ad-do-it)

(setq helm-gtags-ignore-case t)
(setq helm-gtags-path-style 'relative)
;; grep for euc-jp.
;; (setq helm-grep-default-command "grep -a -d skip %e -n%cH -e `echo %p | lv -Ia -Oej` %f | lv -Os -Ia ")
;; (setq helm-grep-default-recurse-command "grep -a -d recurse %e -n%cH -e `echo %p | lv -Ia -Oej` %f | lv -Os -Ia ")


(add-hook 'helm-gtags-mode-hook
          #'(lambda ()
              (local-set-key (kbd "M-.") 'helm-gtags-find-tag)))

(global-set-key (kbd "C-;") 'helm-for-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-z C-r") 'helm-resume)
(global-set-key (kbd "C-z C-f") 'helm-mac-spotlight)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-h b") 'helm-descbinds)

;;;
;;; see http://www49.atwiki.jp/ntemacs/pages/32.html
;;;

(defadvice helm-reduce-file-name (around ad-helm-reduce-file-name activate)
  (let ((fname (ad-get-arg 0))
        (level (ad-get-arg 1)))
    (while (> level 0)
      (setq fname (expand-file-name (concat fname "/../")))
      (setq level (1- level)))
    (setq ad-return-value fname)))

(defadvice helm-completing-read-default-1 (around ad-helm-completing-read-default-1 activate)
  (if (listp (ad-get-arg 4))
      (ad-set-arg 4 (car (ad-get-arg 4))))
  (cl-letf (((symbol-function 'regexp-quote)
             (symbol-function 'identity)))
    ad-do-it))

(defadvice find-file (around ad-find-file activate)
  (let ((current-prefix-arg nil))
    ad-do-it))

(defun helm-howm-do-ag ()
  (interactive)
  (helm-grep-ag-1 howm-directory))
;; use for grep
(defun helm-howm-do-grep ()
  (interactive)
  (helm-do-grep-1
   (list (car (split-string howm-directory "\n"))) '(4) nil '("*.txt" "*.md")))
(global-set-key (kbd "C-z s") 'helm-howm-do-grep)
(global-set-key (kbd "C-z x") 'helm-howm-do-ag)
