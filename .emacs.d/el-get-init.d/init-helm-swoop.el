;; (cl-defun helm-swoop-nomigemo (&key $query ($multiline current-prefix-arg))
;;   (interactive)
;;   (let (helm-migemo-mode)
;;     (helm-swoop :$query $query :$multiline $multiline)))

;; (defun isearch-forward-or-helm-swoop-or-helm-occur (use-helm-swoop)
;;   (interactive "p")
;;   (let (current-prefix-arg
;;         (helm-swoop-pre-input-function 'ignore))
;;     (call-interactively
;;      (case use-helm-swoop
;;        (1 'isearch-forward)             ; C-s
;;        (4 (if (< 1000000 (buffer-size)) 'helm-occur 'helm-swoop)) ; C-u C-s
;;        (16 'helm-swoop-nomigemo)))))                              ; C-u C-u C-s
;; (global-set-key (kbd "C-s") 'isearch-forward-or-helm-swoop-or-helm-occur)
