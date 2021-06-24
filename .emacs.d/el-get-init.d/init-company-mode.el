(global-set-key (kbd "C-M-i") 'company-complete)
;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(with-eval-after-load-feature 'company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-selection-wrap-around t)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)

  ;; C-sで絞り込む
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)

  ;; TABで候補を設定
  (define-key company-active-map (kbd "C-i") 'company-complete-selection)
  (defun company-backends-with-yas ()
    (interactive)
    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))))

(with-eval-after-load-feature 'company-dabbrev
  (setq company-dabbrev-downcase nil))

(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (make-local-variable 'company-backends)
              (push '(company-elisp :with company-yasnippet) company-backends)))
(add-hook 'after-init-hook 'global-company-mode)
