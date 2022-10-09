(add-hook 'after-init-hook
          #'(lambda ()
              (global-corfu-mode)
              (add-hook 'corfu-mode-hook #'corfu-doc-mode)))
(with-eval-after-load 'corfu
  (setq corfu-auto t)
  (setq corfu-auto-prefix 1)
  (setq corfu-echo-documentation nil)
  ;; (setq corfu-separator ?\s)
  (setq corfu-quit-at-boundary nil)
  ;; (setq corfu-quit-no-match nil)
  (with-eval-after-load 'orderless
    (defun my/orderless-dispatch-flex-first (_pattern index _total)
      (and (eq index 0) 'orderless-flex))
    (defun my/orderless-for-corfu ()
      (setq-local orderless-style-dispatchers '(my/orderless-dispatch-flex-first)))))
