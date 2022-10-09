(with-eval-after-load 'lsp-mode
  (setq lsp-idle-delay 1.000)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-document-sync-method nil)
  ;; (setq lsp-log-io t)
  ;; (add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)
  (with-eval-after-load 'lsp-diagnostics
    (setq lsp-diagnostics-provider :auto))
  (with-eval-after-load 'lsp-completion
    (defun my/orderless-for-lsp-mode ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
	    '(orderless)))

    (add-hook 'lsp-completion-mode-hook #'my/orderless-for-lsp-mode)
    (setq lsp-completion-enable t)
    (setq lsp-completion-enable-additional-text-edit t)
    (setq lsp-completion-provider :none)
    ;; see https://github.com/emacs-lsp/lsp-mode/issues/2563#issuecomment-767987191
    ;; (advice-add #'lsp-completion--regex-fuz :override #'identity)
    ))
