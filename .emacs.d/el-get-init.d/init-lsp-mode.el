(setq lsp-enable-file-watchers nil)
(with-eval-after-load-feature 'lsp
  ;; general
  (setq lsp-auto-guess-root t)
  (setq lsp-document-sync-method 'incremental) ;; always send incremental document
  (setq lsp-response-timeout 5)
  (setq lsp-diagnostics-provider :auto)
  (setq lsp-completion-enable t)
  (setq lsp-completion-enable-additional-text-edit nil)
  (setq lsp-prefer-capf t))
;; (add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)
