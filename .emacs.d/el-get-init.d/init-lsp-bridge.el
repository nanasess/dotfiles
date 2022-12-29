(add-hook 'after-init-hook
          #'(lambda ()
              (global-lsp-bridge-mode)))
(with-eval-after-load 'lsp-bridge
  (setq lsp-bridge-php-lsp-server "phpactor")
  (setq lsp-bridge-python-lsp-server "pyright")
  (setq acm-candidate-match-function 'orderless-flex)
  (setq lsp-bridge-enable-hover-diagnostic t)
  (setq acm-enable-doc-markdown-render t)
  ;; (setq lsp-bridge-enable-log t)
  ;; (setq lsp-bridge-enable-debug t)
  ;; (setq lsp-bridge-signature-show-function 'lsp-bridge-signature-posframe)
  ;; (setq acm-enable-tabnine t)
  (global-set-key [remap xref-find-definitions] #'lsp-bridge-find-def)
  (global-set-key [remap xref-pop-marker-stack] #'lsp-bridge-find-def-return)
  (global-set-key (kbd "M-.") #'lsp-bridge-find-def)
  (global-set-key (kbd "M-,") #'lsp-bridge-find-def-return)
  (global-set-key (kbd "M-n") #'lsp-bridge-diagnostic-jump-next)
  (global-set-key (kbd "M-p") #'lsp-bridge-diagnostic-jump-prev)
  (global-set-key (kbd "C-z i") #'lsp-bridge-diagnostic-list))
