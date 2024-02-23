(add-to-list 'auto-mode-alist '("\\.\\(inc\\|php[s34]?\\)$" . php-ts-mode))
(with-eval-after-load 'php-ts-mode
  (with-eval-after-load 'lsp-bridge
    (add-hook 'php-ts-mode-hook #'(lambda ()
                                    (push '(php-ts-mode . lsp-bridge-php-lsp-server) lsp-bridge-single-lang-server-mode-list)
                                    (lsp-bridge-mode 1))))
  (add-hook 'php-mode-hook 'editorconfig-apply)
  (electric-indent-local-mode t)
  (electric-layout-mode t)
  ;; (setq-local electric-layout-rules '((?{ . around)))
  (electric-pair-local-mode t))
