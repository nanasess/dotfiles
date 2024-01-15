(with-eval-after-load 'markdown-mode
  (add-to-list 'auto-mode-alist '("\\.\\(markdown\\|md\\)\\'" . gfm-mode))
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-header-scaling t)
  (setq markdown-indent-on-enter 'indent-and-new-item)
  (define-key markdown-mode-map (kbd "<S-tab>") #'markdown-shifttab))
