(with-eval-after-load-feature 'lsp-ui
  ;; lsp-ui-doc
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-position 'top) ;; top, bottom, or at-point
  ;; (setq lsp-ui-doc-max-width 150)
  ;; (setq lsp-ui-doc-max-height 30)
  (setq lsp-ui-doc-use-childframe t)
  (setq lsp-ui-doc-use-webkit t)
  (setq lsp-ui-doc-delay 1.0)
  (setq lsp-ui-sideline-delay 1.0)
  (setq lsp-ui-doc-post-delay 0.8)
  ;; lsp-ui-sideline
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-ignore-duplicate t)
  (setq lsp-ui-sideline-show-symbol t)
  (setq lsp-ui-sideline-show-hover t)
  ;; (setq lsp-ui-sideline-show-diagnostics nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  ;; lsp-ui-imenu
  (setq lsp-ui-imenu-enable nil)
  (setq lsp-ui-imenu-kind-position 'top)
  ;; lsp-ui-peek
  (setq lsp-ui-peek-enable t)
  ;; (setq lsp-ui-peek-peek-height 20)
  ;; (setq lsp-ui-peek-list-width 50)
  (setq lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
  (defun ladicle/toggle-lsp-ui-doc ()
    (interactive)
    (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
      (lsp-ui-doc-mode 1))))
;; (setq lsp-headerline-breadcrumb-enable nil)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
