(add-hook 'emacs-startup-hook
          #'(lambda ()
              ;; (setq lsp-bridge-enable-mode-line nil)
              (global-lsp-bridge-mode)))
(with-eval-after-load 'lsp-bridge
  (push '(terraform-mode . "terraform-lsp") lsp-bridge-single-lang-server-mode-list)
  (push 'terraform-mode lsp-bridge-default-mode-hooks)
  (defun sm-try-smerge ()
    "Searches for merge conflict markers and disables lsp-bridge-mode if found."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
  	(lsp-bridge-mode -1))))
  (add-hook 'lsp-bridge-mode-hook 'sm-try-smerge t)
  (defun lsp-bridge--mode-line-format ()
    "Compose the LSP-bridge's mode-line."
    (setq-local mode-face
                (if (lsp-bridge-epc-live-p lsp-bridge-epc-process)
                    'lsp-bridge-alive-mode-line
                  'lsp-bridge-kill-mode-line))

    (when lsp-bridge-server
      (propertize "橋"'face mode-face)))
  (setq lsp-bridge-php-lsp-server "phpactor")
  (setq lsp-bridge-python-lsp-server "pyright")
  (setq lsp-bridge-csharp-lsp-server "omnisharp-dotnet")
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
  (global-set-key (kbd "C-z i") #'lsp-bridge-diagnostic-list)
  (define-key acm-mode-map (kbd "<tab>") nil))
