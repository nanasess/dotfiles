(with-eval-after-load 'mermaid-mode
  (setq mermaid-output-format ".pdf")
  (define-key mermaid-mode-map (kbd "TAB") 'mermaid-indent-line)
  (define-key mermaid-mode-map (kbd "<tab>") 'mermaid-indent-line))
