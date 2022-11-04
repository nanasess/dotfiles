(add-hook 'prog-mode-hook 'copilot-mode)
(defun copilot-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (indent-for-tab-command)))
(with-eval-after-load 'copilot
  (define-key copilot-mode-map (kbd "TAB") #'copilot-tab)
  (define-key copilot-mode-map (kbd "<tab>") #'copilot-tab)
  (define-key copilot-mode-map (kbd "M-n") #'copilot-next-completion)
  (define-key copilot-mode-map (kbd "M-p") #'copilot-previous-completion))
