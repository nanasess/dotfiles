(with-eval-after-load 'consult
  (global-set-key (kbd "C-;") 'consult-buffer)
  (global-set-key [remap goto-line] 'consult-goto-line)
  (global-set-key (kbd "C-M-s") 'consult-line)
  (defun consult-howm-do-ag ()
    (interactive)
    (consult-ripgrep howm-directory))
  (global-set-key (kbd "C-z s") 'consult-howm-do-ag)
  (global-set-key (kbd "C-z l") 'consult-ls-git))
