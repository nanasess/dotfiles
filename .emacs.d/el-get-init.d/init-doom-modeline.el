(with-eval-after-load-feature 'doom-modeline-core
  (with-eval-after-load-feature 'all-the-icons
    (add-hook 'doom-modeline-mode-hook
              #'(lambda ()
                  (setf (alist-get "\\.php$" all-the-icons-icon-alist nil nil #'equal)
                        '(all-the-icons-fileicon "php" :face all-the-icons-lpurple))
                  (setf (alist-get "\\.csx?$" all-the-icons-icon-alist nil nil #'equal)
                        '(all-the-icons-alltheicon "csharp-line" :face all-the-icons-dpurple))
                  (setf (alist-get 'php-mode all-the-icons-mode-icon-alist nil nil #'equal)
                        '(all-the-icons-fileicon "php" :face all-the-icons-lpurple))
                  (setf (alist-get 'csharp-mode all-the-icons-mode-icon-alist nil nil #'equal)
                        '(all-the-icons-alltheicon "csharp-line" :face all-the-icons-dpurple))
                  (doom-modeline-def-modeline 'main
                    '(workspace-name window-number modals matches buffer-info remote-host buffer-position parrot selection-info)
                    '(objed-state misc-info persp-name grip github debug lsp minor-modes indent-info buffer-encoding major-mode process vcs checker bar)))))
  (setq doom-modeline-vcs-max-length 999)
  (setq doom-modeline-buffer-file-name-style 'buffer-name))
(add-hook 'emacs-startup-hook 'doom-modeline-mode)
