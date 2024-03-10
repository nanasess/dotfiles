(add-hook 'emacs-startup-hook
          #'(lambda ()
              (global-undo-tree-mode)))
(setopt undo-tree-visualizer-timestamps t)
(setopt undo-tree-visualizer-diff t)
(setopt undo-tree-auto-save-history t)
(setopt undo-tree-enable-undo-in-region t)
(setopt undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo-tree" user-emacs-directory))))
