;; npm i -g yaml-language-server
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook #'eglot-ensure)
