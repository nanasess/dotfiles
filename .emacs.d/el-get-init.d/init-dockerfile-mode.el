;;; npm i -g dockerfile-language-server-nodejs
(add-hook 'dockerfile-mode-hook #'company-backends-with-yas)
(add-hook 'dockerfile-mode-hook #'lsp-deferred)
