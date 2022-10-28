;;; npm i -g dockerfile-language-server-nodejs
(add-hook 'dockerfile-mode-hook #'eglot-ensure)
