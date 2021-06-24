;; npm i -g vscode-json-languageserver
(add-hook 'json-mode-hook #'lsp-deferred)
;; for json format
;; see https://qiita.com/saku/items/d97e930ffc9ca39ac976
(defun jq-format (beg end)
  (interactive "r")
  (shell-command-on-region beg end "jq ." nil t))
