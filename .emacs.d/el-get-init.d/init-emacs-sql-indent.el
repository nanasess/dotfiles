(add-hook 'sql-mode-hook #'(lambda ()
                             (set (make-local-variable 'sql-product) 'sqlite)
                             (sql-indent-enable)
                             (setq sqlind-basic-offset 4)))
