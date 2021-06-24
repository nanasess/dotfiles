(add-hook 'dired-mode-hook
          #'(lambda ()
              (dired-async-mode 1)))
