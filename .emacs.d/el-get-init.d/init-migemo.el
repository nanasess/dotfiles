(defvar migemo-dictionary
  (concat external-directory "migemo/dict/utf-8/migemo-dict"))
(when (file-exists-p migemo-dictionary)
  (setq migemo-command "cmigemo"
        migemo-options '("-q" "--emacs" "-i" "\a")
        migemo-user-dictionary nil
        migemo-regex-dictionary nil
        migemo-use-pattern-alist t
        migemo-use-frequent-pattern-alist t
        migemo-pattern-alist-length 10000
        migemo-coding-system 'utf-8-unix))
(add-hook 'isearch-mode-hook #'(lambda ()
                                 (unless (featurep 'migemo)
                                   (require 'migemo))
                                 (migemo-init)))
