(with-eval-after-load-feature 'twittering-mode
  (setq twittering-status-format "%RT{%FACE[bold]{RT}}%i %s,  %@: %FACE[error]{%FIELD-IF-NONZERO[❤ %d]{favorite_count}}  %FACE[warning]{%FIELD-IF-NONZERO[↺ %d]{retweet_count}}\n%FOLD[  ]{%T // from %f%L%r%R%QT{\n+----\n%FOLD[|]{%i %s,  %@:\n%FOLD[  ]{%T // from %f%L%r%R}}\n+----}}\n ")
  (define-key twittering-mode-map
    (kbd "s") 'twittering-current-timeline)
  (define-key twittering-mode-map
    (kbd "w") 'twittering-update-status-interactive)
  (define-key twittering-edit-mode-map
    (kbd "C-c C-q") 'twittering-edit-cancel-status)
  (define-key twittering-edit-mode-map
    (kbd "C-u C-u") 'twittering-edit-replace-at-point))
(unless (load "twittering-tinyurl-api-key" t t)
  (defvar twittering-bitly-api-key nil))
