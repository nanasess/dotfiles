(add-to-list 'load-path (concat user-emacs-directory ".wakatime.d"))
(load "wakatime-config" t t)
(add-hook 'after-init-hook 'global-wakatime-mode)
