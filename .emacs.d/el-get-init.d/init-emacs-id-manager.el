;; use gpg2 http://blog.n-z.jp/blog/2016-08-20-mac-easypg-gpg2.html
(setq idm-database-file (concat external-directory ".idm-db.gpg"))
(setq idm-copy-action 'kill-new)
(setq idm-gen-password-cmd mkpasswd-command)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(setenv "GPG_AGENT_INFO" nil)
