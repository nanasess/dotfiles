(server-start)

;; grep-find で .svn を除外
(setq grep-find-command 
      (cons (concat "find . -type f -name '*.svn*' -prune -o -exec "
		    "/usr/xpg4/bin/grep -n -e  {} /dev/null \\;") 72))

(provide 'usg-unix-v-init)
