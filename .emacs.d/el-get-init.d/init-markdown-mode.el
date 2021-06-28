(add-to-list 'auto-mode-alist '("\\.\\(markdown\\|md\\)\\'" . gfm-mode))
;; see also http://stackoverflow.com/questions/14275122/editing-markdown-pipe-tables-in-emacs
(defun cleanup-org-tables ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "-+-" nil t) (replace-match "-|-"))))

(add-hook 'markdown-mode-hook
          #'(lambda()
              (require 'org)
              (orgtbl-mode 1)
              (add-hook 'after-save-hook 'cleanup-org-tables  nil 'make-it-local)))

(with-eval-after-load-feature 'org-table
  (add-hook 'markdown-mode-hook
            #'(lambda()
                (define-key orgtbl-mode-map
                  (kbd "<backspace>") 'delete-backward-char)))
  (add-hook 'gfm-mode-hook
            #'(lambda()
                (define-key orgtbl-mode-map
                  (kbd "<backspace>") 'delete-backward-char))))
