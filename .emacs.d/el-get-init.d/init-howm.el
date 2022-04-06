(with-eval-after-load-feature 'howm
  (setq howm-template
        (concat howm-view-title-header
                (concat
                 " %title%cursor\n"
                 "Date: %date\n\n"
                 "%file\n\n")
                (concat
                 "<!--\n"
                 "  Local Variables:\n"
                 "  mode: gfm\n"
                 "  coding: utf-8-unix\n"
                 "  End:\n"
                 "-->\n")))
  (defun howm-save-and-kill-buffer ()
    "kill screen when exiting from howm-mode"
    (interactive)
    (let* ((file-name (buffer-file-name)))
      (when (and file-name (string-match "\\.txt" file-name))
        (if (save-excursion
              (goto-char (point-min))
              (re-search-forward "[^ \t\r\n]" nil t))
            (howm-save-buffer)
          (set-buffer-modified-p nil)
          (when (file-exists-p file-name)
            (delete-file file-name)
            (message "(Deleted %s)" (file-name-nondirectory file-name))))
        (kill-buffer nil))))
  (add-hook 'howm-mode-hook
            #'(lambda ()
                (define-key howm-mode-map (kbd "C-c C-q") 'howm-save-and-kill-buffer)))
  (when (executable-find "rg")
    (setq howm-view-use-grep t)
    (setq howm-view-grep-command "rg")
    (setq howm-view-grep-option "-nH --no-heading --color never")
    (setq howm-view-grep-extended-option nil)
    (setq howm-view-grep-fixed-option "-F")
    (setq howm-view-grep-expr-option nil)
    (setq howm-view-grep-file-stdin-option nil))

  (defun parse-howm-title ()
    (let* ((file-name (buffer-file-name)))
      (when (and file-name (string-match "\\.txt" file-name))
        (if (save-excursion
              (goto-char (point-min))
              (re-search-forward "^Title: \\(.*\\)$" nil t))
            (match-string 1)))))

  ;; see https://stackoverflow.com/a/384346
  (defun rename-file-howm-title ()
    (interactive)
    (let ((name (buffer-name))
          (filename (buffer-file-name))
          (new-name (parse-howm-title))
          (new-filename (format "%s.txt" (parse-howm-title))))
      (if (not (string-empty-p new-name))
          (if (not (string= new-filename "nil.txt"))
              (if (not filename)
                  (message "Buffer '%s' is not visiting a file!" name)
                (if (get-buffer new-filename)
                    (message "A buffer named '%s' already exists!" new-name)
                  (progn
                    (rename-file filename new-filename 1)
                    (rename-buffer new-filename)
                    (set-visited-file-name new-filename)
                    (set-buffer-modified-p nil))))))))
  (add-hook 'howm-mode-hook
            #'(lambda ()
                (add-hook 'before-save-hook 'rename-file-howm-title nil 'local))))

(autoload 'howm-mode "howm" "Hitori Otegaru Wiki Modoki" t)
(autoload 'howm-create "howm" "Hitori Otegaru Wiki Modoki" t)

;;; Add howm-directory/.dir-locals
;;
;; ((nil
;;   (eval
;;    (lambda ()
;;      (when (string-match "\\.txt" (file-name-nondirectory buffer-file-name))
;;        (howm-mode)
;;        (gfm-mode))))))

;; see https://stackoverflow.com/a/384346
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(global-set-key (kbd "C-z c") 'howm-create)
(global-set-key (kbd "C-c ,c") 'howm-create)
