;; anything-howm-plugin.el
;;
(defun howm-title2file (howm-title howm-recent-menu-list)
  (if howm-recent-menu-list
      (progn
        (let* ((top-item (car howm-recent-menu-list))
               (list-item-name (car (cdr top-item)))
               (list-item-file (car top-item)))
          (if (string-equal howm-title list-item-name)
              list-item-file
            (howm-title2file howm-title (cdr howm-recent-menu-list)))
          ))))

(defun howm-get-recent-title-list (howm-recent-menu-list)
  (if howm-recent-menu-list
      (progn
        (let* ((top-item (car howm-recent-menu-list))
               (list-item-name (car (cdr top-item))))
          (append (list list-item-name)
                  (howm-get-recent-title-list (cdr howm-recent-menu-list)))
          ))
    ()))

(setq auto-mode-alist
      (append '(("\\.howm$" . howm-mode)) auto-mode-alist))

(defvar anything-c-howm-recent
  '((name . "howm Recent")
    (candidates . (lambda ()
                    (howm-get-recent-title-list (howm-recent-menu 10))))
    (action . (("Open howm file" .
                (lambda (howm-recent-title)
                  (find-file (howm-title2file howm-recent-title
                                              (howm-recent-menu 10)))))))
    ))

(provide 'anything-howm-plugin)