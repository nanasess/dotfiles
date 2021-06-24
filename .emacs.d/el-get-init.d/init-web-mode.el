(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(with-eval-after-load-feature 'web-mode
  (setq web-mode-enable-block-face t)
                                        ;    (setq web-mode-enable-current-element-highlight nil)
  (setq web-mode-enable-current-column-highlight nil)
  (add-hook 'web-mode-hook
            #'(lambda ()
                (setq web-mode-enable-auto-indentation nil)))
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook
            #'(lambda ()
                (when (string-equal "tsx" (file-name-extension buffer-file-name))
                  (setup-tide-mode))))
  (add-hook 'web-mode-hook
            #'(lambda ()
                (when (string-equal "jsx" (file-name-extension buffer-file-name))
                  (setup-tide-mode))))
  (add-hook 'web-mode-hook
            #'(lambda ()
                (when (string-equal "vue" (file-name-extension buffer-file-name))
                  (setup-tide-mode))))
  (add-hook 'web-mode-hook
            #'(lambda ()
                (when (string-equal "tpl" (file-name-extension buffer-file-name))
                  (web-mode-set-engine "eccube"))))
  (add-hook 'web-mode-hook
            #'(lambda ()
                (make-local-variable 'company-backends)
                (push '(company-web-html :with company-yasnippet) company-backends)))
  (add-hook 'editorconfig-custom-hooks
            (lambda (hash) (setq web-mode-block-padding 0))))
