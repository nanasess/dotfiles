;;; phpactor/language-server-extension
;;; M-x lsp-phpactor-install-extension Phpstan
;; (defvar lsp-phpactor-path "~/.emacs.d/bin/phpactor")
(with-eval-after-load 'php-mode
  (add-to-list 'auto-mode-alist '("\\.\\(inc\\|php[s34]?\\)$" . php-mode))
  (add-hook 'php-mode-hook 'php-c-style)
  (add-hook 'php-mode-hook #'lsp-deferred)
  (add-hook 'php-mode-hook 'editorconfig-apply)
  (with-eval-after-load 'cc-engine
    (add-hook 'php-mode-hook
              #'(lambda ()
                  (setq c-auto-newline 1)
                  (setq c-hungry-delete-key 1)))))
(with-eval-after-load 'php
  (setq php-manual-url "https://www.php.net/manual/ja/"
        php-mode-coding-style 'Symfony2
        php-search-url "https://www.php.net/"))

(add-hook 'php-mode-hook 'php-c-style)
(add-hook 'php-mode-hook #'lsp-deferred)
(setq phpactor-install-directory (concat user-emacs-directory "el-get/phpactor"))
(setq phpactor--debug nil)
(setq company-phpactor-request-async t)
(defun php-c-style ()
  (interactive)
  (require 'php-skeleton)
  (require 'php-skeleton-exceptions)
  ;; (setq dap-php-debug-path "~/.vscode/extensions/felixfbecker.php-debug-1.14.12")
  ;; (setq dap-php-debug-program `("node",(f-join dap-php-debug-path "out/phpDebug.js")))
  ;; (require 'dap-php)
  ;; (require 'flycheck-phpstan)
  ;; (require 'php-ui-phpactor)
  ;; (require 'php-ui)
  ;; (php-ui-mode 1)
  ;; (make-local-variable 'company-backends)
  ;; (push '(company-phpactor :with company-yasnippet) company-backends)
  ;; (make-local-variable 'eldoc-documentation-function)
  ;; (setq eldoc-documentation-function 'phpactor-hover)
  ;; (eldoc-box-hover-mode 1)
  ;; (eldoc-box-hover-at-point-mode 1)
  (electric-indent-local-mode t)
  (electric-layout-mode t)
  ;; (setq-local electric-layout-rules '((?{ . around)))
  (electric-pair-local-mode t)
  ;; (flycheck-mode t)
  ;; (phpactor-smart-jump-register)
  ;; If you feel phumped and phpcs annoying, invalidate them.
  (when (boundp 'flycheck-disabled-checkers)
    (add-to-list 'flycheck-disabled-checkers 'php-phpmd)
    (add-to-list 'flycheck-disabled-checkers 'php-phpcs))
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-start-skip) "// *")
  (set (make-local-variable 'comment-end) ""))
