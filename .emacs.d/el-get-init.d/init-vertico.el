(add-hook 'emacs-startup-hook
          #'(lambda ()
              (vertico-mode)
              (marginalia-mode)
              (savehist-mode)
              (add-to-list 'savehist-additional-variables 'kill-ring)
              (add-to-list 'savehist-additional-variables 'log-edit-comment-ring)
              (add-to-list 'savehist-additional-variables 'search-ring)
              (add-to-list 'savehist-additional-variables 'regexp-search-ring)))

(with-eval-after-load 'vertico
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)
  (setq vertico-count 20)
  (require 'consult)
  (require 'orderless)
  (require 'marginalia)
  (require 'savehist)

  (global-set-key (kbd "C-z C-r") #'vertico-repeat)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (define-key vertico-map (kbd "C-l") #'vertico-directory-up)
  (define-key vertico-map (kbd "C-j") #'vertico-directory-enter)
  (define-key vertico-map (kbd "M-v") #'vertico-next-group)
  (define-key vertico-map (kbd "C-v") #'vertico-previous-group))
