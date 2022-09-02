(add-hook 'after-init-hook
          #'(lambda ()
              (vertico-mode)
              ;; (fido-vertical-mode +1)
              (marginalia-mode)
              (savehist-mode)))
(with-eval-after-load 'vertico
  (custom-declare-face 'vertico-group-title '((t :inherit shadow :slant italic :foreground "#788484")) nil )
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
