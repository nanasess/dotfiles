(with-eval-after-load 'magit
  ;; (require 'forge)
  ;; see https://stackoverflow.com/a/32914548/4956633
  (defun visit-gh-pull-request (repo)
    "Visit the current branch's PR on Github."
    (interactive)
    (message repo)
    (browse-url
     (format "https://github.com/%s/pull/new/%s"
             (replace-regexp-in-string
              "\\.git$" ""
              (replace-regexp-in-string
               "\\`.+github\\.com.\\(.+\\)\\(\\.git\\)?\\'" "\\1"
               repo))
             (magit-get-current-branch))))

  ;; Bitbucket pull requests are kinda funky, it seems to try to just do the
  ;; right thing, so there's no branches to include.
  ;; https://bitbucket.org/<username>/<project>/pull-request/new
  (defun visit-bb-pull-request (repo)
    (message repo)
    (browse-url
     (format "https://bitbucket.org/%s/pull-request/new?source=%s&t=1"
             (replace-regexp-in-string
              "\\`.+bitbucket\\.org.\\(.+\\)\\.git\\'" "\\1"
              repo)
             (magit-get-current-branch))))
  (defun endless/visit-pull-request-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (let ((repo (magit-get "remote" (magit-get-remote) "url")))
      (if (not repo)
          (setq repo (magit-get "remote" (magit-get-push-remote) "url")))
      (if (string-match "github\\.com" repo)
          (visit-gh-pull-request repo)
        (visit-bb-pull-request repo))))

  (setq magit-diff-refine-hunk t)
  (add-to-list 'magit-process-password-prompt-regexps "^パスフレーズを入力: ?$")
  ;; visit PR for github or bitbucket repositories with "v"
  (define-key magit-mode-map "v" #'endless/visit-pull-request-url)
  (define-key magit-log-mode-map (kbd "j") 'magit-section-forward)
  (define-key magit-log-mode-map (kbd "k") 'magit-section-backward)
  (remove-hook 'server-switch-hook 'magit-commit-diff))
(global-set-key (kbd "C-z m") 'magit-status)
