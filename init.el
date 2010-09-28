;;;; init.el --- Emacs initial file.

;; Author: Kentaro Ohkouchi  <nanasess@fsm.ne.jp>
;; URL: https://launchpad.net/~nanasess/+junk/dot.emacs.d/

;; ----------------------------------------------------------------------------
;; initial functions
(defun init-environment (sys-type)
  (add-to-list 'load-path (expand-file-name
			   (concat user-emacs-directory "/" sys-type)))
  (load (concat sys-type "-init")))

(cond ((eq system-type 'darwin) (init-environment "darwin")
       (cond ((eq window-system 'mac) (init-environment "mac"))
	     ((eq window-system 'ns) (init-environment "ns"))))
      ((eq system-type 'berkeley-unix) (init-environment "berkeley-unix")
       (cond ((eq window-system 'x) (init-environment "x"))))
      ((eq system-type 'usg-unix-v) (init-environment "usg-unix-v"))
      ((eq window-system 'w32) (init-environment "win32")))

(add-to-list 'load-path (expand-file-name user-emacs-directory))

;; ----------------------------------------------------------------------------
;; exec-path settings
(dolist (dir (list "/sbin" "/usr/sbin" "/bin" "/usr/bin" "/usr/local/bin"
		   "/opt/local/sbin" "/opt/local/bin"
		   (expand-file-name "~/bin")
		   (expand-file-name "~/.emacs.d")
		   (expand-file-name "~/Applications/pTeX.app/teTeX/bin")))

  (when (and (file-exists-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat dir ":" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))

;; ----------------------------------------------------------------------------
;; Japanesed settings
(require 'japanese-init)

;; ----------------------------------------------------------------------------
;; SKK settings
(setq skk-user-directory "~/Dropbox/ddskk")
(setq skk-init-file (concat user-emacs-directory ".skk"))
(setq skk-preload t)
(setq skk-auto-save-interval 30)
(defun skk-auto-save ()
  "auto save of the skk-jisyo and skk-study."
  (skk-save-jisyo)
  (skk-study-save))
(run-with-idle-timer skk-auto-save-interval t 'skk-auto-save)

;; ----------------------------------------------------------------------------
;; line-number settings
(line-number-mode 1)
(column-number-mode 1)

;; ----------------------------------------------------------------------------
;; global key-bindings
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-t") 'other-window)
(global-set-key (kbd "C-z") 'other-frame)
(global-set-key (kbd "C-j") 'skk-mode)
(global-unset-key (kbd "C-M-t"))

;; ----------------------------------------------------------------------------
;; backup files settings
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.bak/"))
	    backup-directory-alist))
(setq version-control t)
(setq delete-old-versions t)

;; ----------------------------------------------------------------------------
;; recentf settings
(recentf-mode t)
(setq recentf-max-saved-items 1000)

;; ----------------------------------------------------------------------------
;; coloring-region settings
(transient-mark-mode 1)

;; ----------------------------------------------------------------------------
;; show-paren settings
(show-paren-mode 1)

;; ----------------------------------------------------------------------------
;; show EOF settings
(setq default-indicate-empty-lines t)

;; ----------------------------------------------------------------------------
;; face settings
(set-background-color "ivory")

(defface hlline-face
  '((((class color)
      (background dark))
     (:background "blue" :foreground "white"))
    (((class color)
      (background light))
     (:background "honeydew"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
(global-hl-line-mode)

(setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark))

(defface my-mark-whitespace '((t (:background "gray"))) nil)
(defface my-mark-tabs '((t (:background "white smoke"))) nil)
(defface my-mark-lineendsspaces '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-mark-whitespace 'my-mark-whitespace)
(defvar my-mark-tabs 'my-mark-tabs)
(defvar my-mark-lineendsspaces 'my-mark-lineendsspaces)

(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("\t" 0 my-mark-tabs append)
     ("　" 0 my-mark-whitespace append)
     ("[ \t]+$" 0 my-mark-lineendsspaces append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

;; ----------------------------------------------------------------------------
;; window-system settings
(if window-system (tool-bar-mode 0))

;; ----------------------------------------------------------------------------
;; uniquify settings
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;; ----------------------------------------------------------------------------
;; dired-x settings
(setq dired-bind-jump nil)
(add-hook 'dired-mode-hook
	  (lambda ()
	    (local-set-key "\C-t" 'other-window)
	    (local-set-key "r" 'wdired-change-to-wdired-mode)))
(add-hook 'dired-load-hook
	  (lambda ()
	    (load "dired-x")))

;; ----------------------------------------------------------------------------
;; CSS settings
(add-hook 'css-mode-hook
	  (function
	   (lambda()
	     (setq tab-width 4)
	     (setq indent-tabs-mode nil))))

;; ----------------------------------------------------------------------------
;; Java settings
(add-hook 'java-mode-hook
	  (function
	   (lambda()
	     (setq tab-width 4)
	     (setq indent-tabs-mode nil))))

;; ----------------------------------------------------------------------------
;; JavaScript-mode settings
(add-hook 'js-mode-hook
	  (function
	   (lambda()
	     (setq tab-width 4)
	     (setq indent-tabs-mode nil))))

;; ----------------------------------------------------------------------------
;; nXML-mode settings
(add-to-list 'auto-mode-alist
	     '("\\.\\(xml\\|xsl\\|rng\\|html\\|tpl\\)\\'" . nxml-mode))
(add-hook 'nxml-mode-hook
	  (lambda ()
	    (setq auto-fill-mode -1)
	    (setq nxml-slash-auto-complete-flag t)
	    (setq nxml-child-indent 2)
	    (rng-validate-mode 0)
	    (cua-mode 0)
	    (setq indent-tabs-mode t)
	    (setq tab-width 2)))

;; ----------------------------------------------------------------------------
;; SQL settings
(setq sql-product 'postgres)
;(setq sql-postgres-options
;      '("-P" "pager=off" "-p" "54320"))

;; ----------------------------------------------------------------------------
;; tramp settings
(require 'tramp)
(setq tramp-default-method "ssh")
(add-to-list 'tramp-default-proxies-alist
	     '("\\'" "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
	     '("localhost\\'" nil nil))
(add-to-list 'tramp-default-proxies-alist
	     '("\\.local\\'" nil nil))

;; ----------------------------------------------------------------------------
;; migemo settings
(setq migemo-command "migemo"
      migemo-options '("-t" "emacs" "-i" "\a" ))
(setenv "RUBYLIB" "/Library/Ruby/Site/1.8/")
(setq migemo-directory (expand-file-name
			(concat invocation-directory
				"../Resources/share/migemo")))
(require 'migemo nil t)

;; ----------------------------------------------------------------------------
;; gtags settings
(autoload 'gtags-mode "gtags" nil t)
(setq gtags-mode-hook
      '(lambda ()
	 (local-set-key "\M-." 'gtags-find-tag)
	 (local-set-key "\C-u\M-." 'gtags-pop-stack)
	 (local-set-key "\C-u\M-r" 'gtags-find-rtag)
	 (local-set-key "\C-u\M-s" 'gtags-find-symbol)))

;; ----------------------------------------------------------------------------
;; mmm-mode settings
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "/mmm")))
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(set-face-background 'mmm-default-submode-face nil)
(mmm-add-classes
 '((embedded-css
    :submode css-mode
    :front "<style[^>]*>"
    :back  "</style>")))
(mmm-add-classes
 '((embedded-js
    :submode javascript-mode
    :front "<script[^>]*>"
    :back  "</script>")))
(mmm-add-mode-ext-class nil "\\.tpl?\\'" 'embedded-css)
(mmm-add-mode-ext-class nil "\\.tpl?\\'" 'embedded-js)

;; ----------------------------------------------------------------------------
;; term settings
(setq shell-file-name "/opt/local/bin/zsh")
(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(require 'multi-term)
(setq multi-term-program shell-file-name)
(setq term-unbind-key-list '("M-x" "C-z" "C-x" "C-c" "C-h" "C-y" "C-t"))
;; (add-hook 'term-mode-hook
;; 	  '(lambda ()
;; 	     (term-set-escape-char ?\C-x)))
(global-set-key "\C-x\C-t" 'multi-term-dedicated-toggle)

;; ----------------------------------------------------------------------------
;; org-mode settings
(require 'org-install)
(require 'ob-sh)
(require 'ob-css)
(require 'ob-sql)
(setq org-startup-truncated nil)
(setq org-startup-folded nil)
(setq org-return-follows-link t)
(org-remember-insinuate)
(setq org-directory "~/howm/")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull (concat org-directory "flagged.howm"))
(setq org-default-notes-file (concat org-directory "agenda.howm"))
(setq org-time-stamp-formats '("[%Y-%m-%d]" . "[%Y-%m-%d %H:%M]"))
(setq org-remember-templates
      '(("Todo" ?t "** TODO %?\n   %i\n   %a\n   %t" nil "Inbox")
	("Bug" ?b "** TODO %?   :bug:\n   %i\n   %a\n   %t" nil "Inbox")
	("Idea" ?i "** %?\n   %i\n   %a\n   %t" nil "New Ideas")))
(add-hook 'org-remember-mode-hook
	  (lambda ()
	    (local-set-key "\C-c\C-f" 'org-remember-finalize)))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-export-latex-classes
      '(("jarticle"
	"\\documentclass[11t,a4j,oneside]{jarticle}"
	("\\section{%s}" . "\\section*{%s}")
	("\\subsection{%s}" . "\\subsection*{%s}")
	("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	("\\paragraph{%s}" . "\\paragraph*{%s}")
	("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
(setq org-latex-to-pdf-process
      '("org-latex-to-pdf.sh %s" "org-latex-to-pdf.sh %s"))
;; ----------------------------------------------------------------------------
;; session settings
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(setq session-globals-max-size 500)

;; ----------------------------------------------------------------------------
;; moccur settings
(require 'color-moccur)
(require 'moccur-edit)
(setq moccur-use-migemo t)

;; ----------------------------------------------------------------------------
;; sense-region settings
(require 'sense-region)
(sense-region-on)

;; ----------------------------------------------------------------------------
;; dvc settings
(require 'dvc-autoloads)
(setq dvc-tips-enabled nil)

;; ----------------------------------------------------------------------------
;; vc-svn settings
(setq process-coding-system-alist
      (cons '("svn" . utf-8) process-coding-system-alist))

;; ----------------------------------------------------------------------------
;; psvn settings
(require 'psvn)
(setq svn-status-svn-environment-var-list '("LC_MESSAGES=C"
					    "LC_ALL="
					    "LANG=ja_JP.UTF-8"
					    "LC_TIME=C"))

;; ----------------------------------------------------------------------------
;; howm settings
(setq howm-menu-lang 'ja)
(setq howm-directory org-directory)
(setq howm-history-file (concat howm-directory ".howm-history"))
(setq howm-keyword-file (concat howm-directory ".howm-keys"))
(setq howm-menu-schedule-days-before 30)
(setq howm-menu-schedule-days 30)
(setq howm-menu-expiry-hours 2)
(setq howm-menu-refresh-after-save nil)
(setq howm-refresh-after-save nil)
(setq howm-list-all-title t)
(add-hook 'org-mode-hook 'howm-mode)
(setq howm-view-title-header "*")
(setq howm-view-use-grep nil)
(add-to-list 'auto-mode-alist '("\\.howm$" . org-mode))
(require 'howm)
(setq howm-template
      (concat howm-view-title-header
	      (concat
	      " %title%cursor %date\n\n"
	      "%file\n\n")
	      (concat
	       "#+LATEX_CLASS: jarticle\n"
	       "# Local Variables:\n"
	       "# coding: utf-8-unix\n"
	       "# End:\n")))
(defun howm-save-and-kill-buffer ()
"kill screen when exiting from howm-mode
"
  (interactive)
  (let* ((file-name (buffer-file-name)))
    (when (and file-name (string-match "\\.howm" file-name))
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
	  '(lambda nil
	     (start-process "howm-svn-update" "*Messages*" "svn" "update"
			   (expand-file-name howm-directory))))
(eval-after-load "howm-mode"
  '(progn
     (define-key howm-mode-map
       "\C-c\C-q" 'howm-save-and-kill-buffer)))

;; ----------------------------------------------------------------------------
;; PHP settings
(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.\\(inc\\|php[s34]?\\)" . php-mode))
(setq php-mode-force-pear t)
(setq php-manual-url "http://jp2.php.net/manual/ja/")
(setq php-search-url "http://jp2.php.net/")
(add-hook 'php-mode-hook
	  (lambda ()
	    (gtags-mode 1)
	    (require 'php-completion)
	    (php-completion-mode t)
	    (define-key php-mode-map (kbd "C-o") 'phpcmp-complete)
	    (when (require 'auto-complete nil t)
	      (make-variable-buffer-local 'ac-sources)
	      (add-to-list 'ac-sources 
			   'ac-source-php-completion
			   'ac-source-yasnippet)
	      (auto-complete-mode t))))

;; ----------------------------------------------------------------------------
;; yasnippet settings
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (expand-file-name (concat user-emacs-directory "/snippets")))
(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt))

;; ----------------------------------------------------------------------------
;; auto-complete.el settings
(require 'auto-complete)
(require 'auto-complete-gtags)
(require 'auto-complete-yasnippet)

(global-auto-complete-mode t)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
(setq ac-auto-start nil)
(global-set-key "\M-/" 'ac-start)

(set-default 'ac-sources '(ac-source-yasnippet
			   ac-source-abbrev
			   ac-source-words-in-buffer
			   ac-source-gtags))

;; ----------------------------------------------------------------------------
;; mew settings
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
;; mm-version
(require 'mm-version)

;; ----------------------------------------------------------------------------
;; w3m seettings
(autoload 'w3m "w3m" "Visit the www page using w3m" t)
(setq w3m-use-cookies t)
(setq w3m-cookie-accept-bad-cookies t)
(setq w3m-broken-proxy-cache t)
(setq w3m-bookmark-file (concat howm-directory "bookmark.html"))
(setq w3m-bookmark-file-coding-system "utf-8-unix")
;  (require 'octet)
;  (octet-mime-setup))

;; ----------------------------------------------------------------------------
;; simple-hatena-mode settings
(require 'simple-hatena-mode)
(setq simple-hatena-default-id "nanasess")
(setq simple-hatena-bin	 (expand-file-name (concat user-emacs-directory "/hw.pl")))
(setq simple-hatena-root howm-directory)
(add-hook 'simple-hatena-mode-hook
	  '(lambda nil
	     (call-process "svn" nil "*Messages*" nil "update"
			   (expand-file-name (concat howm-directory "nanasess/diary")))))
(add-hook 'simple-hatena-after-submit-hook
	  '(lambda nil
	     (call-process "svn" nil "*Messages*" nil "ci" "-m" " " ".")))

;; ----------------------------------------------------------------------------
;; twitting-mode settings
(require 'twittering-mode)
(unless (load "twittering-tinyurl-api-key" t t)
  (setq twittering-tinyurl-api-key nil))
(setq twittering-auth-method 'xauth)
(setq twittering-username "nanasess")
(setq twittering-status-format (concat "%i %S(%s),  %@:\n%"
				       "FILL[  ]{%T // from %f%L%r%R}\n "))
(setq twittering-retweet-format "RT @%s: %t")
(setq twittering-display-remaining t)
(setq twittering-allow-insecure-server-cert t)
(add-hook 'twittering-mode-hook
	  (lambda ()
	    (let ((km twittering-mode-map))
	      (define-key km (kbd "s") 'twittering-current-timeline)
	      (define-key km (kbd "w") 'twittering-update-status-interactive))
	    (let ((km twittering-edit-mode-map))
	      (define-key km (kbd "C-c C-q") 'twittering-edit-cancel-status)
	      (define-key km (kbd "C-u C-u") 'twittering-edit-replace-at-point))))
(defun twittering-tinyurl-get (longurl)
  "Tinyfy LONGURL."
  (if longurl
      (let ((buffer
	     (twittering-url-retrieve-synchronously (concat
						     twittering-tinyurl-api
						     longurl))))
	(with-current-buffer buffer
	  (goto-char (point-min))
	  (prog1
	      (if (search-forward-regexp "\n\r?\n\\([^\n\r]*\\)" nil t)
		  (match-string-no-properties 1)
		(error "TinyURL failed: %s" longurl))
	    (kill-buffer buffer))))
    nil))
(setq twittering-tinyurl-api (concat "http://api.j.mp/v3/shorten?login="
				     twittering-username
				     "&apiKey="
				     twittering-tinyurl-api-key
				     "&format=txt&uri="))

;; ----------------------------------------------------------------------------
;; navi2ch settings
(autoload 'navi2ch "navi2ch" "Navigator for 2ch for Emacs" t)

;; ----------------------------------------------------------------------------
;; japanese-holiays settings
(add-hook 'calendar-load-hook
	  (lambda ()
	    (require 'japanese-holidays)
	    (setq calendar-holidays
		  (append japanese-holidays local-holidays other-holidays))))
(setq mark-holidays-in-calendar t)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
(setq calendar-weekend-marker 'diary)
(add-hook 'today-visible-calendar-hook 'calendar-mark-weekend)
(add-hook 'today-invisible-calendar-hook 'calendar-mark-weekend)

;; ----------------------------------------------------------------------------
;; emacs-muse settings
(require 'muse-settings)

;; ----------------------------------------------------------------------------
;; htmlize settings
(require 'htmlize)

;; ----------------------------------------------------------------------------
;; install-elisp settings
(require 'install-elisp)

;; ----------------------------------------------------------------------------
;; auto-install settings
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/")
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

;; ----------------------------------------------------------------------------
;; auto-async-byte-compile settings
(require 'auto-async-byte-compile)
(setq auto-async-byte-compile-exclude-files-regexp "/mac/") ;dummy
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;; ----------------------------------------------------------------------------
;; anything.el settings
(require 'anything-startup)
(require 'anything-howm-plugin)
(require 'anything-gtags)
(require 'anything-migemo nil t)
(defun my-anything ()
  "Anything command for you.
It is automatically generated by `anything-migrate-sources'."
  (interactive)
  (anything-other-buffer
    '(anything-c-source-buffers+
      anything-c-source-recentf
      anything-c-source-files-in-current-dir+
      anything-c-howm-recent
      anything-c-source-bookmarks
      anything-c-source-gtags-select)
    "*my-anything*"))
(global-set-key (kbd "C-;") 'my-anything)
(setq grep-command "grep -nHr -e ")
(require 'grep-edit)
(define-key anything-map (kbd "C-v") 'anything-next-source)
(define-key anything-map (kbd "M-v") 'anything-previous-source)

;; ----------------------------------------------------------------------------
;; one-key settings
(require 'one-key)
(require 'one-key-config)
(require 'my-one-key-config)
(require 'one-key-default)
(one-key-default-setup-keys)
