;;
;; init.el
;;
;;

(defun init-environment (sys-type)
  (setq load-path (cons (expand-file-name
			 (concat user-emacs-directory "/" sys-type)) load-path))
  (load (concat sys-type "-init.el")))

(cond ((eq system-type 'darwin) (init-environment "darwin")
       (cond ((eq window-system 'mac) (init-environment "mac"))
	     ((eq window-system 'ns) (init-environment "ns"))))
      ((eq system-type 'berkeley-unix) (init-environment "berkeley-unix")
       (cond ((eq window-system 'x) (init-environment "x"))))
      ((eq system-type 'usg-unix-v) (init-environment "usg-unix-v"))
      ((eq window-system 'w32) (init-environment "win32")))

(setq load-path (cons (expand-file-name user-emacs-directory) load-path))

;; exec-path settings
(dolist (dir (list "/sbin" "/usr/sbin" "/bin" "/usr/bin" "/usr/local/bin"
		   "/opt/local/sbin" "/opt/local/bin"
		   (expand-file-name "~/bin")
		   (expand-file-name "~/.emacs.d")))

  (when (and (file-exists-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat dir ":" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))

;; ラインナンバーの有無
(line-number-mode 1)
(column-number-mode 1)

;; global-set-key
(global-set-key "\M-g" 'goto-line)
(global-set-key (kbd "C-t") 'other-window)
(global-set-key (kbd "C-z") 'other-frame)
(global-unset-key (kbd "C-M-t"))

;; backup files
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.bak/"))
	    backup-directory-alist))
(setq version-control t)
(setq delete-old-versions t)

;; recentf
(recentf-mode t)
(setq recentf-max-saved-items 1000)

;; リージョンに色をつける
(transient-mark-mode 1)

;; 対応する括弧に色をつける
(show-paren-mode 1)

;; EOF を表示
(setq default-indicate-empty-lines t)

;; mm-version
(require 'mm-version)

;; cua-mode
(setq cua-enable-cua-keys nil)
(cua-mode t)

;; --------------------------- Japanesed settings ------------------------------
(require 'japanese-init)
(setq skk-preload t)

;; --------------------------- migemo Settings -------------------------------
(setq migemo-command "migemo"
      migemo-options '("-t" "emacs" "-i" "\a" ))
(setenv "RUBYLIB" "/Library/Ruby/Site/1.8/")
(setq migemo-directory "~/Applications/Emacs.app/Contents/Resources/share/migemo")
(require 'migemo)

;; --------------------------- dired-x settings --------------------------------
(setq dired-bind-jump nil)
(add-hook 'dired-mode-hook
	  (lambda ()
	    (local-set-key "\C-t" 'other-window)))

(add-hook 'dired-load-hook
	  (lambda ()
	    (load "dired-x")))

;; --------------------------- CSS Settings ------------------------------------
(add-hook 'css-mode-hook
	  (function
	   (lambda()
	     (setq tab-width 4)
	     (setq indent-tabs-mode nil))))

;; --------------------------- Java Setting ------------------------------------
(add-hook 'java-mode-hook
	  (function
	   (lambda()
	     (setq tab-width 4)
	     (setq indent-tabs-mode nil))))

;; --------------------------- install-elisp setting ---------------------------
(require 'install-elisp)

;; --------------------------- ELScreen setting --------------------------------
(setq elscreen-prefix-key "\M-z")
(require 'elscreen)
(require 'elscreen-howm)
(setq elscreen-display-screen-number nil)
(setq elscreen-display-tab nil)

;; --------------------------- coloring tab ------------------------------------
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

;; --------------------------- tramp settings ----------------------------------
(require 'tramp)
(setq tramp-default-method "ssh")
(add-to-list 'tramp-default-proxies-alist
	     '("\\'" "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
	     '("localhost\\'" nil nil))
(add-to-list 'tramp-default-proxies-alist
	     '("\\.local\\'" nil nil))

;; --------------------------- eshell settings ---------------------------------
;; (require 'eshell-settings)

;; --------------------------- term settings -----------------------------------
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

;; --------------------------- SQL settings ------------------------------------
(setq sql-product 'postgres)
;(setq sql-postgres-options
;      '("-P" "pager=off" "-p" "54320"))


;; --------------------------- htmlize settings --------------------------------
(require 'htmlize)

;; --------------------------- howm settings  ----------------------------------
(setq howm-menu-lang 'ja)
(setq howm-history-file "~/howm/.howm-history")
(setq howm-keyword-file "~/howm/.howm-keys")
(setq howm-menu-schedule-days-before 30)
(setq howm-menu-schedule-days 30)
(setq howm-menu-expiry-hours 2)
(setq howm-menu-refresh-after-save nil)
(setq howm-refresh-after-save nil)
(setq howm-list-all-title t)
(setq howm-view-title-header "#title")
(require 'howm)
(setq howm-template
      (concat howm-view-title-header
	      (concat
	      " %title%cursor\n"
	      "#date %date\n\n"
	      "%file\n\n")
	      (concat 
	       "; Local Variables:\n"
	       "; mode: howm\n"
	       "; coding: utf-8-unix\n; End:\n")))
(add-hook 'howm-menu-hook
	  '(lambda nil
	     (call-process "svn" nil "*Messages*" nil "update"
			   (expand-file-name "~/howm"))))

;; --------------------------- emacs-muse setting ------------------------------
(require 'muse-settings)

;; --------------------------- auto-complete.el settings -----------------------
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

;; --------------------------- yasnippet settings ------------------------------
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (expand-file-name (concat user-emacs-directory "/snippets")))
(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt))

;; --------------------------- PHP Settings ------------------------------------
(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.\\(inc\\|php[s34]?\\)" . php-mode))
(setq php-mode-force-pear t)
(setq php-manual-url "http://jp2.php.net/manual/ja/")
(setq php-search-url "http://jp2.php.net/")
(add-hook 'php-mode-hook
	  (lambda ()
	    (require 'php-completion)
	    (php-completion-mode t)
	    (define-key php-mode-map (kbd "C-o") 'phpcmp-complete)
	    (when (require 'auto-complete nil t)
	      (make-variable-buffer-local 'ac-sources)
	      (add-to-list 'ac-sources 
			   'ac-source-php-completion
			   'ac-source-yasnippet)
	      (auto-complete-mode t))))

;; --------------------------- mew settings ------------------------------------
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)

;; --------------------------- w3m seettings ------------------------------------
(autoload 'w3m "w3m" "Visit the www page using w3m" t)
(setq w3m-use-cookies t)
(setq w3m-cookie-accept-bad-cookies t)
(setq w3m-broken-proxy-cache t)
(setq w3m-bookmark-file "~/howm/bookmark.html")
(setq w3m-bookmark-file-coding-system "utf-8-unix")
;  (require 'octet)
;  (octet-mime-setup))

;; --------------------------- dvc settings ------------------------------------
(require 'dvc-autoloads)
(setq dvc-tips-enabled nil)

;; --------------------------- vc-svn settings ---------------------------------
(setq process-coding-system-alist
      (cons '("svn" . utf-8) process-coding-system-alist))

;; --------------------------- psvn settings -----------------------------------
(require 'psvn)
(setq svn-status-svn-environment-var-list '("LC_MESSAGES=C"
					    "LC_ALL="
					    "LANG=ja_JP.UTF-8"
					    "LC_TIME=C"))

;; --------------------------- minibuffer settings -----------------------------
(require 'minibuf-isearch)

(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(setq session-globals-max-size 500)

(defun minibuffer-delete-duplicate ()
  (let (list)
    (dolist (elt (symbol-value minibuffer-history-variable))
      (unless (member elt list)
	(push elt list)))
    (set minibuffer-history-variable (nreverse list))))
(add-hook 'minibuffer-setup-hook 'minibuffer-delete-duplicate)

;; --------------------------- moccur settings ---------------------------------
(require 'color-moccur)
(require 'moccur-edit)
(setq moccur-use-migemo t)

;; --------------------------- anything.el settings ----------------------------
(require 'anything-config)
(require 'anything-match-plugin)
(require 'anything-howm-plugin)
(require 'anything-migemo)
(require 'anything-complete)
(require 'anything-show-completion)
(require 'anything-gtags)
(anything-read-string-mode 1)

(setq anything-input-idle-delay 0.2)
(setq anything-idle-delay 0.7)

(setq anything-sources (list anything-c-source-buffers
			     anything-c-source-recentf
			     anything-c-howm-recent
			     anything-c-source-file-name-history
			     anything-c-source-bookmarks
			     anything-c-source-files-in-current-dir
			     anything-c-source-gtags-select))
(define-key anything-map (kbd "C-p") 'anything-previous-line)
(define-key anything-map (kbd "C-n") 'anything-next-line)
(define-key anything-map (kbd "C-v") 'anything-next-source)
(define-key anything-map (kbd "M-v") 'anything-previous-source)
(global-set-key (kbd "C-;") 'anything-migemo)

(require 'ac-anything)
(define-key ac-complete-mode-map (kbd "C-M-;") 'ac-complete-with-anything)

;; see. http://bitbucket.org/sakito/dot.emacs.d/src/827e9b6f96ba/site-start.d/init_anything.el
(require 'anything-c-moccur)
(setq anything-c-moccur-anything-idle-delay 0.2
      anything-c-moccur-higligt-info-line-flag t
      anything-c-moccur-enable-auto-look-flag t
      anything-c-moccur-enable-initial-pattern t)

;; バッファ内検索
(global-set-key (kbd "M-m") 'anything-c-moccur-occur-by-moccur)
;;ディレクトリ
;(global-set-key (kbd "C-M-o") 'anything-c-moccur-dmoccur)
;; grep
(require 'anything-grep)
;;(global-set-key (kbd "C-M-o") 'moccur-grep)
;;(global-set-key (kbd "C-M-o") 'anything-grep)
(global-set-key (kbd "C-M-o") 'moccur-grep-find)
;; dired
(add-hook 'dired-mode-hook
          '(lambda ()
             (local-set-key (kbd "O") 'anything-c-moccur-dired-do-moccur-by-moccur)))

;; --------------------------- simple-hatena-mode settings ---------------------
(require 'simple-hatena-mode)
(setq simple-hatena-default-id "nanasess")
(setq simple-hatena-bin	 (expand-file-name (concat user-emacs-directory "/hw.pl")))
(setq simple-hatena-root "~/howm")
(add-hook 'simple-hatena-mode-hook
	  '(lambda nil
	     (call-process "svn" nil "*Messages*" nil "update"
			   (expand-file-name "~/howm/nanasess/diary"))))
(add-hook 'simple-hatena-after-submit-hook
	  '(lambda nil
	     (call-process "svn" nil "*Messages*" nil "ci" "-m" " " ".")))

;; --------------------------- twitting-mode settings --------------------------
(require 'twittering-mode)
(setq twittering-username "nanasess")
(setq twittering-retweet-format "RT @%s: %t")
; (twittering-icon-mode)
(add-hook 'twittering-mode-hook
	  '(lambda ()
	     (define-key global-map (kbd "C-M-t")
	       'twittering-update-status-from-pop-up-buffer)))

;; --------------------------- window-system settings --------------------------
(if window-system (tool-bar-mode 0))

;; --------------------------- japanese-holiays settings -----------------------
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

