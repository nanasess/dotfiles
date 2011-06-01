;;;; init.el --- Emacs initial file.

;; Author: Kentaro Ohkouchi  <nanasess@fsm.ne.jp>
;; URL: https://launchpad.net/~nanasess/+junk/dot.emacs.d/

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; initial load files
;;;

(dolist (sys-type (list (symbol-name system-type)
			(symbol-name window-system)))

  (add-to-list 'load-path
	       (expand-file-name (concat user-emacs-directory sys-type)))
  (load "init" t))
(add-to-list 'load-path (expand-file-name user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; exec-path settings
;;;

(dolist (dir (list "/sbin" "/usr/sbin" "/bin" "/usr/bin" "/usr/local/bin"
		   "/opt/local/sbin" "/opt/local/bin"
		   (expand-file-name "~/bin")
		   (expand-file-name "~/.emacs.d")
		   (expand-file-name "~/Applications/pTeX.app/teTeX/bin")))

  (when (and (file-exists-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat dir ":" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Japanesed settings
;;;

(require 'japanese-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; izonmoji-mode settings
;;;
;;; (auto-install-from-url "http://navi2ch.cvs.sourceforge.net/viewvc/navi2ch/navi2ch/contrib/izonmoji-mode.el")
;;;
(require 'izonmoji-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SKK settings
;;;

(setq skk-user-directory "~/Dropbox/ddskk")
(setq skk-init-file (concat user-emacs-directory ".skk"))
(setq skk-preload t)
(setq skk-auto-save-interval 30)
(defun toggle-skk-kutouten ()
  "toggle skk-kutoten-type."
  (interactive)
  (cond ((eq skk-kutouten-type 'en)
	 (setq skk-kutouten-type 'jp))
	((setq skk-kutouten-type 'en)))
  (message (format "skk-kutoten-type on set to the %s." skk-kutouten-type)))
(defun skk-auto-save ()
  "auto save of the skk-jisyo and skk-study."
  (skk-save-jisyo)
  (skk-study-save))
(run-with-idle-timer skk-auto-save-interval t 'skk-auto-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; line-number settings
;;;

(line-number-mode 1)
(column-number-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; global key-bindings
;;;

(global-unset-key (kbd "C-M-t"))
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-j") 'skk-mode)
(global-set-key (kbd "C-t") 'other-window)
(global-set-key (kbd "C-z C-u") 'other-frame)
(global-set-key (kbd "C-z C-j") 'toggle-skk-kutouten)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; backup files settings
;;;

(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.bak/"))
	    backup-directory-alist))
(setq version-control t)
(setq delete-old-versions t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; recentf settings
;;;
;;; (auto-install-from-emacswiki "recentf-ext.el")
;;;

(require 'recentf-ext)
(setq recentf-max-saved-items 50000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; coloring-region settings
;;;

(transient-mark-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; show-paren settings
;;;

(show-paren-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; show EOF settings
;;;

(setq default-indicate-empty-lines t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; face settings
;;;

(set-background-color "ivory")
(set-face-foreground 'font-lock-keyword-face "#7f007f")

(defface hlline-face
  '((((class color) (background light))
     (:background "honeydew"))) nil)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; window-system settings
;;;

(if window-system (tool-bar-mode 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; uniquify settings
;;;

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; dired-x settings
;;;

(setq dired-bind-jump nil)
(add-hook 'dired-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-t") 'other-window)
	    (local-set-key (kbd "r") 'wdired-change-to-wdired-mode)))
(add-hook 'dired-load-hook
	  (lambda ()
	    (load "dired-x")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CSS settings
;;;

(add-hook 'css-mode-hook
	  (lambda ()
	    (setq tab-width 4)
	    (setq indent-tabs-mode nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Java settings
;;;

(add-hook 'java-mode-hook
	  (lambda ()
	    (setq tab-width 4)
	    (setq indent-tabs-mode nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; JavaScript-mode settings
;;;

(add-hook 'javascript-mode-hook
	  (lambda ()
	    (setq tab-width 4)
	    (setq indent-tabs-mode nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; js2-mode settings
;;;

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
	  (lambda ()
	    (setq tab-width 4)
	    (setq indent-tabs-mode nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; nXML-mode settings
;;;

(add-to-list 'auto-mode-alist
	     '("\\.\\(xml\\|xsl\\|rng\\|html\\|tpl\\)\\'" . nxml-mode))
(add-hook 'nxml-mode-hook
	  (lambda ()
	    (setq auto-fill-mode -1)
	    (setq nxml-slash-auto-complete-flag t)
	    (setq nxml-child-indent 2)
	    (rng-validate-mode 0)
	    (setq indent-tabs-mode nil)
	    (setq tab-width 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SQL settings
;;;

(setq sql-product 'postgres)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; e2wm settings
;;;
;;; (auto-install-from-url "https://github.com/kiwanami/emacs-window-layout/raw/master/window-layout.el")
;;; (auto-install-from-url "https://github.com/kiwanami/emacs-window-manager/raw/master/e2wm.el")
;;; (auto-install-from-url "https://github.com/kiwanami/emacs-window-manager/raw/master/e2wm-config.el")
;;;

(require 'e2wm)
(setq e2wm:prefix-key "C-z")
;;; layout
(setq e2wm:c-code-recipe
      '(| (:left-max-size 30)
	  (- (:upper-size-ratio 0.7)
	     files history)
	  (- (:upper-size-ratio 0.7)
	     (| (:right-max-size 25)
		main imenu)
	     sub)))
;;; keymap
(e2wm:add-keymap
 e2wm:pst-minor-mode-keymap
 '(("prefix c" . e2wm:dp-code) ; codeへ変更
   ("prefix t" . e2wm:dp-two)  ; twoへ変更
   ("prefix i" . e2wm:dp-doc)  ; docへ変更
   ("prefix d" . e2wm:dp-dashboard) ; dashboardへ変更
   ("prefix C-p" . e2wm:pst-history-forward-command) ; 履歴を進む
   ("prefix C-n" . e2wm:pst-history-back-command) ; 履歴をもどる
   ("prefix L" . ielm)
   ("prefix m" . e2wm:pst-window-select-main-command)
   ) e2wm:prefix-key)

;;; dashboard
(setq e2wm:c-dashboard-plugins
      '((open :plugin-args (:command mew :buffer "%inbox"))
	(open :plugin-args (:command twittering-mode :buffer ":home"))
	(open :plugin-args (:command w3m-bookmark-view :buffer "*w3m*"))
	(open :plugin-args (:command howm-menu :buffer "*howmM:%menu%*"))))

(setq follow-intercept-processes nil)

(defvar normal-frame-width 82)
(defvar wide-frame-width 175)

(defun frame-size-greater-p ()
  (< (+ (/ (- wide-frame-width normal-frame-width) 2) normal-frame-width)
     (frame-width (selected-frame))))

(defun normal-size-frame ()
  "Resize to normal size frame."
  (interactive)
  (setq toggle-frame-flag t)
  (set-frame-width (selected-frame) normal-frame-width))

(defun wide-size-frame ()
  "Resize to wide size frame."
  (interactive)
  (setq toggle-frame-flag nil)
  (set-frame-width (selected-frame) wide-frame-width))

(defun toggle-size-frame ()
  "toggle frame size."
  (interactive)
  (cond ((frame-size-greater-p) (normal-size-frame))
	((wide-size-frame))))

(global-set-key (kbd "M-+") 'e2wm:start-management)
(global-set-key (kbd "C-z C-z") 'toggle-size-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; migemo settings
;;;

(setq migemo-dictionary "/usr/local/share/migemo/euc-jp/migemo-dict")
(when (file-exists-p migemo-dictionary)
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs" "-i" "\a"))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (require 'migemo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; gtags settings
;;;

(autoload 'gtags-mode "gtags" nil t)
(add-hook 'gtags-mode-hook
	  (lambda ()
	    (local-set-key (kbd "M-.") 'gtags-find-tag)
	    (local-set-key (kbd "C-u M-.") 'gtags-pop-stack)
	    (local-set-key (kbd "C-u M-r") 'gtags-find-rtag)
	    (local-set-key (kbd "C-u M-s") 'gtags-find-symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mmm-mode settings
;;;

(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "mmm")))
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(set-face-background 'mmm-default-submode-face "ivory2")
(setq mmm-font-lock-available-p t)
(mmm-add-classes
 '((embedded-css
    :submode css-mode :front "<style[^>]*>" :back  "</style>")))
(mmm-add-classes
 '((embedded-js
    :submode javascript-mode :front "<script[^>]*>" :back "</script>")))
(mmm-add-group
 'php-others
 '((php-heredoc
    :include-front t
    :include-back t
    :front-offset 0
    :back-offset 0
    :front "<<<\\s-*[\"\']?\\([a-zA-Z_][a-zA-Z0-9_]+\\)"
    :back "^\\s-*~1;$"
    :save-matches 1
    :submode sql-mode
    :face mmm-code-submode-face
    :delimiter-mode nil)))
(mmm-add-mode-ext-class nil "\\.tpl?\\'" 'embedded-css)
(mmm-add-mode-ext-class nil "\\.tpl?\\'" 'embedded-js)
(mmm-add-mode-ext-class nil "\\.php\\'" 'php-others)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; org-mode settings
;;;
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "org")))
(require 'org-install)
(require 'ob-sh)
(require 'ob-css)
(require 'ob-sql)
(setq org-startup-truncated nil)
(setq org-startup-folded nil)
(setq org-return-follows-link t)
(org-remember-insinuate)
(setq org-directory "~/Dropbox/howm/")
(setq org-default-notes-file (concat org-directory "agenda.howm"))
(setq org-capture-templates
      '(("l" "年/月/日のエントリを作成する" entry
	 (file+datetree org-default-notes-file))
	("m" "年/月/日のリストを作成する" item
	 (file+datetree org-default-notes-file))))
(global-set-key (kbd "C-z C-c") 'org-capture)
(org-defkey org-mode-map (kbd "C-j") 'skk-mode)
(setq org-export-latex-classes
      '(("jarticle"
	 "\\documentclass[11t,a4j,oneside]{jarticle}"
	 ("\\section{%s}" . "\\section*{%s}")
	 ("\\subsection{%s}" . "\\subsection*{%s}")
	 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	 ("\\paragraph{%s}" . "\\paragraph*{%s}")
	 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
(setq org-latex-to-pdf-process
      '("org-latex-to-pdf.sh %f" "org-latex-to-pdf.sh %f"))
(set-face-bold-p 'org-document-title nil)
(set-face-attribute 'org-document-title nil :height 1.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; htmlize settings
;;;

(require 'htmlize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; session settings
;;;

(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(setq session-globals-max-size 500)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; moccur settings
;;;

(require 'color-moccur)
(require 'moccur-edit)
(setq moccur-use-migemo t)
(setq moccur-split-word t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sense-region settings
;;;

(require 'sense-region)
(sense-region-on)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; dvc settings
;;;

(when (require 'dvc-autoloads nil t)
  (setq dvc-tips-enabled nil)
  (setq dvc-prefix-key "\C-z\C-v"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; vc-svn settings
;;;

(add-to-list 'process-coding-system-alist '("svn" . utf-8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; psvn settings
;;;

(require 'psvn)
(setq svn-status-svn-environment-var-list
      (append '("LANG=ja_JP.UTF-8" "LC_TIME=C")
	      svn-status-svn-environment-var-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; howm settings
;;;

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
(setq howm-schedule-menu-types "[!@\+]")
(add-hook 'org-mode-hook 'howm-mode)
(setq howm-view-title-header "#+TITLE:")
(setq howm-view-use-grep nil)
(add-to-list 'auto-mode-alist '("\\.howm$" . org-mode))
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "howm")))
(require 'howm)
(setq howm-template
      (concat howm-view-title-header
	      (concat
	       " %title%cursor\n"
	       "#+DATE: %date\n\n"
	       "%file\n\n")
	      (concat
	       "#+LATEX_CLASS: jarticle\n"
	       "# Local Variables:\n"
	       "# coding: utf-8-unix\n"
	       "# End:\n")))
(defun howm-save-and-kill-buffer ()
  "kill screen when exiting from howm-mode"
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
	  (lambda ()
	    (define-key howm-mode-map (kbd "C-c C-q") 'howm-save-and-kill-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PHP settings
;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; yasnippet settings
;;;

(require 'yasnippet)
(yas/initialize)
(yas/load-directory (expand-file-name (concat user-emacs-directory "snippets")))
(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt))
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
(add-hook 'org-mode-hook
          (lambda ()
            ;; yasnippet (using the new org-cycle hooks)
            (setq ac-use-overriding-local-map t)
            (make-variable-frame-local 'yas/trigger-key)
            (setq  yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; auto-complete.el settings
;;;

(require 'auto-complete)
(add-to-list 'ac-dictionary-directories (concat user-emacs-directory "dict"))
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-show-menu 0.8)
(setq ac-use-menu-map t)
(define-key ac-completing-map [tab] 'ac-complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew settings
;;;

(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
;; mm-version
(require 'mm-version)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; w3m seettings
;;;

(autoload 'w3m "w3m" "Visit the www page using w3m" t)
(setq w3m-init-file (concat user-emacs-directory ".emacs-w3m"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; simple-hatena-mode settings
;;;

(require 'simple-hatena-mode)
(setq simple-hatena-default-id "nanasess")
(setq simple-hatena-bin (expand-file-name (concat user-emacs-directory "hw.pl")))
(setq simple-hatena-root howm-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; twitting-mode settings
;;;
;;; (auto-install-from-url "http://github.com/hayamiz/twittering-mode/raw/master/twittering-mode.el")
;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; navi2ch settings
;;;

(autoload 'navi2ch "navi2ch" "Navigator for 2ch for Emacs" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Japanese-holiays settings
;;;

(setq mark-holidays-in-calendar t)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
(setq calendar-weekend-marker 'diary)
(require 'japanese-holidays)
(setq calendar-holidays
      (append japanese-holidays local-holidays other-holidays))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; calfw settings
;;;
;;; (auto-install-from-url "https://github.com/kiwanami/emacs-calfw/raw/master/calfw.el")
;;; (auto-install-from-url "https://github.com/kiwanami/emacs-calfw/raw/master/calfw-howm.el")
;;; (auto-install-from-url "https://github.com/kiwanami/emacs-calfw/raw/master/calfw-ical.el")
;;;

(require 'calfw)
(setq my-howm-schedule-page "calfw スケジュール")

(defun my-cfw-open-schedule-buffer ()
  (interactive)
  (let*
      ((date (cfw:cursor-to-nearest-date))
       (howm-items
        (howm-folder-grep
         howm-directory
         (regexp-quote my-howm-schedule-page))))
    (cond
     ((null howm-items) ; create
      (howm-create-file-with-title my-howm-schedule-page nil nil nil nil))
     (t
      (howm-view-open-item (car howm-items))))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert
     (format "[%04d-%02d-%02d]@ "
             (calendar-extract-year date)
             (calendar-extract-month date)
             (calendar-extract-day date)))))

(eval-after-load "howm-menu"
  '(progn
     (require 'calfw-howm)
     (cfw:install-howm-schedules)
     (define-key howm-mode-map (kbd "M-C") 'cfw:open-howm-calendar)
     (define-key cfw:howm-schedule-map (kbd "i") 'my-cfw-open-schedule-buffer)
     (define-key cfw:howm-schedule-inline-keymap (kbd "i")
       'my-cfw-open-schedule-buffer)))

(setq cfw:howm-schedule-summary-transformer
      (lambda (line) (split-string (replace-regexp-in-string "^[^@!\+]+[@!\+] " "" line) " / ")))

(setq calendar-month-name-array
      ["January(01)" "February(02)" "March(03)" "April(04)" "May(05)" "June(06)"
       "July(07)" "August(08)" "September(09)" "October(10)" "November(11)"
       "December(12)"])
(setq calendar-day-name-array
      ["日" "月" "火" "水" "木" "金" "土"])

;; (require 'calfw-ical)
;; (cfw:install-ical-schedules)
;; (setq cfw:ical-calendar-contents-sources
;;       '("http://www.google.com/calendar/ical/nanasess%40gmail.com/private-61b32d43d3ef5ec36e4339e0161389aa/basic.ics"
;; 	"http://www.google.com/calendar/ical/p%23weather%40group.v.calendar.google.com/public/basic.ics"))
;; (setq cfw:ical-calendar-annotations-sources
;;       '("http://www.google.com/calendar/ical/p%23weather%40group.v.calendar.google.com/public/basic.ics"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; pdf-preview settings
;;;
;;; (auto-install-from-url "http://homepage.mac.com/matsuan_tamachan/emacs/pdf-preview.el")
;;;

(require 'pdf-preview)
(setq ps-print-header nil)
(setq pdf-preview-preview-command "open")
(setq mew-print-function 'pdf-preview-buffer-with-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; auto-install settings
;;;

(require 'auto-install)
(setq auto-install-directory user-emacs-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; auto-async-byte-compile settings
;;;

(require 'auto-async-byte-compile)
(setq auto-async-byte-compile-exclude-files-regexp "/mac/") ;dummy
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; emacs-init-check settings
;;;
;;; (auto-install-from-emacswiki "emacs-init-check.el")
;;;

(require 'emacs-init-check)
(add-to-list 'auto-emacs-init-check-program-args "nice")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; anything.el settings
;;;
;;; (auto-install-batch "anything")
;;; (auto-install-from-url "https://github.com/kitokitoki/anything-howm/raw/master/anything-howm.el")
;;;

(require 'anything-startup)
(require 'anything-howm)
(require 'anything-gtags)
(require 'anything-migemo nil t)
;; (setq anything-use-migemo nil)
(defun my-anything ()
  "Anything command for you.
It is automatically generated by `anything-migrate-sources'."
  (interactive)
  (anything-other-buffer
   '(anything-c-source-buffers+-howm-title
     anything-c-source-recentf
     anything-c-source-files-in-current-dir+
     anything-c-source-file-cache
     anything-c-source-filelist
     anything-c-source-locate
     anything-c-howm-recent
     anything-c-source-gtags-select
     anything-c-source-bookmarks
     anything-c-source-kill-ring
     anything-c-source-emacs-commands)
   "*my-anything*"))
(global-set-key (kbd "C-;") 'my-anything)
(global-set-key (kbd "C-x C-;") 'anything-call-source)
(setq grep-command "grep -nHr -e ./")
(require 'grep-edit)
(define-key anything-map (kbd "C-v") 'anything-next-source)
(define-key anything-map (kbd "M-v") 'anything-previous-source)

(require 'anything-c-moccur)
(global-set-key (kbd "M-m") 'anything-c-moccur-occur-by-moccur)
(global-set-key (kbd "C-M-m") 'anything-c-moccur-dmoccur)
(add-hook 'dired-mode-hook
	  (lambda ()
	    (local-set-key (kbd "O")
			   'anything-c-moccur-dired-do-moccur-by-moccur)))
(global-set-key (kbd "M-s") 'anything-c-moccur-isearch-forward)
(global-set-key (kbd "M-r") 'anything-c-moccur-isearch-backward)

(require 'ac-anything2)
(define-key ac-complete-mode-map (kbd "C-;") 'ac-anything2)

(setq anything-howm-menu-list
      '(("m [メニュー]" . "(howm-menu)")
	("c [メモを作成]" . "(anything-howm-create-new-memo nil)")
        ("cr[リージョンからメモを作成]" . "(anything-howm-create-new-memo (anything-howm-set-selected-text))")
        ("s [検索]" . "(howm-list-grep-fixed)")
        ("l [一覧]" . "(howm-list-recent)")))

(setq anything-howm-recent-menu-number-limit 100)
(setq anything-howm-data-directory howm-directory)
(defun anything-howm-display-buffer (buf)
  (pop-to-buffer buf))
(global-set-key (kbd "C-c ,") 'anything-cached-howm-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; one-key settings
;;;

(require 'one-key)
(require 'one-key-config)
(require 'my-one-key-config)
(require 'one-key-default)
(one-key-default-setup-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; popwin settings
;;;
;;; (auto-install-from-url "https://github.com/m2ym/popwin-el/raw/master/popwin.el")
;;;

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(setq anything-samewindow nil)
(setq popwin:special-display-config
      (append
       '((" *auto-async-byte-compile*"		:noselect t)
	 ("*anything complete*"			:height 10)
	 ("*anything*"				:height 30)
	 ("*my-anything*"			:height 30)
	 ("*anything-howm-menu*"		:height 30)
	 ("*One-Key*"				:noselect t)
	 ("^\*.*-status.*\*"			:regexp t :height 20)
	 ("^\*dvc-commit.*\*"			:regexp t :noselect t)
	 ("^\*dvc-error.*\*"			:regexp t :noselect t)
	 ("*Async Shell Command*"		:noselect t))
       popwin:special-display-config))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; UI async settings
;;;
;;; (auto-install-from-url "http://github.com/kiwanami/emacs-deferred/raw/master/deferred.el")
;;; (auto-install-from-url "http://github.com/kiwanami/emacs-inertial-scroll/raw/master/inertial-scroll.el")
;;;

(require 'deferred)
(require 'inertial-scroll)
(setq inertias-initial-velocity 50)
(setq inertias-friction 120)
(setq inertias-update-time 60)
(setq inertias-rest-coef 0.1)
(global-set-key (kbd "C-v") 'inertias-up)
(global-set-key (kbd "M-v") 'inertias-down)
(global-set-key [wheel-up] 'inertias-down-wheel)
(global-set-key [wheel-down] 'inertias-up-wheel)

(defun twittering-scroll-up()
  "Scroll up if possible; otherwise invoke `twittering-goto-next-status',
which fetch older tweets on non reverse-mode."
  (interactive)
  (cond
   ((= (point) (point-max))
    (twittering-goto-next-status))
   ((= (window-end) (point-max))
    (goto-char (point-max)))
   (t
    (inertias-up))))

(defun twittering-scroll-down()
  "Scroll down if possible; otherwise invoke `twittering-goto-previous-status',
which fetch older tweets on reverse-mode."
  (interactive)
  (cond
   ((= (point) (point-min))
    (twittering-goto-previous-status))
   ((= (window-start) (point-min))
    (goto-char (point-min)))
   (t
    (inertias-down))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mkpasswd settings
;;;

(defvar mkpasswd-command "head -c 10 < /dev/random | uuencode -m - | tail -n 2 |head -n 1 | head -c10")
(autoload 'mkpasswd "mkpasswd" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; id-manager settings
;;;
;;; (auto-install-from-url "https://github.com/kiwanami/emacs-id-manager/raw/master/id-manager.el")
;;;

(autoload 'id-manager "id-manager" nil t)
(setenv "GPG_AGENT_INFO" nil)
(setq idm-database-file "~/Dropbox/.idm-db.gpg")
(setq idm-copy-action 'kill-new)
(setq idm-gen-password-cmd mkpasswd-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; locate settings
;;;

(setq locate-home-database  (expand-file-name "~/locate.database"))
(setq locate-update-command (expand-file-name
			     (concat user-emacs-directory "locate.updatedb.sh")))
(setq locate-update-command-program-args
      (list "nice" "-n" "19" locate-update-command))

(setq anything-c-locate-command
      (concat "locate -i -d " locate-home-database " %s"))

(defun locate-update-home ()
  "offer to update the locate database in home."
  (interactive)
  (set-process-sentinel
   (apply 'start-process "locate-update-home" "*Messages*"
	  locate-update-command-program-args)
   (lambda (proc stat)
     (if (zerop (process-exit-status proc))
	 (message "locate.updatedb...done")))))
