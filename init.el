;;; init.el --- Emacs initialization file.

;; Author: Kentaro Ohkouchi  <nanasess@fsm.ne.jp>
;; URL: git://github.com/nanasess/dot.emacs.git

;;; Code:

(defvar user-initial-directory (concat user-emacs-directory "init.d/"))
(defvar user-site-lisp-directory (concat user-emacs-directory "site-lisp/"))
(defvar user-misc-directory (concat user-emacs-directory "etc/"))
(defvar user-bin-directory (concat user-emacs-directory "bin/"))
(defvar dropbox-directory (expand-file-name "~/SparkleShare/share/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; initial load files
;;;

(dolist (sys-type (list (symbol-name system-type)
			(symbol-name window-system)))

  (add-to-list 'load-path
	       (expand-file-name
		(concat user-initial-directory "arch/" sys-type)))
  (load "init" t))
(add-to-list 'load-path (expand-file-name user-emacs-directory))
(add-to-list 'load-path (expand-file-name user-initial-directory))
(add-to-list 'load-path (expand-file-name user-site-lisp-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; exec-path settings
;;;

(dolist (dir (list "/sbin" "/usr/sbin" "/bin" "/usr/bin" "/usr/local/bin"
		   "/opt/local/sbin" "/opt/local/bin"
		   (expand-file-name "~/bin")
		   (expand-file-name "~/.emacs.d/bin")
		   (expand-file-name "~/Applications/UpTeX.app/teTeX/bin")))

  (when (and (file-exists-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat dir ":" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Japanesed settings
;;;

(require 'japanese-init)

;;  (auto-install-from-url "http://nijino.homelinux.net/emacs/cp5022x.el")
(require 'cp5022x)
(define-coding-system-alias 'iso-2022-jp 'cp50220)
(define-coding-system-alias 'euc-jp 'cp51932)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SKK settings
;;;

(setq skk-user-directory (concat dropbox-directory "ddskk"))
(setq skk-init-file (concat user-initial-directory "skk-init.el"))
(setq skk-preload t)
(setq skk-auto-save-interval 30)
(setq skk-isearch-start-mode 'latin)
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
(size-indication-mode 1)

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

(setq indicate-empty-lines t)

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

(add-hook 'diff-mode-hook
	  (lambda ()
	    (set-face-foreground 'diff-context-face "grey50")
	    (set-face-foreground 'diff-hunk-header-face "medium blue")
	    (set-face-background 'diff-removed-face "#ffdddd")
	    (set-face-background 'diff-added-face "#ddffdd")
	    (set-face-background 'diff-refine-change "Thistle1")
	    (set-face-bold-p 'diff-refine-change t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; window-system settings
;;;

(cond (window-system
       (tool-bar-mode 0)))

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
;;; cc-mode settings
;;;

;; (define-key c-mode-base-map "*" nil) ; for Emacs 23
(when (<= emacs-major-version 23)
  (add-to-list 'load-path (expand-file-name (concat user-site-lisp-directory "cc"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Indent settings
;;;

(defun basic-indent ()
  (setq tab-width 4)
  (setq indent-tabs-mode nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Emacs lisp settings
;;;

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (setq tab-width 8)
	    (setq indent-tabs-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CSS settings
;;;

(add-hook 'css-mode-hook 'basic-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Java settings
;;;

(add-hook 'java-mode-hook 'basic-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; JavaScript-mode settings
;;;

(add-hook 'javascript-mode-hook 'basic-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; js3-mode settings
;;;
;;; (auto-install-from-url "https://raw.github.com/thomblake/js3-mode/master/js3.el")
;;;

(setq js3-mirror-mode t)
(autoload 'js3-mode "js3" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
(add-hook 'js3-mode-hook
	  (lambda ()
	    (setq js3-indent-level 4)
	    (setq js3-mode-dev-mode-p t)
	    (setq js3-auto-indent-p t)
	    (setq js3-enter-indents-newline t)
	    (setq js3-indent-on-enter-key t)
	    (when (require 'auto-complete nil t)
	      (make-variable-buffer-local 'ac-sources)
	      (add-to-list 'ac-sources 'ac-source-yasnippet)
	      (auto-complete-mode t))))

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
(add-hook 'sql-mode-hook 'basic-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; frame-size settings
;;;

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

(defun toggle-fullscreen ()
  (interactive)
  (if (frame-parameter nil 'fullscreen)
      (set-frame-parameter nil 'fullscreen nil)
    (set-frame-parameter nil 'fullscreen 'fullscreen)))

(global-set-key (kbd "C-z C-a") 'toggle-fullscreen)
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

(require 'gtags)
(setq gtags-path-style 'relative)
(add-hook 'gtags-mode-hook
	  (lambda ()
	    (local-set-key (kbd "M-.") 'gtags-find-tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; goto-chg settings
;;;
;;; (auto-install-from-emacswiki "goto-chg.el")
;;;

(require 'goto-chg)
(global-set-key (kbd "C-.") 'goto-last-change)
(global-set-key (kbd "C-,") 'goto-last-change-reverse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; goto-chg settings
;;;
;;; (auto-install-from-emacswiki "point-undo.el")
;;;

(require 'point-undo)
(define-key global-map (kbd "C-M-,") 'point-undo)
(define-key global-map (kbd "C-M-.") 'point-redo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mmm-mode settings
;;;

(add-to-list 'load-path (expand-file-name (concat user-site-lisp-directory "mmm")))
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(set-face-background 'mmm-default-submode-face "ivory2")
(setq mmm-font-lock-available-p t)
(require 'mmm-sample)
(setq mmm-here-doc-mode-alist
      (append (list '("__EOF__" . sql-mode)
		    '("__EOS__" . sql-mode))
	      mmm-here-doc-mode-alist))
(mmm-add-classes
 '((php-heredoc
    :front "<<<\\s-*[\"\']?\\([a-zA-Z_][a-zA-Z0-9_]+\\)"
    :front-offset (end-of-line 1)
    :back "^\\s-*~1;$"
    :save-matches 1
    :face mmm-code-submode-face
    :delimiter-mode nil
    :match-submode mmm-here-doc-get-mode
    :insert ((?d here-doc "Here-document Name: " @ "<<" str _ "\n"
		 @ "\n" @ str "\n" @)))))

(mmm-add-mode-ext-class nil "\\.tpl?\\'" 'embedded-css)
(mmm-add-mode-ext-class nil "\\.tpl?\\'" 'html-js)
(mmm-add-mode-ext-class nil "\\.php\\'" 'php-heredoc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; org-mode settings
;;;
(when (<= emacs-major-version 23)
  (add-to-list 'load-path (expand-file-name (concat user-site-lisp-directory "org")))
  (require 'org-install))
(require 'ob-sh)
(require 'ob-css)
(require 'ob-sql)
(setq org-startup-truncated nil)
(setq org-startup-folded nil)
(setq org-return-follows-link t)
(org-remember-insinuate)
(setq org-directory (concat dropbox-directory "howm/"))
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

;;; org-export-generic
;; (auto-install-from-url "http://orgmode.org/w/?p=org-mode.git;a=blob_plain;f=contrib/lisp/org-export-generic.el;hb=HEAD")
(load "org-export-generic" t t)
;;; orgmode-markdown
;; (auto-install-from-url "https://raw.github.com/alexhenning/ORGMODE-Markdown/master/markdown.el")
(load "markdown" t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; htmlize settings
;;;

(autoload 'htmlize-buffer "htmlize"
  "Convert BUFFER to HTML, preserving colors and decorations.")
(autoload 'htmlize-region "htmlize"
  "Convert the region to HTML, preserving colors and decorations.")
(autoload 'htmlize-file "htmlize"
  "Load FILE, fontify it, convert it to HTML, and save the result.")

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

(add-to-list 'load-path (expand-file-name (concat user-site-lisp-directory "dvc")))
(require 'dvc-autoloads)
(setq dvc-tips-enabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; vc-svn settings
;;;

(add-to-list 'process-coding-system-alist '("svn" . utf-8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; psvn settings
;;;
;;; (auto-install-from-url "http://svn.apache.org/repos/asf/subversion/trunk/contrib/client-side/emacs/dsvn.el")
;;;

(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; howm settings
;;;

(setq howm-menu-lang 'ja)
(setq howm-directory org-directory)
(setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.howm")
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
(add-to-list 'load-path (expand-file-name (concat user-site-lisp-directory "howm")))
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
;;; quickrun.el settings
;;;
;;; (auto-install-from-url "https://raw.github.com/syohex/emacs-quickrun/master/quickrun.el")
;;;

(require 'quickrun)
(defface phpunit-pass
  '((t (:foreground "white" :background "green" :weight bold))) nil)
(defface phpunit-fail
  '((t (:foreground "white" :background "red" :weight bold))) nil)

(defun quickrun/phpunit-outputter ()
  (save-excursion
    (goto-char (point-min))
    (while (replace-regexp "" "")
      nil))
  (highlight-phrase "^OK.*$" 'phpunit-pass)
  (highlight-phrase "^FAILURES.*$" 'phpunit-fail))

(quickrun-add-command "phpunit" '((:command . "phpunit")
                                  (:exec . "%c %s")
                                  (:outputter . quickrun/phpunit-outputter)))
(add-to-list 'quickrun-file-alist
	     '("\\(Test\\.php\\|TestSuite\\.php\\|AllTests\\.php\\)\\'" . "phpunit"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PHP settings
;;;

(require 'php-mode)

(defconst php-style
  `((c-recognize-knr-p . nil)
    (c-enable-xemacs-performance-kludge-p . t) ; speed up indentation in XEmacs
    (c-basic-offset . 4)
    (indent-tabs-mode . nil)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . ((defun-open after)
                               (defun-close before after)
                               (class-open after)
                               (class-close before after)
                               (namespace-open after)
                               (inline-open after)
                               (inline-close before after)
                               (block-open after)
                               (block-close . c-snug-do-while)
                               (extern-lang-open after)
                               (extern-lang-close after)
                               (statement-case-open after)
                               (substatement-open after)))
    (c-hanging-colons-alist . ((case-label)
                               (label after)
                               (access-label after)
                               (member-init-intro before)
                               (inher-intro)))
    (c-hanging-semi&comma-criteria
     . (c-semi&comma-no-newlines-for-oneline-inliners
        c-semi&comma-inside-parenlist
        c-semi&comma-no-newlines-before-nonblanks))
    (c-indent-comments-syntactically-p . nil)
    (comment-column . 40)
    (c-cleanup-list . (brace-else-brace
                       brace-elseif-brace
                       brace-catch-brace
                       empty-defun-braces
                       defun-close-semi
                       list-close-comma
                       scope-operator))
    (c-offsets-alist . ((func-decl-cont . ++)
                        (member-init-intro . ++)
                        (inher-intro . ++)
                        (comment-intro . 0)
                        (arglist-close . c-lineup-arglist)
                        (topmost-intro . 0)
                        (block-open . 0)
                        (inline-open . 0)
                        (substatement-open . 0)
                        (statement-cont
                         .
                         (,(when (fboundp 'c-no-indent-after-java-annotations)
                             'c-no-indent-after-java-annotations)
                          ,(when (fboundp 'c-lineup-assignments)
                             'c-lineup-assignments)
                          ++))
                        (label . /)
                        (case-label . +)
                        (statement-case-open . +)
                        (statement-case-intro . +) ; case w/o {
                        (access-label . /)
                        (innamespace . 0))))
  "
My PHP Programming Style
see http://google-styleguide.googlecode.com/svn/trunk/google-c-style.el")

(require 'flymake)
(defun flymake-php-init ()
  "Use php to check the syntax of the current file."
  (let* ((temp (flymake-init-create-temp-buffer-copy
		'flymake-create-temp-inplace))
	 (local (file-relative-name temp (file-name-directory buffer-file-name))))
    (list "php" (list "-f" local "-l"))))

(add-to-list 'flymake-err-line-patterns
	     '("\\(Parse\\|Fatal\\) error: +\\(.*?\\) in \\(.*?\\) on line \\([0-9]+\\)$" 3 4 nil 2))
(add-to-list 'flymake-allowed-file-name-masks '("\\.php$" flymake-php-init))

;; (auto-install-from-url "http://stcamp.net/share/php-electric.el")
(require 'php-electric)
(defun php-c-style ()
  (interactive)
  (make-local-variable 'c-tab-always-indent)
  (setq c-tab-always-indent t)
  (gtags-mode 1)
  (php-electric-mode 1)
  (c-toggle-hungry-state 1)
  ;; (c-toggle-auto-hungry-state 1)
  (flymake-mode 1)
  (require 'php-completion)
  (php-completion-mode t)
  (define-key php-mode-map (kbd "C-o") 'phpcmp-complete)
  (define-key php-mode-map [return] 'newline-and-indent)
  (define-key php-mode-map (kbd "C-z C-t") 'quickrun)
  (define-key php-mode-map (kbd "M-p") 'flymake-goto-prev-error)
  (define-key php-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (make-local-variable 'comment-start)
  (setq comment-start "// ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "// *")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (when (require 'auto-complete nil t)
    (make-variable-buffer-local 'ac-sources)
    (add-to-list 'ac-sources
		 'ac-source-php-completion
		 'ac-source-yasnippet)
    (auto-complete-mode t))
  (c-add-style "php-style" php-style t))

(add-to-list 'auto-mode-alist '("\\.\\(inc\\|php[s34]?\\)" . php-mode))
(setq php-mode-force-pear t)
(setq php-manual-url "http://jp2.php.net/manual/ja/")
(setq php-search-url "http://jp2.php.net/")

(add-hook 'php-mode-hook 'php-c-style)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; geben settings
;;;

(autoload 'geben "geben" "DBGp protocol front-end" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; yasnippet settings
;;;

(require 'yasnippet)
(yas/initialize)
(yas/load-directory (expand-file-name (concat user-misc-directory "snippets")))
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
;;; zencoding settings
;;;

(add-to-list 'load-path (expand-file-name (concat user-site-lisp-directory "zencoding")))
(require 'zencoding-mode)
(add-hook 'nxml-mode-hook 'zencoding-mode)
(define-key zencoding-mode-keymap (kbd "<C-return>") 'zencoding-expand-yas)
(define-key zencoding-mode-keymap (kbd "<M-return>") 'zencoding-expand-line)
(define-key zencoding-mode-keymap (kbd "C-j") 'skk-kakutei)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; markdown-mode settings
;;;
;;; (auto-install-from-url "http://jblevins.org/git/markdown-mode.git/plain/markdown-mode.el")
;;;

(autoload 'markdown-mode "markdown-mode" nil t)
(autoload 'gfm-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(markdown\\|md\\)\\'" . gfm-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; csv-mode settings
;;;

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; auto-complete.el settings
;;;

(require 'auto-complete)
(add-to-list 'ac-dictionary-directories (concat user-misc-directory "dict"))
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-show-menu 0.3)
(setq ac-use-menu-map t)
(define-key ac-completing-map [tab] 'ac-complete)
(define-key ac-completing-map [return] 'ac-complete)

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
(setq w3m-init-file (concat user-initial-directory "emacs-w3m-init.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; simple-hatena-mode settings
;;;

(require 'html-helper-mode)
(autoload 'simple-hatena "simple-hatena-mode" nil t)
(add-hook 'simple-hatena-mode-hook
	  (lambda ()
	    (require 'hatena)
	    (setq simple-hatena-default-id "nanasess")
	    (setq simple-hatena-bin (expand-file-name (concat user-bin-directory "hw.pl")))
	    (setq simple-hatena-root howm-directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; twitting-mode settings
;;;
;;; (auto-install-from-url "https://raw.github.com/hayamiz/twittering-mode/master/twittering-mode.el")
;;;

(autoload 'twit "twittering-mode" nil t)
(unless (load "twittering-tinyurl-api-key" t t)
  (setq twittering-bitly-api-key nil))
(setq twittering-auth-method 'xauth)
(setq twittering-username "nanasess")
(setq twittering-bitly-login twittering-username)
(setq twittering-tinyurl-service 'j.mp)
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
(add-hook 'calendar-load-hook
	  (lambda ()
	    (require 'japanese-holidays)
	    (setq calendar-holidays
		  (append japanese-holidays holiday-local-holidays
			  holiday-other-holidays))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; pdf-preview settings
;;;
;;; (auto-install-from-url "http://homepage.mac.com/matsuan_tamachan/emacs/pdf-preview.el")
;;;

(autoload 'pdf-preview-buffer "pdf-preview" nil t)
(autoload 'pdf-preview-buffer-with-faces "pdf-preview" nil t)
(setq ps-print-header nil)
(setq pdf-preview-preview-command "open")
(setq mew-print-function 'pdf-preview-buffer-with-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; auto-install settings
;;;

(require 'auto-install)
(setq auto-install-directory user-site-lisp-directory)

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
;;; (auto-install-from-url "https://raw.github.com/wakaran/anything-howm/master/anything-howm.el")
;;;

(setq anything-howm-use-migemo t)
(setq w3m-command "/opt/local/bin/w3m")
(require 'anything-startup)
(require 'anything-howm)
(require 'anything-gtags)
(setq anything-candidate-number-limit 500)

(defun my-anything ()
  "Anything command for you.
It is automatically generated by `anything-migrate-sources'."
  (interactive)
  (anything-other-buffer
   '(anything-c-source-ffap-line
     anything-c-source-ffap-guesser
     anything-c-source-buffers+-howm-title
     anything-c-source-recentf
     anything-c-source-files-in-current-dir+
     anything-c-source-file-cache
     anything-c-source-filelist
     anything-c-source-locate
     anything-c-source-gtags-select
     anything-c-source-bookmarks
     anything-c-source-kill-ring)
   "*my-anything*"))
(global-set-key (kbd "C-;") 'my-anything)
(global-set-key (kbd "C-x C-;") 'anything-call-source)
(global-set-key (kbd "C-z C-r") 'anything-resume)
(setq grep-command "ack -af | xargs grep -Hin ")

(define-key anything-map (kbd "C-v") 'anything-next-source)
(define-key anything-map (kbd "M-v") 'anything-previous-source)

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
(global-set-key (kbd "C-z ,") 'anything-howm-menu-command)
(global-set-key (kbd "C-z .") 'anything-howm-resume)

(require 'anything-gist)

(setq shell-history-file "~/.zsh/.zsh-history")
(require 'shell-history)
(require 'anything-c-shell-history)
(setq anything-c-shell-history-file shell-history-file)

;; anything in dired
;; see. http://d.hatena.ne.jp/syohex/20120105/1325770778
(defun my/anything-dired ()
  ""
  (interactive)
  (let ((curbuf (current-buffer)))
    (if (anything-other-buffer
         '(anything-c-source-files-in-current-dir+)
         " *anything-dired*")
        (kill-buffer curbuf))))

(define-key dired-mode-map (kbd "p") 'my/anything-dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; one-key settings
;;;

;; (require 'one-key)
;; (require 'one-key-config)
;; (require 'my-one-key-config)
;; (require 'one-key-default)
;; (one-key-default-setup-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; popwin settings
;;;
;;; (auto-install-from-url "https://raw.github.com/m2ym/popwin-el/master/popwin.el")
;;;

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(setq anything-samewindow nil)
(setq popwin:special-display-config
      (append
       '(("*Async Shell Command*"		:noselect t)
	 ("^\*dvc-commit.*\*"			:regexp t :noselect t)
	 ("^\*bzr-status.*\*"			:regexp t :noselect t)
	 ("^\*xgit-status.*\*"			:regexp t :noselect t)
	 ("^\*dvc-error.*\*"			:regexp t :noselect t)
	 ("*quickrun*"				:noselect t))
       popwin:special-display-config))

;; (global-set-key (kbd "C-x C-p") popwin:keymap)
(setq auto-async-byte-compile-display-function 'popwin:popup-buffer-tail)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; e2wm settings
;;;
;;; (auto-install-from-url "https://raw.github.com/kiwanami/emacs-window-layout/master/window-layout.el")
;;; (auto-install-from-url "https://raw.github.com/kiwanami/emacs-window-manager/master/e2wm.el")
;;; (auto-install-from-url "https://raw.github.com/gist/1842966/98b5f0596096b138009bffcd5d2e3609719fb5d5/e2wm-edbi-pre.el")
;;;

(autoload 'e2wm:start-management "e2wm" nil t)
(setq e2wm:def-plugin-clock-text t)
(global-set-key (kbd "M-+") 'e2wm:start-management)

(add-hook 'e2wm:pre-start-hook
	  (lambda ()
	    (load "e2wm-edbi")
	    (global-set-key (kbd "C-z C-c") 'e2wm:dp-code)
	    (global-set-key (kbd "C-z C-d") 'e2wm:dp-edbi)
	    (global-set-key (kbd "C-z 2") 'e2wm:dp-two)
	    (global-set-key (kbd "C-z 1")
			    'e2wm:dp-code-main-maximize-toggle-command)
	    (global-set-key (kbd "C-z Q") 'e2wm:stop-management)))

(defun e2wm:current-buffer ()
  (cond
   ((e2wm:managed-p)
    (e2wm:history-get-main-buffer))
   ((featurep 'elscreen)
    (let* ((frame-confs (elscreen-get-frame-confs (selected-frame)))
           (num (nth 1 (assoc 'screen-history frame-confs)))
           (cur-window-conf (cadr (assoc num (assoc 'screen-property frame-confs))))
           (marker (nth 2 cur-window-conf)))
      (marker-buffer marker)))
   (t
    (nth 1
         (assoc 'buffer-list
                (nth 1 (nth 1 (current-frame-configuration))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Emacs DBI settings
;;;
;;; (auto-install-from-url "https://raw.github.com/kiwanami/emacs-deferred/master/concurrent.el")
;;; (auto-install-from-url "https://raw.github.com/kiwanami/emacs-ctable/master/ctable.el")
;;; (auto-install-from-url "https://raw.github.com/kiwanami/emacs-epc/master/epc.el")
;;; (auto-install-from-url "https://raw.github.com/kiwanami/emacs-edbi/master/edbi.el")
;;; (auto-install-from-url "https://raw.github.com/kiwanami/emacs-edbi/master/edbi-bridge.pl")
;;; cpan RPC::EPC::Service DBI DBD::SQLite DBD::Pg DBD::mysql
;;; dbi:Pg:dbname=dbname;host=hostname;password=password
;;;

(autoload 'e2wm:dp-edbi "edbi" nil t)
(setq edbi:query-result-fix-header nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mkpasswd settings
;;;

(defvar mkpasswd-command
  "head -c 10 < /dev/random | uuencode -m - | tail -n 2 |head -n 1 | head -c10")
(autoload 'mkpasswd "mkpasswd" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; id-manager settings
;;;
;;; (auto-install-from-url "https://github.com/kiwanami/emacs-id-manager/raw/master/id-manager.el")
;;;

(autoload 'id-manager "id-manager" nil t)
(setenv "GPG_AGENT_INFO" nil)
(setq idm-database-file (concat dropbox-directory ".idm-db.gpg"))
(setq idm-copy-action 'kill-new)
(setq idm-gen-password-cmd mkpasswd-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; term+.el
;;;
;;; https://github.com/tarao/term-plus-el
;;; https://raw.github.com/tarao/dotfiles/master/.zsh/eterm.zsh
;;;

(add-to-list 'load-path (expand-file-name
			 (concat user-site-lisp-directory "term-plus")))
(add-to-list 'load-path (expand-file-name
			 (concat user-site-lisp-directory "evil")))
(add-to-list 'load-path (expand-file-name
			 (concat user-site-lisp-directory "multi-mode-util")))

(require 'term+mux)
(require 'xterm-256color)
(require 'key-intercept)
(require 'multi-mode-util)
(require 'term+evil)
(require 'term+anything-shell-history)

(add-hook 'term+char-mode-hook
	  (lambda ()
	    (define-key term+char-map (kbd "M-w") #'term+mark-or-copy)
	    (define-key term+char-map (kbd "C-t C-t") #'other-window)))

(setq undo-tree-mode-lighter " uT")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; locate settings
;;;

(defvar locate-home-database (expand-file-name "~/locate.database"))
(defvar locate-update-command
  (expand-file-name (concat user-bin-directory "locate.updatedb.sh")))
(defvar locate-update-command-program-args
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; apachectl settings
;;;
;;;

(defvar apachectl-program-command "/opt/local/apache2/bin/apachectl")
(defvar apachectl-buffer-name "*apachectl*")
(defun executable-apachectl (args)
"Executable apachectl command.
required setting with sudoers.

e.g.)
username ALL=NOPASSWD: /opt/local/apache2/bin/apachectl configtest,\\
         /opt/local/apache2/bin/apachectl start,\\
         /opt/local/apache2/bin/apachectl stop,\\
         /opt/local/apache2/bin/apachectl graceful,\\
         /opt/local/apache2/bin/apachectl restart
"
  (interactive)
  (let ((apachectl-command (list "sudo" apachectl-program-command args)))
    (with-current-buffer (get-buffer-create apachectl-buffer-name)
      (erase-buffer)
      (buffer-disable-undo)
      (set-process-sentinel
       (apply 'start-process (format "apachectl %s" args) (current-buffer)
	      apachectl-command)
       (lambda (proc stat)
	 (cond ((zerop (process-exit-status proc))
		(message "%s... successful!" proc))
	       ((popwin:popup-buffer-tail apachectl-buffer-name)
		(error "%s... failur!" proc))))))))
(defun apachectl/start ()
  (interactive)
  (executable-apachectl "start"))
(defun apachectl/stop ()
  (interactive)
  (executable-apachectl "stop"))
(defun apachectl/restart ()
  (interactive)
  (executable-apachectl "restart"))
(defun apachectl/configtest ()
  (interactive)
  (executable-apachectl "configtest"))
(defun apachectl/graceful ()
  (interactive)
  (executable-apachectl "graceful"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; convert to path settings
;;;

(defun convert-win-to-mac-path()
  (interactive)
  (let ((buf (get-buffer-create "*convert*")) str)
    (setq str (buffer-substring (region-beginning) (region-end)))
    (with-current-buffer
	buf (setq ret (buffer-string))
	(setq str (replace-regexp-in-string
		   "\\\\\\\\[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+" "/Volumes" str))
	(setq str (replace-regexp-in-string "\\\\" "/" str))
	(insert str))
    (kill-buffer buf)
    (message "%s" str)
    (kill-new str)
    (delete-region (region-beginning) (region-end))
    (insert str)))

(defun convert-smb-to-win-path()
  (interactive)
  (let ((buf (get-buffer-create "*convert*")) str)
    (setq str (buffer-substring (region-beginning) (region-end)))
    (with-current-buffer
	buf (setq ret (buffer-string))
	(setq str (ucs-normalize-NFC-string str))
	(setq str (replace-regexp-in-string "smb://" "\\\\\\\\" str))
	(setq str (replace-regexp-in-string "/" "\\\\" str))
	(insert str))
    (kill-buffer buf)
    (message "%s" str)
    (kill-new str)
    (delete-region (region-beginning) (region-end))
    (insert str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; redmine.el settings
;;;
;;; (auto-install-from-url "https://raw.github.com/nanasess/redmine-el/master/redmine.el")
;;;

(load "redmine-config" t t)
