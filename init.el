;;; init.el --- Emacs initialization file.

;; Author: Kentaro Ohkouchi  <nanasess@fsm.ne.jp>
;; URL: git://github.com/nanasess/dot.emacs.git

;;; Code:

(setq gc-cons-threshold (* 128 1024 1024))
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(defvar user-initial-directory (locate-user-emacs-file "init.d/"))
(defvar user-site-lisp-directory (locate-user-emacs-file "site-lisp/"))
(defvar user-misc-directory (locate-user-emacs-file "etc/"))
(defvar user-bin-directory (locate-user-emacs-file "bin/"))
(defvar external-directory (expand-file-name "~/GoogleDrive/share/"))

;; Silence the warning.
(defun el-get (&optional sync &rest packages))
(defun smartrep-define-key (keymap prefix alist))
(defun global-undo-tree-mode () ())
(defun quickrun-add-command (key alist))
(defun c-toggle-hungry-state (arg))
(defun php-completion-mode (arg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; el-get settings
;;;
(eval-after-load "el-get"
  '(progn
     (add-to-list 'el-get-recipe-path (locate-user-emacs-file "recipes"))))
(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))
(add-to-list 'load-path (concat user-emacs-directory "el-get/ddskk"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get 'sync)

;; (el-get 'sync 'esup)
;; (require 'esup)
(el-get 'sync 'cp5022x)
(define-coding-system-alias 'iso-2022-jp 'cp50220)
(define-coding-system-alias 'euc-jp 'cp51932)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SKK settings
;;;

(el-get 'sync 'ddskk)
(custom-set-variables
 '(skk-user-directory (concat external-directory "ddskk"))
 '(skk-init-file (concat user-initial-directory "skk-init.el"))
 '(skk-preload t)
 '(skk-isearch-start-mode 'latin))

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
(global-unset-key (kbd "C-\\"))
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-j") 'skk-mode)
(global-set-key (kbd "C-x C-j") 'skk-mode)
(global-set-key (kbd "C-t") 'other-window)
(global-set-key (kbd "C-z C-u") 'other-frame)
(global-set-key (kbd "C-z C-j") 'toggle-skk-kutouten)

(global-set-key (kbd "C-M-g") 'end-of-buffer)
(global-set-key (kbd "C-M-j") 'next-line)
(global-set-key (kbd "C-M-k") 'previous-line)
(global-set-key (kbd "C-M-h") 'backward-char)
(global-set-key (kbd "C-M-l") 'forward-char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; backup files settings
;;;

(custom-set-variables
 '(make-backup-files t)
 '(backup-directory-alist
   (cons (cons "\\.*$" (expand-file-name "~/.bak/"))
	 backup-directory-alist))
 '(version-control t)
 '(delete-old-versions t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; recentf settings
;;;

(el-get 'sync 'recentf-ext)
(custom-set-variables
 '(recentf-max-saved-items 50000))

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

(custom-set-variables '(indicate-empty-lines t)
		      '(eol-mnemonic-dos "(DOS)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; face settings
;;;

;; (set-background-color "ivory")
;; (set-face-foreground 'font-lock-keyword-face "#7f007f")

(defface hlline-face
  '((((class color) (background light))
     (:background "Beige"))) nil :group 'font-lock-highlighting-faces)
(custom-set-variables
 '(hl-line-face 'hlline-face))

(custom-set-variables
 '(whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark)))

(defface my-mark-whitespace '((t (:background "gray"))) nil
  :group 'font-lock-highlighting-faces)
(defface my-mark-tabs '((t (:background "Gainsboro"))) nil
  :group 'font-lock-highlighting-faces)
(defface my-mark-lineendsspaces '((t (:foreground "SteelBlue" :underline t))) nil
  :group 'font-lock-highlighting-faces)
(defvar my-mark-whitespace 'my-mark-whitespace)
(defvar my-mark-tabs 'my-mark-tabs)
(defvar my-mark-lineendsspaces 'my-mark-lineendsspaces)

(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("\t" 0 my-mark-tabs append)
     ("　" 0 my-mark-whitespace append)
     ("[ \t]+$" 0 my-mark-lineendsspaces append))))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

(add-hook 'diff-mode-hook
	  #'(lambda ()
	      ;; (set-face-foreground 'diff-context-face "grey50")
	      ;; (set-face-foreground 'diff-hunk-header-face "medium blue")
	      ;; (set-face-background 'diff-removed-face "#ffdddd")
	      ;; (set-face-background 'diff-added-face "#ddffdd")
	      ;; (set-face-background 'diff-refine-change "Thistle1")
	      (set-face-bold-p 'diff-refine-change t)))

;; see also http://rubikitch.com/2015/05/14/global-hl-line-mode-timer/
(global-hl-line-mode 0)
(defun global-hl-line-timer-function ()
  (global-hl-line-unhighlight-all)
  (let ((global-hl-line-mode t))
    (global-hl-line-highlight)))
(setq global-hl-line-timer
      (run-with-idle-timer 0.1 t 'global-hl-line-timer-function))
;; (cancel-timer global-hl-line-timer)

;; use solarized.
;; (el-get 'sync 'solarized-theme)
(el-get 'sync 'plan9-theme)
(require 'plan9-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; window-system settings
;;;

(cond (window-system (tool-bar-mode 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; uniquify settings
;;;

(if (not (version< "24.3.99" emacs-version))
    (require 'uniquify)
  (custom-set-variables
   '(uniquify-buffer-name-style 'post-forward-angle-brackets)
   '(uniquify-ignore-buffers-re "*[^*]+*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; dired-x settings
;;;

(custom-set-variables '(dired-bind-jump nil))
(add-hook 'dired-mode-hook
	  #'(lambda ()
	      (local-set-key (kbd "C-t") 'other-window)
	      (local-set-key (kbd "r") 'wdired-change-to-wdired-mode)))
(add-hook 'dired-load-hook
	  #'(lambda ()
	      (load "dired-x")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Indent settings
;;;

(defun basic-indent ()
  (setq tab-width 4)
  (setq indent-tabs-mode nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Misc settings
;;;

(custom-set-variables '(x-select-enable-clipboard t)
		      '(x-select-enable-primary t)
		      '(save-interprogram-paste-before-kill t)
		      '(mouse-yank-at-point t)
		      '(visible-bell t)
		      '(ediff-window-setup-function 'ediff-setup-windows-plain))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Emacs lisp settings
;;;

(add-hook 'emacs-lisp-mode-hook
	  #'(lambda ()
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
(eval-after-load "cc-mode"
  '(progn
     (define-key java-mode-map [return] 'newline-and-indent)))
(add-to-list 'auto-mode-alist
	     '("\\.\\(cls\\|trigger\\)\\'" . java-mode))
;; (require 'cedet)
;; (el-get 'sync 'malabar-mode)
;; (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
;; (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
;;                                   global-semanticdb-minor-mode
;;                                   global-semantic-idle-summary-mode
;;                                   global-semantic-mru-bookmark-mode))
;; (semantic-mode 1)
;; (add-hook 'malabar-mode-hook
;; 	  #'(lambda ()
;; 	      (add-hook 'after-save-hook 'malabar-compile-file-silently nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; JavaScript-mode settings
;;;

;; (add-hook 'javascript-mode-hook 'basic-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; js2-mode settings
;;;

(el-get 'sync 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(custom-set-variables
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t))
(add-hook 'js-mode-hook 'js2-minor-mode)
(eval-after-load "js2-mode"
  '(progn
     (electric-indent-mode 0)
     (define-key js2-mode-map (kbd "RET") 'js2-line-break)))
(defun disabled-indent-tabs-mode ()
  (set-variable 'indent-tabs-mode nil))
(add-hook 'js2-mode-hook 'disabled-indent-tabs-mode)

;; (el-get 'sync 'jade-mode)
;; (require 'sws-mode)
;; (require 'jade-mode)
;; (add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
;; (add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; nXML-mode settings
;;;

(add-to-list 'auto-mode-alist
	     '("\\.\\(xml\\|xsl\\|rng\\)\\'" . nxml-mode))
(add-to-list 'auto-mode-alist
	     '("\\.\\(html\\|tpl\\)\\'" . nxml-web-mode))

(add-hook 'nxml-mode-hook
	  #'(lambda ()
	      (rng-validate-mode 0)
	      (custom-set-variables
	       '(auto-fill-mode -1)
	       '(nxml-slash-auto-complete-flag t)
	       '(nxml-child-indent 2)
	       '(indent-tabs-mode nil)
	       '(tab-width 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; yaml-mode settings
;;;

(el-get 'yaml-mode)
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
	  #'(lambda ()
	      (define-key yaml-mode-map "\C-m" 'newline-and-indent)
	      (setq yaml-indent-offset 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SQL settings
;;;

(custom-set-variables '(sql-product 'postgres))
(add-hook 'sql-mode-hook 'basic-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; view-mode settings
;;;

(add-hook 'view-mode-hook
	  #'(lambda ()
	      (setq view-read-only t)
	      (auto-revert-mode 1)
	      (setq line-move-visual nil)))
(eval-after-load "view"
  '(progn
     (define-key view-mode-map (kbd "h") 'backward-word)
     (define-key view-mode-map (kbd "l") 'forward-word)
     (define-key view-mode-map (kbd "j") 'next-line)
     (define-key view-mode-map (kbd "k") 'previous-line)
     (define-key view-mode-map " " 'scroll-up)
     (define-key view-mode-map (kbd "b") 'scroll-down)))
(add-to-list 'auto-mode-alist '("\\.log$" . view-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; frame-size settings
;;;

(defvar normal-frame-width 82)
(defvar wide-frame-width 175)
(defvar toggle-frame-flag t)

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

;; (el-get 'sync 'migemo)
(defvar migemo-dictionary
  (concat external-directory "migemo/dict/utf-8/migemo-dict"))
(custom-set-variables
 '(isearch-lax-whitespace nil))
(when (file-exists-p migemo-dictionary)
  (custom-set-variables
   '(migemo-command "cmigemo")
   '(migemo-options '("-q" "--emacs" "-i" "\a"))
   '(migemo-user-dictionary nil)
   '(migemo-regex-dictionary nil)
   '(migemo-use-pattern-alist t)
   '(migemo-use-frequent-pattern-alist t)
   '(migemo-pattern-alist-length 1000)
   '(migemo-coding-system 'utf-8-unix))
  (require 'migemo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; goto-chg settings
;;;
;;; (auto-install-from-emacswiki "goto-chg.el")
;;;

(el-get 'sync 'goto-chg)
(global-set-key (kbd "C-.") 'goto-last-change)
(global-set-key (kbd "C-,") 'goto-last-change-reverse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; point-undo settings
;;;
;;; (auto-install-from-emacswiki "point-undo.el")
;;;

;; (el-get 'sync 'point-undo)
;; (require 'point-undo)
;; (define-key global-map (kbd "C-M-,") 'point-undo)
;; (define-key global-map (kbd "C-M-.") 'point-redo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; visual-regexp settings
;;;

(el-get 'sync 'visual-regexp)
(define-key global-map (kbd "M-%") 'vr/query-replace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mmm-mode settings
;;;

(el-get 'sync 'mmm-mode)
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(set-face-background 'mmm-default-submode-face "ivory2")
(custom-set-variables '(mmm-font-lock-available-p t))
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

(mmm-add-mode-ext-class nil "\\.\\(html\\|tpl\\)\\'" 'embedded-css)
(mmm-add-mode-ext-class nil "\\.\\(html\\|tpl\\)\\'" 'html-js)
(mmm-add-mode-ext-class nil "\\.php\\'" 'php-heredoc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; org-mode settings
;;;
(custom-set-variables
 '(org-startup-truncated nil)
 '(org-startup-folded nil)
 '(org-return-follows-link t)
 '(org-directory (concat external-directory "howm/")))

;;; org-html5presentation
;; (el-get 'sync 'org-html5presentation)

;;; org-tree-slide
;; http://pastelwill.jp/wiki/doku.php?id=emacs:org-tree-slide
;; (el-get 'sync 'org-tree-slide)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; htmlize settings
;;;

(el-get 'sync 'htmlize)
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

(el-get 'sync 'session)
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(setq session-save-print-spec '(t nil 40000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; cua-mode settings
;;;

(if (not (version< "24.3.99" emacs-version))
    (cua-mode t)
  (custom-set-variables '(cua-enable-cua-keys nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; expand-region settings
;;;

(el-get 'sync 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(el-get 'sync 'multiple-cursors)
(el-get 'sync 'smartrep)

(global-set-key (kbd "<C-M-return>") 'mc/edit-lines)
(smartrep-define-key
    global-map "C-z" '(("C-n" . 'mc/mark-next-like-this)
		       ("C-p" . 'mc/mark-previous-like-this)
		       ("*"   . 'mc/mark-all-like-this)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; undo-tree settings
;;;

(el-get 'sync 'undo-tree)
(global-undo-tree-mode)
(custom-set-variables '(undo-tree-mode-lighter " uT"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; vc-svn settings
;;;

(add-to-list 'process-coding-system-alist '("svn" . utf-8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; psvn settings
;;;

(el-get 'sync 'dsvn)
(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; magit settings
;;;

(el-get 'sync 'magit)
(add-to-list 'load-path (concat user-emacs-directory "el-get/magit"))
(require 'magit)
(require 'magit-svn)

;; see also https://github.com/magit/magit/issues/1318#issuecomment-71192459
(defun magit-expand-git-file-name--msys (args)
  "Handle Msys directory names such as /c/* by changing them to C:/*"
  (let ((filename (car args)))
        (when (string-match "^/\\([a-z]\\)/\\(.*\\)" filename)
          (setq filename (concat (match-string 1 filename) ":/"
                                 (match-string 2 filename))))
        (list filename)))
(advice-add 'magit-expand-git-file-name :filter-args #'magit-expand-git-file-name--msys)

(set-face-attribute 'magit-item-highlight nil
		    :inherit nil)
(global-set-key (kbd "C-z m") 'magit-status)
(define-key magit-log-mode-map (kbd "j") 'magit-goto-next-section)
(define-key magit-log-mode-map (kbd "k") 'magit-goto-previous-section)

(setq magit-last-seen-setup-instructions "1.4.0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; howm settings
;;;

(el-get 'sync 'howm)
(defvar howm-menu-lang 'ja)
;; (defvar howm-directory org-directory)
(defvar howm-directory (concat external-directory "howm/"))
(defvar howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.txt")
(defvar howm-history-file (concat howm-directory ".howm-history"))
(defvar howm-keyword-file (concat howm-directory ".howm-keys"))
(defvar howm-menu-schedule-days-before 30)
(defvar howm-menu-schedule-days 30)
(defvar howm-menu-expiry-hours 2)
(defvar howm-menu-refresh-after-save nil)
(defvar howm-refresh-after-save nil)
(defvar howm-list-all-title t)
(defvar howm-schedule-menu-types "[!@\+]")
(add-hook 'markdown-mode-hook 'howm-mode)
(add-hook 'org-mode-hook 'howm-mode)
(defvar howm-view-title-header "Title:")
(defvar howm-view-use-grep nil)
(add-to-list 'auto-mode-alist '("\\.txt$" . gfm-mode))
(require 'howm)
(setq howm-template
      (concat howm-view-title-header
	      (concat
	       " %title%cursor\n"
	       "Date: %date\n\n"
	       "%file\n\n")
	      (concat
	       "<!--\n"
	       "  Local Variables:\n"
	       "  mode: gfm \n"
	       "  coding: utf-8-unix\n"
	       "  End:\n"
	       "-->\n")))
(defun howm-save-and-kill-buffer ()
  "kill screen when exiting from howm-mode"
  (interactive)
  (let* ((file-name (buffer-file-name)))
    (when (and file-name (string-match "\\.txt" file-name))
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
	  #'(lambda ()
	      (define-key howm-mode-map (kbd "C-c C-q") 'howm-save-and-kill-buffer)))

(global-set-key (kbd "C-z c") 'howm-create)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; quickrun.el settings
;;;

(el-get 'sync 'quickrun)
(defface phpunit-pass
  '((t (:foreground "white" :background "green" :weight bold))) nil
  :group 'font-lock-highlighting-faces)
(defface phpunit-fail
  '((t (:foreground "white" :background "red" :weight bold))) nil
  :group 'font-lock-highlighting-faces)

(defun quickrun/phpunit-outputter ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "" nil t)
      (replace-match "" nil nil)))
  (highlight-phrase "^OK.*$" 'phpunit-pass)
  (highlight-phrase "^FAILURES.*$" 'phpunit-fail))

(quickrun-add-command "phpunit" '((:command . "phpunit")
                                  (:exec . "%c %s")
                                  (:outputter . quickrun/phpunit-outputter)))

(eval-after-load "quickrun"
  '(progn
     (add-to-list
      'quickrun-file-alist
      '("\\(Test\\.php\\|TestSuite\\.php\\|AllTests\\.php\\)\\'" . "phpunit"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; yasnippet settings
;;;

(el-get 'sync 'yasnippet)
(yas-global-mode 1)
(custom-set-variables '(yas-prompt-functions '(yas-dropdown-prompt
					       yas-ido-prompt
					       yas-completing-prompt)))
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
(add-hook 'org-mode-hook
          #'(lambda ()
	      ;; yasnippet (using the new org-cycle hooks)
	      (custom-set-variables '(ac-use-overriding-local-map t))
	      (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)))
(eval-after-load "yasnippet"
  '(progn
     (define-key yas/keymap [tab] 'yas/next-field)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; auto-complete.el settings
;;;

(el-get 'sync 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
	     (expand-file-name
	      (concat user-site-lisp-directory "auto-complete/dict")))
(ac-config-default)
(setq ac-auto-show-menu 0.3)
(setq ac-use-menu-map t)
(define-key ac-completing-map [tab] 'ac-complete)
(define-key ac-completing-map [return] 'ac-complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PHP settings
;;;

(el-get 'sync 'php-mode)
(el-get 'sync 'php-electric)
(el-get 'sync 'php-completion)

(defun php-c-style ()
  (interactive)
  (c-toggle-hungry-state 1)
  ;; (c-toggle-auto-hungry-state 1)
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-start-skip) "// *")
  (set (make-local-variable 'comment-end) ""))

(add-to-list 'auto-mode-alist '("\\.\\(inc\\|php[s34]?\\)$" . php-mode))
(custom-set-variables '(php-mode-coding-style 'psr2)
		      '(php-manual-url "http://jp2.php.net/manual/ja/")
		      '(php-search-url "http://jp2.php.net/"))

(setq browse-url-browser-function 'eww-browse-url)

(add-hook 'php-mode-hook 'php-c-style)
(add-hook 'php-mode-hook 'helm-gtags-mode)

(eval-after-load "php-mode"
  '(progn
     (setq yas-trigger-key (kbd "<tab>"))
     (define-key php-mode-map [return] 'newline-and-indent)
     (define-key php-mode-map (kbd "C-z C-t") 'quickrun)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; markdown-mode settings
;;;

(el-get 'sync 'markdown-mode)
(autoload 'markdown-mode "markdown-mode" nil t)
(autoload 'gfm-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(markdown\\|md\\)\\'" . gfm-mode))

;; see also http://stackoverflow.com/questions/14275122/editing-markdown-pipe-tables-in-emacs
(defun cleanup-org-tables ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "-+-" nil t) (replace-match "-|-"))))
(add-hook 'markdown-mode-hook 'orgtbl-mode)
(add-hook 'markdown-mode-hook
          #'(lambda()
	      (add-hook 'after-save-hook 'cleanup-org-tables  nil 'make-it-local)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; csv-mode settings
;;;

(el-get 'sync 'csv-mode)
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew settings
;;;

;; (el-get 'sync 'mew)
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
;; mm-version
(require 'mm-version)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; twitting-mode settings
;;;

(el-get 'sync 'twittering-mode)
(autoload 'twit "twittering-mode" nil t)
(unless (load "twittering-tinyurl-api-key" t t)
  (defvar twittering-bitly-api-key nil))
(custom-set-variables
 '(twittering-auth-method 'xauth)
 '(twittering-username "nanasess")
 '(twittering-bitly-login twittering-username)
 '(twittering-tinyurl-service 'j.mp)
 '(twittering-status-format (concat "%i %S(%s),  %@:\n%"
	     "FILL[  ]{%T // from %f%L%r%R}\n "))
 '(twittering-retweet-format "RT @%s: %t")
 '(twittering-display-remaining t)
 '(twittering-allow-insecure-server-cert nil))
(eval-after-load "twittering-mode"
  '(progn
     (define-key twittering-mode-map
       (kbd "s") 'twittering-current-timeline)
     (define-key twittering-mode-map
       (kbd "w") 'twittering-update-status-interactive)
     (define-key twittering-edit-mode-map
       (kbd "C-c C-q") 'twittering-edit-cancel-status)
     (define-key twittering-edit-mode-map
       (kbd "C-u C-u") 'twittering-edit-replace-at-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; pdf-preview settings
;;;

;; (autoload 'pdf-preview-buffer "pdf-preview" nil t)
;; (autoload 'pdf-preview-buffer-with-faces "pdf-preview" nil t)
;; (defvar ps-print-header nil)
;; (defvar pdf-preview-preview-command "open")
;; (defvar mew-print-function 'pdf-preview-buffer-with-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; auto-async-byte-compile settings
;;;

;; (el-get 'sync 'auto-async-byte-compile)
;; (require 'auto-async-byte-compile)
;; (setq auto-async-byte-compile-exclude-files-regexp "/mac/") ;dummy
;; (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; helm settings
;;;

(el-get 'sync 'helm)
(el-get 'sync 'helm-migemo)
(el-get 'sync 'helm-ag)
(el-get 'sync 'helm-ack)
(el-get 'sync 'helm-gtags)
(el-get 'sync 'helm-cmd-t)
(el-get 'sync 'helm-descbinds)

(defadvice helm-grep-highlight-match (around ad-helm-grep-highlight-match activate)
  (ad-set-arg 1 t)
  ad-do-it)

(custom-set-variables
 '(helm-mode t)
 '(helm-input-idle-delay 0.2)
 '(helm-buffer-max-length 40)
 '(helm-ff-auto-update-initial-value nil)
 '(helm-truncate-lines t)
 '(helm-for-files-preferred-list
   '(helm-source-buffers-list
	helm-source-recentf
	helm-source-bookmarks
	helm-source-file-cache
	helm-source-files-in-current-dir
	helm-source-mac-spotlight
	helm-source-buffer-not-found))
 '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
 '(helm-ag-command-option "--all-text")
 '(helm-ag-thing-at-point 'symbol)
 '(helm-c-ack-thing-at-point 'symbol)
 '(helm-gtags-path-style 'relative)
 '(helm-gtags-ignore-case t)
 '(enable-recursive-minibuffers t))

;; grep for euc-jp.
;; (setq helm-grep-default-command "grep -a -d skip %e -n%cH -e `echo %p | lv -Ia -Oej` %f | lv -Os -Ia ")
;; (setq helm-grep-default-recurse-command "grep -a -d recurse %e -n%cH -e `echo %p | lv -Ia -Oej` %f | lv -Os -Ia ")

(el-get 'sync 'wgrep)
(custom-set-variables
 '(wgrep-enable-key "r"))

(add-hook 'helm-gtags-mode-hook
	  #'(lambda ()
	      (local-set-key (kbd "M-.") 'helm-gtags-find-tag)))

(require 'helm-C-x-b)

(global-set-key (kbd "C-;") 'helm-for-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-z C-r") 'helm-resume)
(global-set-key (kbd "C-z C-f") 'helm-mac-spotlight)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-h b") 'helm-descbinds)
(global-set-key (kbd "C-z l") 'helm-C-x-b)

(eval-after-load "helm"
  '(progn
     (define-key helm-map (kbd "C-v") 'helm-next-source)
     (define-key helm-map (kbd "M-v") 'helm-previous-source)
     (defun helm-mac-spotlight ()
       "Preconfigured `helm' for `mdfind'."
       (interactive)
       (let ((helm-ff-transformer-show-only-basename nil))
	 (helm-other-buffer 'helm-source-mac-spotlight "*helm mdfind*")))))

;;;
;;; see http://www49.atwiki.jp/ntemacs/pages/32.html
;;;

(defadvice helm-reduce-file-name (around ad-helm-reduce-file-name activate)
  (let ((fname (ad-get-arg 0))
        (level (ad-get-arg 1)))
    (while (> level 0)
      (setq fname (expand-file-name (concat fname "/../")))
      (setq level (1- level)))
    (setq ad-return-value fname)))

(defadvice helm-completing-read-default-1 (around ad-helm-completing-read-default-1 activate)
  (if (listp (ad-get-arg 4))
      (ad-set-arg 4 (car (ad-get-arg 4))))
  (cl-letf (((symbol-function 'regexp-quote)
          (symbol-function 'identity)))
    ad-do-it))

(defadvice find-file (around ad-find-file activate)
  (let ((current-prefix-arg nil))
    ad-do-it))

(require 'helm-howm)
(defvar hh:howm-data-directory howm-directory)
(custom-set-variables
 '(hh:menu-list nil)
 '(hh:recent-menu-number-limit 100))

(defun helm-howm-do-grep ()
  (interactive)
  (helm-do-grep-1
   (list (car (split-string hh:howm-data-directory "\n"))) '(4) nil '("*.txt")))

(global-set-key (kbd "C-z ,") 'hh:menu-command)
(global-set-key (kbd "C-z .") 'hh:resume)
(global-set-key (kbd "C-z s") 'helm-howm-do-grep)

(eval-after-load "helm-migemo"
  '(defun helm-compile-source--candidates-in-buffer (source)
     (helm-aif (assoc 'candidates-in-buffer source)
         (append source
                 `((candidates
                    . ,(or (cdr it)
                           (lambda ()
                             ;; Do not use `source' because other plugins
                             ;; (such as helm-migemo) may change it
                             (helm-candidates-in-buffer (helm-get-current-source)))))
                   (volatile) (match identity)))
       source)))
(helm-migemize-command helm-source-kill-ring)
;; (helm-migemize-command helm-for-files)
(helm-migemize-command hh:menu-command)
(helm-migemize-command helm-resume)
(helm-migemize-command helm-git-files)

;; (set-face-background 'helm-source-header "azure2")
;; (set-face-attribute 'helm-source-header nil :height 1.1 :weight 'normal)
;; (set-face-background 'helm-selection "Beige")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; popwin settings
;;;

(el-get 'sync 'popwin)
(require 'popwin)
(popwin-mode 1)
(setq popwin:special-display-config
      (append
       '(("*Async Shell Command*"		:noselect t)
	 ("^\*bzr-status.*\*"			:regexp t :noselect t)
	 ("^\*xgit-status.*\*"			:regexp t :noselect t)
	 ("*quickrun*"				:noselect t))
       popwin:special-display-config))

(global-set-key (kbd "C-x C-p") popwin:keymap)
(setq auto-async-byte-compile-display-function 'popwin:popup-buffer-tail)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; auto-save settings
;;;

(el-get 'sync 'auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 1.5)
(setq auto-save-buffers-enhanced-save-scratch-buffer-to-file-p t)
(setq auto-save-buffers-enhanced-file-related-with-scratch-buffer
      (concat howm-directory "scratch.txt"))
(auto-save-buffers-enhanced t)
(global-set-key "\C-xas" 'auto-save-buffers-enhanced-toggle-activity)

(el-get 'sync 'scratch-pop)
(global-set-key (kbd "C-c c") 'scratch-pop)
(makunbound 'scratch-ext-minor-mode-map)
(define-minor-mode scratch-ext-minor-mode
  "Minor mode for *scratch* buffer."
  nil ""
  '(("\C-c\C-c" . scratch-pop-kill-ring-save-exit)
    ("\C-c\C-e" . erase-buffer)))

(with-current-buffer (get-buffer-create "*scratch*")
  (erase-buffer)
  (ignore-errors
    (insert-file-contents auto-save-buffers-enhanced-file-related-with-scratch-buffer))
  (org-mode)
  (setq header-line-format "scratch!!")
  (scratch-ext-minor-mode 1))
(defun scratch-pop-kill-ring-save-exit ()
  "Save after close the contents of buffer to killring."
  (interactive)
  (kill-new (buffer-string))
  (erase-buffer)
  (funcall (if (fboundp 'popwin:close-popup-window)
               'popwin:close-popup-window
             'quit-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; UI async settings
;;;

(el-get 'sync 'deferred)
(require 'deferred)
(el-get 'sync 'inertial-scroll)
(require 'inertial-scroll)
(setq inertias-initial-velocity 50)
(setq inertias-friction 120)
(setq inertias-update-time 60)
(setq inertias-rest-coef 0.1)
(global-set-key (kbd "C-v") 'inertias-up)
(global-set-key (kbd "M-v") 'inertias-down)
;; (global-set-key [wheel-up] 'inertias-down-wheel)
;; (global-set-key [wheel-down] 'inertias-up-wheel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Emacs DBI settings
;;;
;;; cpan RPC::EPC::Service DBI DBD::SQLite DBD::Pg DBD::mysql
;;; e.g.) dbi:Pg:dbname=dbname;host=hostname;password=password
;;;

(el-get 'sync 'edbi)
(eval-after-load "edbi"
  '(progn
     (custom-set-variables '(edbi:query-result-fix-header nil)
			   '(edbi:ds-history-list-num 50)
			   '(edbi:query-result-column-max-width nil))))

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

(el-get 'sync 'id-manager)
(autoload 'id-manager "id-manager" nil t)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(setenv "GPG_AGENT_INFO" nil)
(defvar idm-database-file (concat external-directory ".idm-db.gpg"))
(defvar idm-copy-action 'kill-new)
(defvar idm-gen-password-cmd mkpasswd-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; google-translate settings
;;;

(el-get 'sync 'google-translate)
(global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; locate settings
;;;

(defvar locate-home-database (expand-file-name "~/locate.database"))
(defvar locate-update-command
  (expand-file-name (concat user-bin-directory "locate.updatedb.sh")))
(defvar locate-update-command-program-args
  (list "nice" "-n" "19" locate-update-command))

(defvar helm-c-locate-command
  (concat "locate -i %s -d " locate-home-database " %s"))

(defun locate-update-home ()
  "offer to update the locate database in home."
  (interactive)
  (set-process-sentinel
   (apply 'start-process "locate-update-home" "*Messages*"
	  locate-update-command-program-args)
   #'(lambda (proc stat)
       (if (zerop (process-exit-status proc))
	   (message "locate.updatedb...done")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; apachectl settings
;;;
;;;

(defvar apachectl-program-command "/usr/local/sbin/apachectl")
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
       #'(lambda (proc stat)
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
  (let ((buf (get-buffer-create "*convert*")) str ret)
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
  (let ((buf (get-buffer-create "*convert*")) str ret)
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
;;; po-mode.el settings
;;;

(el-get 'sync 'po-mode)
(autoload 'po-mode "po-mode"
  "Major mode for translators to edit PO files" t)
(setq auto-mode-alist (cons '("\\.po\\'\\|\\.po\\." . po-mode)
			    auto-mode-alist))

(autoload 'po-find-file-coding-system "po-compat")
(modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\."
			    'po-find-file-coding-system)
(setq gc-cons-threshold 800000)
(put 'downcase-region 'disabled nil)
