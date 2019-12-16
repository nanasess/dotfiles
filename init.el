; init.el --- Emacs initialization file.

;; Author: Kentaro Ohkouchi  <nanasess@fsm.ne.jp>
;; URL: git://github.com/nanasess/dot.emacs.git

;;; Code:

;; see https://github.com/jschaf/esup/issues/54#issue-317095645
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq gc-cons-threshold (* 128 1024 1024))
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(defvar user-initial-directory (locate-user-emacs-file "init.d/"))
(defvar user-site-lisp-directory (locate-user-emacs-file "site-lisp/"))
(defvar user-misc-directory (locate-user-emacs-file "etc/"))
(defvar user-bin-directory (locate-user-emacs-file "bin/"))
(defvar external-directory (expand-file-name "~/OneDrive - nanasess.net/emacs/"))
(defvar openweathermap-api-key nil)

(setq load-prefer-newer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; el-get settings
;;;

(add-to-list 'load-path (concat user-emacs-directory "el-get/ddskk"))
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get-bundle! el-get-lock
  :type github
  :pkgname "tarao/el-get-lock")
(el-get-lock)

(el-get-bundle with-eval-after-load-feature-el
  :type github
  :features with-eval-after-load-feature
  :pkgname "tarao/with-eval-after-load-feature-el")

;; (el-get-bundle esup)
;; (el-get-bundle! initchart
;;   :type github
;;   :pkgname "yuttie/initchart")
;; (initchart-record-execution-time-of load file)
;; (initchart-record-execution-time-of require feature)
(el-get-bundle awasira/cp5022x.el
  :name cp5022x
  :features cp5022x
  (with-eval-after-load-feature 'cp5022x
    (define-coding-system-alias 'iso-2022-jp 'cp50220)
    (define-coding-system-alias 'euc-jp 'cp51932)))

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
(add-to-list 'load-path (expand-file-name (locate-user-emacs-file "secret.d/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; exec-path settings
;;;

(dolist (dir (list "/sbin" "/usr/sbin" "/bin" "/usr/bin" "/usr/local/bin"
		   "/opt/local/sbin" "/opt/local/bin" "/usr/gnu/bin"
		   (expand-file-name "~/bin")
		   (expand-file-name "~/.emacs.d/bin")
		   (expand-file-name "~/.local/bin")
		   (expand-file-name "~/.composer/vendor/bin")
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

(setq skk-user-directory (concat external-directory "ddskk")
      skk-init-file (concat user-initial-directory "skk-init.el"))
(el-get-bundle ddskk
  (with-eval-after-load-feature 'skk
    ;; see https://uwabami.github.io/cc-env/Emacs.html
    (defun disable-skk-setup-modeline ()
      (setq skk-indicator-alist (skk-make-indicator-alist))
      (force-mode-line-update t))
    (advice-add 'skk-setup-modeline :override 'disable-skk-setup-modeline)))
(setq skk-preload nil)

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
(setq dired-bind-jump nil)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq enable-recursive-minibuffers t)
(setq cua-enable-cua-keys nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; backup files settings
;;;

(add-to-list 'backup-directory-alist (cons "\\.*$" (expand-file-name "~/.bak/")))
(setq delete-old-versions t
      make-backup-files t
      version-control t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; recentf settings
;;;

(el-get-bundle recentf-ext)
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
;;; face settings
;;;

(setq visible-bell t)
;; use solarized.
(el-get-bundle doom-themes)
(load-theme 'doom-solarized-light t)
(set-face-attribute 'font-lock-comment-face nil :slant 'normal)
(set-face-attribute 'font-lock-type-face nil :slant 'normal :weight 'bold)
(set-face-attribute 'font-lock-builtin-face nil :slant 'normal :weight 'bold)

(require 'whitespace)
(setq whitespace-style
      '(face trailing tabs spaces space-mark tab-mark))
(setq whitespace-display-mappings nil)
(setq whitespace-trailing-regexp  "\\([ \u00A0]+\\)$")
(setq whitespace-space-regexp "\\(\u3000+\\)")
(set-face-attribute 'whitespace-trailing nil
		    :foreground nil
		    :background "#FDF6E3"
		    :underline t)
(set-face-attribute 'whitespace-tab nil
		    ;; base4
		    :foreground "#E1DBCD"
		    :background "#E1DBCD"
		    :underline nil)
(set-face-attribute 'whitespace-space nil
		    ;; base5
		    :foreground "#D6D6D6"
		    :background "#D6D6D6"
		    :underline nil)
(global-whitespace-mode t)

;; see also http://rubikitch.com/2015/05/14/global-hl-line-mode-timer/
(global-hl-line-mode 0)
(defun global-hl-line-timer-function ()
  (global-hl-line-unhighlight-all)
  (let ((global-hl-line-mode t))
    (global-hl-line-highlight)))
(setq global-hl-line-timer
      (run-with-idle-timer 0.1 t 'global-hl-line-timer-function))
;; (cancel-timer global-hl-line-timer)

(el-get-bundle shrink-path
  :type github
  :pkgname "zbelial/shrink-path.el"
  :depends (dash f s))
(el-get-bundle all-the-icons)

(load "openweathermap-api-key" t)
(when openweathermap-api-key
  (el-get-bundle! sky-color-clock
      :type github
      :pkgname "zk-phi/sky-color-clock"
      (with-eval-after-load-feature 'sky-color-clock
	(sky-color-clock-initialize 34.8)(setq sky-color-clock-format "")
	(sky-color-clock-initialize-openweathermap-client openweathermap-api-key 1855207))))

(el-get-bundle doom-modeline
  :type github
  :depends (all-the-icons dash eldoc-eval shrink-path)
  :pkgname "seagle0128/doom-modeline"
  (add-hook 'after-init-hook 'doom-modeline-mode)
  (with-eval-after-load-feature 'doom-modeline-core
    (add-hook 'doom-modeline-mode-hook
	      #'(lambda ()
		  (setf (alist-get "\\.php$" all-the-icons-icon-alist nil nil #'equal)
			'(all-the-icons-fileicon "php" :face all-the-icons-lpurple))
		  (setf (alist-get "\\.csx?$" all-the-icons-icon-alist nil nil #'equal)
			'(all-the-icons-alltheicon "csharp-line" :face all-the-icons-dpurple))
		  (setf (alist-get 'php-mode all-the-icons-mode-icon-alist nil nil #'equal)
			'(all-the-icons-fileicon "php" :face all-the-icons-lpurple))
		  (setf (alist-get 'csharp-mode all-the-icons-mode-icon-alist nil nil #'equal)
			'(all-the-icons-alltheicon "csharp-line" :face all-the-icons-dpurple))
		  (doom-modeline-def-modeline 'main
		    '(bar input-method-skk workspace-name window-number modals matches buffer-info remote-host buffer-position parrot selection-info)
		    '(objed-state misc-info persp-name grip github debug lsp minor-modes indent-info buffer-encoding major-mode process vcs checker sky-color-clock))))
    (setq doom-modeline-vcs-max-length 999)
    (setq doom-modeline-buffer-file-name-style 'buffer-name)
    (doom-modeline-def-segment sky-color-clock
      (concat (doom-modeline-spc)
	      (sky-color-clock)))

    (doom-modeline-def-segment input-method-skk
      "The current ddskk status."
      (concat
       (doom-modeline-spc)
       (propertize
        (cond
         ((not (boundp 'skk-modeline-input-mode)) "[--]")
	 (t (if (string= "" skk-modeline-input-mode) "[--]"
	      (substring (format "%s" skk-modeline-input-mode) 2 -1)))))))))

(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

(el-get-bundle symbol-overlay
  :type github
  :pkgname "wolray/symbol-overlay"
  (with-eval-after-load-feature 'symbol-overlay
    (global-set-key (kbd "M-i") 'symbol-overlay-put)
    (global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
    (global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
    (global-set-key (kbd "<f7>") 'symbol-overlay-mode)
    (global-set-key (kbd "<f8>") 'symbol-overlay-remove-all)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; window-system settings
;;;

(cond (window-system (tool-bar-mode 0)))

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

(el-get-bundle emacs-async
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1))

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

(setq-default indent-tabs-mode nil)
(defun basic-indent ()
  (setq tab-width 4)
  (setq indent-tabs-mode nil))

(el-get-bundle editorconfig
  (add-hook 'after-init-hook 'editorconfig-mode))

(el-get-bundle prettier-js)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Misc settings
;;;

(setq indicate-empty-lines t)
(setq isearch-lax-whitespace nil)
(setq mouse-yank-at-point t)
(setq select-enable-clipboard t)
(setq select-enable-primary t)
(setq save-interprogram-paste-before-kill t)
(delete-selection-mode 1)

;; XXX allow remembering risky and safe variables
;; see https://emacs.stackexchange.com/a/44604
(defun risky-local-variable-p (sym &optional _ignored) nil)
(defun safe-local-variable-p (sym val) t)

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
;;; JavaScript-mode settings
;;;

;; (add-hook 'javascript-mode-hook 'basic-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; js2-mode settings
;;;

(el-get-bundle js2-mode
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (with-eval-after-load-feature 'js2-mode
    (electric-indent-local-mode 0)
    (define-key js2-mode-map (kbd "RET") 'js2-line-break)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; tide settings
;;;

(el-get-bundle tide)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'typescript-mode-hook 'prettier-js-mode)

;; format options
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
(add-hook 'js2-mode-hook #'setup-tide-mode)

;; (el-get 'sync 'jade-mode)
;; (require 'sws-mode)
;; (require 'jade-mode)
;; (add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
;; (add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; web-mode settings
;;;

(el-get-bundle web-mode
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(tpl\\)\\'" . web-mode))
  (with-eval-after-load-feature 'web-mode
    ;; (setq web-mode-block-padding 4)
    (setq web-mode-enable-block-face t)
    ;; (setq web-mode-script-padding 4)
    ;; (setq web-mode-style-padding 4)
    ;; (setq web-mode-enable-auto-indentation nil)
    (setq web-mode-enable-current-element-highlight nil)
    (setq web-mode-enable-current-column-highlight nil)
    (add-to-list 'auto-mode-alist '("\\.\\(twig\\|html\\)\\'" . web-mode))
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
    (add-hook 'editorconfig-custom-hooks
	      (lambda (hash) (setq web-mode-block-padding 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; yaml-mode settings
;;;

(el-get-bundle yaml-mode
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SQL settings
;;;

(setq sql-product 'postgres)
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
(with-eval-after-load-feature 'view
     (define-key view-mode-map (kbd "h") 'backward-word)
     (define-key view-mode-map (kbd "l") 'forward-word)
     (define-key view-mode-map (kbd "j") 'next-line)
     (define-key view-mode-map (kbd "k") 'previous-line)
     (define-key view-mode-map " " 'scroll-up)
     (define-key view-mode-map (kbd "b") 'scroll-down))
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

(defun change-frame-height-up ()
  (interactive)
  (set-frame-height (selected-frame) (+ (frame-height (selected-frame)) 1)))
(defun change-frame-height-down ()
  (interactive)
  (set-frame-height (selected-frame) (- (frame-height (selected-frame)) 1)))
(defun change-frame-width-up ()
  (interactive)
  (set-frame-width (selected-frame) (+ (frame-width (selected-frame)) 1)))
(defun change-frame-width-down ()
  (interactive)
  (set-frame-width (selected-frame) (- (frame-width (selected-frame)) 1)))

(global-set-key (kbd "C-z C-a") 'toggle-fullscreen)
(global-set-key (kbd "C-z C-z") 'toggle-size-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; migemo settings
;;;

(defvar migemo-dictionary
  (concat external-directory "migemo/dict/utf-8/migemo-dict"))
(when (file-exists-p migemo-dictionary)
  (setq migemo-command "cmigemo"
	migemo-options '("-q" "--emacs" "-i" "\a")
	migemo-user-dictionary nil
	migemo-regex-dictionary nil
	migemo-use-pattern-alist t
	migemo-use-frequent-pattern-alist t
	migemo-pattern-alist-length 10000
	migemo-coding-system 'utf-8-unix))
(el-get-bundle! migemo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; goto-chg settings
;;;
;;; (auto-install-from-emacswiki "goto-chg.el")
;;;

(el-get-bundle goto-chg)
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

(el-get-bundle visual-regexp)
(define-key global-map (kbd "M-%") 'vr/query-replace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; easy-kill settings
;;;

(el-get-bundle! easy-kill in leoliu/easy-kill)
(global-set-key [remap kill-ring-save] 'easy-kill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; yasnippet settings
;;;

(el-get-bundle yasnippet)
(add-hook 'after-init-hook 'yas-global-mode)
(el-get-bundle yasnippet-snippets)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; org-mode settings
;;;

(setq org-directory (concat external-directory "howm/"))
(setq org-return-follows-link t)
(setq org-startup-folded nil)
(setq org-startup-truncated nil)

(add-hook 'org-mode-hook
          #'(lambda ()
	      ;; yasnippet (using the new org-cycle hooks)
	      (setq ac-use-overriding-local-map t)))

;;; org-html5presentation
;; (el-get 'sync 'org-html5presentation)

;;; org-tree-slide
;; http://pastelwill.jp/wiki/doku.php?id=emacs:org-tree-slide
;; (el-get 'sync 'org-tree-slide)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; session settings
;;;

(el-get-bundle! session
  :type github
  :pkgname "nanasess/emacs-session"
  (add-hook 'after-init-hook 'session-initialize)
  (setq session-save-print-spec '(t nil 40000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; cua-mode settings
;;;

(if (not (version< "24.3.99" emacs-version))
    (cua-mode t)
  (setq cua-enable-cua-keys nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; expand-region settings
;;;

(el-get-bundle expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(el-get-bundle multiple-cursors)
(global-set-key (kbd "<C-M-return>") 'mc/edit-lines)

;; see https://github.com/magnars/expand-region.el/issues/220
(setq shift-select-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; vc-svn settings
;;;

(add-to-list 'process-coding-system-alist '("svn" . utf-8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; psvn settings
;;;

(el-get-bundle dsvn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; smart-jump settings
;;;
(el-get-bundle dumb-jump
  :type github
  :pkgname "jacktasia/dumb-jump")
(el-get-bundle smart-jump
  :type github
  :depends dumb-jump
  :pkgname "jojojames/smart-jump")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; magit settings
;;;

(setenv "EDITOR" "emacsclient")
(defun visit-gh-pull-request (repo) nil)
(defun visit-bb-pull-request (repo) nil)
(defun endless/visit-pull-request-url () nil)
(el-get-bundle emacs-async)
(el-get-bundle transient)
(el-get-bundle with-editor)
(el-get-bundle magit
  (with-eval-after-load-feature 'magit
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
		 "\\`.+github\\.com:\\(.+\\)\\(\\.git\\)?\\'" "\\1"
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
		"\\`.+bitbucket\\.org:\\(.+\\)\\.git\\'" "\\1"
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
    ;; visit PR for github or bitbucket repositories with "v"
    (define-key magit-mode-map "v" #'endless/visit-pull-request-url)
    (define-key magit-log-mode-map (kbd "j") 'magit-section-forward)
    (define-key magit-log-mode-map (kbd "k") 'magit-section-backward)
    (remove-hook 'server-switch-hook 'magit-commit-diff)))
(global-set-key (kbd "C-z m") 'magit-status)

(with-eval-after-load-feature 'smerge-mode
  (define-key smerge-mode-map (kbd "M-n") 'smerge-next)
  (define-key smerge-mode-map (kbd "M-p") 'smerge-prev))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; howm settings
;;;

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
(setq howm-view-use-grep t)
;; see http://blechmusik.hatenablog.jp/entry/2013/07/09/015124
(setq howm-process-coding-system 'utf-8-unix)
(setq howm-todo-menu-types "[-+~!]")
(el-get-bundle! howm
  :type git
  :url "git://git.osdn.jp/gitroot/howm/howm.git"
  :build `(("./configure" ,(concat "--with-emacs=" el-get-emacs)) ("make")))
(add-to-list 'auto-mode-alist '("\\.txt$" . gfm-mode))
(setq howm-template
      (concat howm-view-title-header
	      (concat
	       " %title%cursor\n"
	       "Date: %date\n\n"
	       "%file\n\n")
	      (concat
	       "<!--\n"
	       "  Local Variables:\n"
	       "  mode: gfm\n"
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
(defun quickrun/phpunit-outputter ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "" nil t)
      (replace-match "" nil nil)))
  (highlight-phrase "OK.*$" 'phpunit-pass)
  (highlight-phrase "ERRORS.*$" 'phpunit-fail))
(el-get-bundle quickrun
  (with-eval-after-load-feature 'quickrun
    (add-to-list 'quickrun-file-alist '("Test\\.php\\'" . "phpunit"))
    (quickrun-add-command "phpunit" '((:command . "phpunit")
				      (:exec . ("%c -c ~/git-repos/ec-cube/phpunit.xml.dist %s"))
				      (:outputter . quickrun/phpunit-outputter)))
    (defface phpunit-pass
      '((t (:foreground "white" :background "ForestGreen" :weight bold))) nil
      :group 'font-lock-highlighting-faces)
    (defface phpunit-fail
      '((t (:foreground "white" :background "red" :weight bold))) nil
      :group 'font-lock-highlighting-faces)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; company-mode settings
;;;

(defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
(el-get-bundle company-mode
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "C-M-i") 'company-complete)
  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
	backend
      (append (if (consp backend) backend (list backend))
	      '(:with company-yasnippet))))
  (with-eval-after-load-feature 'company
    (setq company-idle-delay 0.1
          company-minimum-prefix-length 2
          company-tooltip-align-annotations t
          company-selection-wrap-around t)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-search-map (kbd "C-n") 'company-select-next)
    (define-key company-search-map (kbd "C-p") 'company-select-previous)

    ;; C-sで絞り込む
    (define-key company-active-map (kbd "C-s") 'company-filter-candidates)

    ;; TABで候補を設定
    (define-key company-active-map (kbd "C-i") 'company-complete-selection)
    (defun company-backends-with-yas ()
      (interactive)
      (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))))

  (with-eval-after-load-feature 'company-dabbrev
    (setq company-dabbrev-downcase nil)))

(add-hook 'emacs-lisp-mode-hook
	  #'(lambda ()
	      (make-local-variable 'company-backends)
	      (push '(company-elisp :with company-yasnippet) company-backends)))

(el-get-bundle company-box
  :type github
  :pkgname "sebastiencs/company-box"
  (add-hook 'company-mode-hook 'company-box-mode)
  (with-eval-after-load-feature 'company-box
    (defun company-box--update-width (&optional no-update height)
      (unless no-update
	(redisplay))
      (-let* ((frame (company-box--get-frame))
	      (window (frame-parameter nil 'company-box-window))
	      (start (window-start window))
	      (char-width (frame-char-width frame))
	      (end (or (and height (with-current-buffer (window-buffer window)
				     (save-excursion
				       (goto-char start)
				       (forward-line height)
				       (point))))
		       (window-end window)))
	      ;; (max-width (- (frame-pixel-width) company-box--x char-width))
	      (max-width (- (x-display-pixel-width)
			    (max (eval (frame-parameter nil 'left)) 0)
			    company-box--x char-width))
	      (width (+ (company-box--calc-len (window-buffer window) start end char-width)
			(if (company-box--scrollbar-p frame) (* 2 char-width) 0)
			char-width))
	      (width (max (min width max-width)
			  (* company-tooltip-minimum-width char-width)))
	      (diff (abs (- (frame-pixel-width frame) width))))
    (or (and no-update width)
        (and (> diff 2) (set-frame-width frame width nil t)))))

    (setq company-box-enable-icon t)
    (setq company-box-show-single-candidate t)
    ;; (setq company-box-max-candidates 50)
    (setq company-box-doc-delay 0.5)
    (setq company-box-icons-alist 'company-box-icons-all-the-icons)
    ;; see https://github.com/zenith-john/zenith-emacs/blob/8d85e5e5d9e477873762452063683609ae2dc91e/config/init-company.el
    (defconst company-box-icons--phpactor-alist
      '(("interface" . Interface)
	("class" . Class)
	("method" . Method)
	("function" . Function)
	("property" . Property)
	("constant" . Constant)
	("variable" . Variable)
	("interface" . Interface)
	("module" . Module)
	("template" . Template)))
    (defun company-box-icons--phpactor (candidate)
      ;; (message "omnisharp-item: %s" (get-text-property 0 'omnisharp-item candidate))
      (when (derived-mode-p 'php-mode)
	(let ((key (get-text-property 0 'type candidate)))
	  (cdr (assoc key company-box-icons--phpactor-alist)))))
    (defun company-box-icons--yasnippet+ (candidate)
      (message "%s" (get-text-property 0 'yas-annotation candidate))
      (when (get-text-property 0 'yas-annotation candidate)
	'Yasnippet))

    (setq company-box-icons-functions
	  '(company-box-icons--yasnippet+ company-box-icons--lsp company-box-icons--elisp company-box-icons--phpactor))

    (setq company-box-icons-all-the-icons
	  `((Unknown       . ,(all-the-icons-material "find_in_page"             :height 0.8 :face 'all-the-icons-blue-alt))
	    (Text          . ,(all-the-icons-material "text_fields"              :height 0.8 :face 'all-the-icons-green))
	    (Method        . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-purple))
	    (Function      . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-purple))
	    (Constructor   . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-purple))
	    (Field         . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-purple))
	    (Variable      . ,(all-the-icons-material "adjust"                   :height 0.8 :face 'all-the-icons-blue))
	    (Class         . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-purple))
	    (Interface     . ,(all-the-icons-material "settings_input_component" :height 0.8 :face 'all-the-icons-purple))
	    (Module        . ,(all-the-icons-material "view_module"              :height 0.8 :face 'all-the-icons-purple))
	    (Property      . ,(all-the-icons-material "settings_applications"    :height 0.8 :face 'all-the-icons-purple))
	    (Unit          . ,(all-the-icons-material "straighten"               :height 0.8 :face 'all-the-icons-purple))
	    (Value         . ,(all-the-icons-material "filter_1"                 :height 0.8 :face 'all-the-icons-purple))
	    (Enum          . ,(all-the-icons-material "plus_one"                 :height 0.8 :face 'all-the-icons-purple))
	    (Keyword       . ,(all-the-icons-material "filter_center_focus"      :height 0.8 :face 'all-the-icons-purple))
	    (Snippet       . ,(all-the-icons-material "short_text"               :height 0.8 :face 'all-the-icons-purple))
	    (Color         . ,(all-the-icons-material "color_lens"               :height 0.8 :face 'all-the-icons-purple))
	    (File          . ,(all-the-icons-material "insert_drive_file"        :height 0.8 :face 'all-the-icons-purple))
	    (Reference     . ,(all-the-icons-material "collections_bookmark"     :height 0.8 :face 'all-the-icons-purple))
	    (Folder        . ,(all-the-icons-material "folder"                   :height 0.8 :face 'all-the-icons-purple))
	    (EnumMember    . ,(all-the-icons-material "people"                   :height 0.8 :face 'all-the-icons-purple))
	    (Constant      . ,(all-the-icons-material "pause_circle_filled"      :height 0.8 :face 'all-the-icons-purple))
	    (Struct        . ,(all-the-icons-material "streetview"               :height 0.8 :face 'all-the-icons-purple))
	    (Event         . ,(all-the-icons-material "event"                    :height 0.8 :face 'all-the-icons-purple))
	    (Operator      . ,(all-the-icons-material "control_point"            :height 0.8 :face 'all-the-icons-purple))
	    (TypeParameter . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-purple))
	    ;; (Template   . ,(company-box-icons-image "Template.png"))))
	    (Yasnippet     . ,(all-the-icons-material "share"               :height 0.8 :face 'all-the-icons-green))
	    (ElispFunction . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-purple))
	    (ElispVariable . ,(all-the-icons-material "check_circle"             :height 0.8 :face 'all-the-icons-blue))
	    (ElispFeature  . ,(all-the-icons-material "stars"                    :height 0.8 :face 'all-the-icons-orange))
	    (ElispFace     . ,(all-the-icons-material "format_paint"             :height 0.8 :face 'all-the-icons-pink))))
    (setq company-box-backends-colors
	  '((company-yasnippet . (:selected (:background "#DEB542" :weight bold)))
	    (company-dabbrev . (:selected (:background "PaleTurquoise" :weight bold)))))
    (defface company-box-scrollbar
      '((t (:background "#073642" :weight bold))) nil :group 'company-box)))

(el-get-bundle git-complete
  :type github
  :pkgname "zk-phi/git-complete"
  :depends popup
  (with-eval-after-load-feature 'git-complete
    (setq git-complete-enable-autopair t)
    (global-set-key (kbd "C-z /") 'git-complete)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; markdown-mode settings
;;;

(el-get-bundle markdown-mode
  (add-to-list 'auto-mode-alist '("\\.\\(markdown\\|md\\)\\'" . gfm-mode)))

;; see also http://stackoverflow.com/questions/14275122/editing-markdown-pipe-tables-in-emacs
(defun cleanup-org-tables ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "-+-" nil t) (replace-match "-|-"))))

(add-hook 'markdown-mode-hook 'turn-on-orgtbl)
(add-hook 'markdown-mode-hook
          #'(lambda()
	      (add-hook 'after-save-hook 'cleanup-org-tables  nil 'make-it-local)))
(add-hook 'gfm-mode-hook 'turn-on-orgtbl)
(add-hook 'gfm-mode-hook
          #'(lambda()
	      (add-hook 'after-save-hook 'cleanup-org-tables  nil 'make-it-local)))
(with-eval-after-load-feature 'org-table
  (add-hook 'markdown-mode-hook
            #'(lambda()
		(define-key orgtbl-mode-map
		  (kbd "<backspace>") 'delete-backward-char)))
  (add-hook 'gfm-mode-hook
            #'(lambda()
		(define-key orgtbl-mode-map
		  (kbd "<backspace>") 'delete-backward-char))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Language-server settings
;;;
(el-get-bundle request)
(el-get-bundle spinner)
(el-get-bundle f)
(el-get-bundle ht)
(el-get-bundle lsp-mode
  :type github
  :pkgname "emacs-lsp/lsp-mode"
  :depends (spinner f ht)
  (with-eval-after-load-feature 'lsp-mode
    ;; https://qiita.com/Ladicle/items/feb5f9dce9adf89652cf#lsp
    (setq lsp-print-io nil)
    (setq lsp-print-performance nil)
    ;; general
    (setq lsp-auto-guess-root t)
    (setq lsp-document-sync-method 'incremental) ;; always send incremental document
    (setq lsp-response-timeout 5)
    (setq lsp-prefer-flymake 'flymake)
    (setq lsp-enable-completion-at-point nil)
    (require 'lsp-clients)))

(el-get-bundle lsp-ui
  :type github
  :pkgname "emacs-lsp/lsp-ui"
  (with-eval-after-load-feature 'lsp-ui
    ;; lsp-ui-doc
    (setq lsp-ui-doc-enable t)
    (setq lsp-ui-doc-header t)
    (setq lsp-ui-doc-include-signature t)
    (setq lsp-ui-doc-position 'top) ;; top, bottom, or at-point
    (setq lsp-ui-doc-max-width 150)
    (setq lsp-ui-doc-max-height 30)
    (setq lsp-ui-doc-use-childframe t)
    (setq lsp-ui-doc-use-webkit t)
    ;; lsp-ui-flycheck
    (setq lsp-ui-flycheck-enable nil)
    ;; lsp-ui-sideline
    (setq lsp-ui-sideline-enable nil)
    (setq lsp-ui-sideline-ignore-duplicate t)
    (setq lsp-ui-sideline-show-symbol t)
    (setq lsp-ui-sideline-show-hover t)
    (setq lsp-ui-sideline-show-diagnostics nil)
    (setq lsp-ui-sideline-show-code-actions nil)
    ;; lsp-ui-imenu
    (setq lsp-ui-imenu-enable nil)
    (setq lsp-ui-imenu-kind-position 'top)
    ;; lsp-ui-peek
    (setq lsp-ui-peek-enable t)
    (setq lsp-ui-peek-peek-height 20)
    (setq lsp-ui-peek-list-width 50)
    (setq lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
    (defun ladicle/toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
	(lsp-ui-doc-mode 1))))
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))
(el-get-bundle company-lsp
  :type github
  :pkgname "tigersoldier/company-lsp"
  (with-eval-after-load-feature 'company-lsp
    (setq company-lsp-cache-candidates t) ;; always using cache
    (setq company-lsp-async t)
    (setq company-lsp-enable-recompletion nil)))

(el-get-bundle eldoc-box
  :type github
  :pkgname "casouri/eldoc-box"
  (defface eldoc-box-border '((t (:background "#E1DBCD"))) nil ; base4
    :group 'font-lock-highlighting-faces)
  (defface eldoc-box-body '((t . (:background "#FFFBEA"))) nil ; bg-alt
    :group 'font-lock-highlighting-faces))
(setq eldoc-box-clear-with-C-g t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PHP settings
;;;

(el-get-bundle php-mode
  :type github
  :pkgname "emacs-php/php-mode"
  :build `(("make" ,(format "EMACS=%s" el-get-emacs)))
	   ;; (,el-get-emacs "-batch" "-q" "-no-site-file" "-l")
	   ;; (,el-get-emacs "-q" "-l" init.el --batch -f batch-byte-compile init.e)
  :autoloads "php-mode-autoloads"
  (with-eval-after-load-feature 'php-mode
    (add-to-list 'auto-mode-alist '("\\.\\(inc\\|php[s34]?\\)$" . php-mode))
    (add-hook 'php-mode-hook 'php-c-style))
    ;; (add-hook 'php-mode-hook #'lsp))
  (with-eval-after-load-feature 'php
    (setq php-manual-url "http://jp2.php.net/manual/ja/"
	  php-mode-coding-style 'Symfony2
	  php-search-url "http://jp2.php.net/")))

(el-get-bundle php-runtime
  :type github
  :pkgname "emacs-php/php-runtime.el")

(el-get-bundle php-skeleton
  :type github
  :pkgname "emacs-php/php-skeleton")

(el-get-bundle composer
  :type github
  :pkgname "emacs-php/composer.el"
  :depends request)

(setq phpactor-install-directory (concat user-emacs-directory "el-get/phpactor"))
(setq phpactor--debug nil)
(setq company-phpactor-request-async t)
(el-get-bundle phpactor
  :type github
  :pkgname "emacs-php/phpactor.el"
  :branch "master"
  :depends (f composer company-mode smart-jump)
  :autoloads "company-phpactor"
  (with-eval-after-load-feature 'phpactor))
;; (setq lsp-clients-phpactor-server-command "phpactor server:start --stdio")
;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-stdio-connection
;;                                    (lambda () lsp-clients-php-server-command))
;;                   :major-modes '(php-mode)
;;                   :priority -2
;;                   :server-id 'php-ls))

(el-get-bundle phpstan
  :type github
  :pkgname "emacs-php/phpstan.el")

(defun php-c-style ()
  (interactive)
  (require 'php-skeleton)
  (require 'php-skeleton-exceptions)
  (require 'flycheck-phpstan)
  (make-local-variable 'company-backends)
  (push '(company-phpactor :with company-yasnippet) company-backends)
  (make-local-variable 'eldoc-documentation-function)
  (setq eldoc-documentation-function 'phpactor-hover)
  (eldoc-box-hover-mode 1)
  ;; (eldoc-box-hover-at-point-mode 1)
  (c-toggle-auto-newline 1)
  (c-toggle-auto-hungry-state 1)
  (electric-indent-local-mode t)
  (electric-layout-mode t)
  ;; (setq-local electric-layout-rules '((?{ . around)))
  (electric-pair-local-mode t)
  (flycheck-mode t)
  (phpactor-smart-jump-register)
  ;; If you feel phumped and phpcs annoying, invalidate them.
  (when (boundp 'flycheck-disabled-checkers)
    (add-to-list 'flycheck-disabled-checkers 'php-phpmd)
    (add-to-list 'flycheck-disabled-checkers 'php-phpcs))
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-start-skip) "// *")
  (set (make-local-variable 'comment-end) ""))

(el-get-bundle phpunit
  :type github
  :pkgname "nlamirault/phpunit.el"
  (add-to-list 'auto-mode-alist '("\\Test.php$'" . phpunit-mode))
  (with-eval-after-load-feature 'phpunit
    (define-key php-mode-map (kbd "C-z C-t") 'phpunit-current-class))
    (setq phpunit-configuration-file  "phpunit.xml.dist")
    (setq phpunit-default-program  "phpunit"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Java settings
;;;
(defun java-c-style ()
  (require 'lsp-java)
  (define-key java-mode-map [return] 'newline-and-indent))
(el-get-bundle lsp-java
  :type github
  :pkgname "emacs-lsp/lsp-java"
  (with-eval-after-load-feature 'lsp-java
    (add-hook 'java-mode-hook #'company-backends-with-yas)
    (add-hook 'java-mode-hook #'lsp)))
(el-get-bundle emacswiki:tree-mode)
(el-get-bundle bui
  :type github
  :pkgname "alezost/bui.el")
(el-get-bundle dap-mode
  :type github
  :pkgname "yyoncho/dap-mode"
  :depends (tree-mode bui)
  (dap-mode 1)
  (dap-ui-mode 1))

(add-hook 'java-mode-hook 'java-c-style)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; csv-mode settings
;;;

(el-get-bundle csv-mode in emacsmirror/csv-mode)
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; csharp
;;;

;; XXX omnisharp-utils.el で (require 'shut-up) しないと動かないかも
(el-get-bundle shut-up in cask/shut-up)
(el-get-bundle omnisharp-emacs
  :type github
  :pkgname "OmniSharp/omnisharp-emacs"
  :depends (csharp-mode popup shut-up dash s f))
;; (el-get-bundle company)
;; (el-get-bundle ac-company)
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-omnisharp))
;;
;; git clone git@github.com:OmniSharp/omnisharp-roslyn.git
;; cd omnisharp-roslyn
;; ./build.sh
;;
;; XXX OmniSharp-Roslyn が自動起動してくれないので 以下のようにして手動で起動させる
;; ~/git-repos/omnisharp-roslyn/artifacts/publish/OmniSharp/default/netcoreapp1.1/OmniSharp -s `pwd`
;;
;; さらに csharp-mode や omnisharp-mode がちゃんと起動しない場合は以下のように手動で起動させる
;; M-x my-csharp-mode-hook
;; M-x my-omnisharp-mode-hook
(setq omnisharp-server-executable-path "~/bin/omnisharp-osx/run")
;; (require 'ac-company)
;; (ac-company-define-source ac-source-company-omnisharp company-omnisharp)

;; (setq omnisharp-debug 1)

(with-eval-after-load-feature 'csharp-mode
  (defun my-csharp-mode-hook ()
    (interactive)
    (flycheck-mode 1)
    ;; (auto-complete-mode 1)
    (electric-pair-local-mode 1) ;; for Emacs25
    (setq c-syntactic-indentation t)
    (c-set-style "ellemtel")
    (setq truncate-lines t)
    (setq flycheck-idle-change-delay 2)
    (omnisharp-mode 1))
  (add-hook 'csharp-mode-hook 'my-csharp-mode-hook)
  (add-hook 'csharp-mode-hook
	    #'(lambda ()
		;; (add-to-list 'ac-sources 'ac-source-omnisharp)
		(add-to-list 'flycheck-checkers 'csharp-omnisharp-codecheck))))
(with-eval-after-load-feature 'omnisharp
  (defun my-omnisharp-mode-hook ()
    (interactive)
    (message "omnisharp-mode enabled")
    ;; (define-key omnisharp-mode-map "\C-c\C-s" 'omnisharp-start-omnisharp-server)
    ;; (define-key company-active-map (kbd ".") (lambda() (interactive) (company-complete-selection-insert-key-and-complete '".")))
    ;; (define-key company-active-map (kbd "]") (lambda() (interactive) (company-complete-selection-insert-key-and-complete '"]")))
    ;; (define-key company-active-map (kbd "[") (lambda() (interactive) (company-complete-selection-insert-key '"[")))
    ;; (define-key company-active-map (kbd ")") (lambda() (interactive) (company-complete-selection-insert-key '")")))
    ;; (define-key company-active-map (kbd "<SPC>") nil)
    ;; (define-key company-active-map (kbd ";") (lambda() (interactive) (company-complete-selection-insert-key '";")))
    ;; (define-key company-active-map (kbd ">") (lambda() (interactive) (company-complete-selection-insert-key '">")))
    (define-key omnisharp-mode-map (kbd "}") 'csharp-indent-function-on-closing-brace) 
    (define-key omnisharp-mode-map "\M-/"     'omnisharp-auto-complete)
    (define-key omnisharp-mode-map "."        'omnisharp-add-dot-and-auto-complete)
    (define-key omnisharp-mode-map "\C-c\C-c" 'omnisharp-code-format)
    (define-key omnisharp-mode-map "\C-c\C-N" 'omnisharp-navigate-to-solution-member)
    (define-key omnisharp-mode-map "\C-c\C-n" 'omnisharp-navigate-to-current-file-member)
    (define-key omnisharp-mode-map "\C-c\C-f" 'omnisharp-navigate-to-solution-file)
    (define-key omnisharp-mode-map "\M-."     'omnisharp-go-to-definition)
    (define-key omnisharp-mode-map "\C-c\C-r" 'omnisharp-rename)
    (define-key omnisharp-mode-map "\C-c\C-v" 'omnisharp-run-code-action-refactoring)
    (define-key omnisharp-mode-map "\C-c\C-o" 'omnisharp-auto-complete-overrides)
    (define-key omnisharp-mode-map "\C-c\C-u" 'omnisharp-fix-usings)
    ;; (define-key omnisharp-mode-map (kbd "<RET>") 'csharp-newline-and-indent)

    ;; (make-local-variable 'company-backends)
    ;; (push '(company-omnisharp :with company-yasnippet) company-backends)
    (add-to-list 'company-backends '(company-omnisharp :with company-yasnippet))
    (eldoc-box-hover-mode 1)
    ;; (eldoc-box-hover-at-point-mode 1)
    ;; (define-key omnisharp-mode-map "\C-c\C-t\C-s" (lambda() (interactive) (omnisharp-unit-test "single")))
    ;; (define-key omnisharp-mode-map "\C-c\C-t\C-r" (lambda() (interactive) (omnisharp-unit-test "fixture")))
    ;; (define-key omnisharp-mode-map "\C-c\C-t\C-e" (lambda() (interactive) (omnisharp-unit-test "all")))
    )
  (setq omnisharp-company-strip-trailing-brackets nil)
  (add-hook 'omnisharp-mode-hook 'my-omnisharp-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; haskell-ide-engine settings
;;;
;;; git clone git@github.com:haskell/haskell-ide-engine.git ~/.emacs.d/haskell-ide-engine
;;;

(el-get-bundle haskell-mode
  :type github
  :pkgname "haskell/haskell-mode"
  ;; :info "."
  ;; :build `(("make" ,(format "EMACS=%s" el-get-emacs) "all"))
  (with-eval-after-load-feature 'haskell-mode
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    (add-hook 'haskell-mode-hook #'company-backends-with-yas)
    (add-hook 'haskell-mode-hook #'lsp)))

(el-get-bundle lsp-haskell
  :type github
  :pkgname "emacs-lsp/lsp-haskell")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dockerfile settings
;;;
;;; npm i -g dockerfile-language-server-nodejs
;;;
(el-get-bundle dockerfile-mode)
;; (el-get-bundle lsp-dockerfile
;;   :type github
;;   :pkgname "emacs-lsp/lsp-dockerfile"
;;   :features lsp-dockerfile
;;   (with-eval-after-load-feature 'dockerfile-mode
;;     (add-hook 'dockerfile-mode-hook #'company-backends-with-yas)
;;     (add-hook 'dockerfile-mode-hook #'lsp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CSS settings
;;;

(with-eval-after-load-feature 'css-mode
  (add-hook 'css-mode-hook #'company-backends-with-yas)
  (add-hook 'css-mode-hook #'lsp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; shel settings
;;;

(with-eval-after-load-feature 'sh-script
  (add-hook 'sh-mode-hook #'company-backends-with-yas)
  (add-hook 'sh-mode-hook #'lsp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; yafolding settings
;;;

;; (el-get-bundle yafolding)
;; (add-hook 'prog-mode-hook 'yafolding-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew settings
;;;

(setq mew-rc-file "~/.mew.d/.mew.el")
(setq mew-thread-indent-strings [" +" " +" " |" "  "])
(el-get-bundle mew)
;; mm-version
(require 'mm-version)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; twitting-mode settings
;;;

(el-get-bundle twittering-mode
  (with-eval-after-load-feature 'twittering-mode
    (setq twittering-status-format "%RT{%FACE[bold]{RT}}%i %s,  %@: %FACE[error]{%FIELD-IF-NONZERO[❤ %d]{favorite_count}}  %FACE[warning]{%FIELD-IF-NONZERO[↺ %d]{retweet_count}}\n%FOLD[  ]{%T // from %f%L%r%R%QT{\n+----\n%FOLD[|]{%i %s,  %@:\n%FOLD[  ]{%T // from %f%L%r%R}}\n+----}}\n ")
    (define-key twittering-mode-map
      (kbd "s") 'twittering-current-timeline)
    (define-key twittering-mode-map
      (kbd "w") 'twittering-update-status-interactive)
    (define-key twittering-edit-mode-map
      (kbd "C-c C-q") 'twittering-edit-cancel-status)
    (define-key twittering-edit-mode-map
      (kbd "C-u C-u") 'twittering-edit-replace-at-point)))

(unless (load "twittering-tinyurl-api-key" t t)
  (defvar twittering-bitly-api-key nil))

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

;; (el-get-bundle auto-async-byte-compile)
;; (setq auto-async-byte-compile-init-file (concat user-emacs-directory "init.el"))
;; (setq auto-async-byte-compile-exclude-files-regexp "/mac/") ;dummy
;; (setq auto-async-byte-compile-suppress-warnings t)
;; (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; helm settings
;;;

(el-get-bundle helm
  (with-eval-after-load-feature 'helm
    (helm-migemo-mode 1)
    (define-key helm-map (kbd "C-v") 'helm-next-source)
    (define-key helm-map (kbd "M-v") 'helm-previous-source)

    (defun helm-mac-spotlight ()
      "Preconfigured `helm' for `mdfind'."
      (interactive)
      (let ((helm-ff-transformer-show-only-basename nil))
	(helm-other-buffer 'helm-source-mac-spotlight "*helm mdfind*")))))

(defconst helm-for-files-preferred-list
  '(helm-source-buffers-list
    helm-source-recentf
    helm-source-file-cache
    helm-source-files-in-current-dir
    helm-source-mac-spotlight))
(setq helm-buffer-max-length 40
      helm-c-ack-thing-at-point 'symbol
      helm-ff-auto-update-initial-value nil
      ;; helm-grep-default-recurse-command "ggrep -a -d recurse %e -n%cH -e %p %f"
      helm-input-idle-delay 0.2
      helm-mode t
      helm-truncate-lines t)

(with-eval-after-load-feature 'helm-grep
  ;; use ripgrep https://github.com/BurntSushi/ripgrep
  (when (executable-find "rg")
    (setq helm-grep-ag-command "rg --color=always -S --no-heading --line-number %s %s %s")))

(el-get-bundle helm-ls-git)
(el-get-bundle helm-descbinds)

(defadvice helm-grep-highlight-match
    (around ad-helm-grep-highlight-match activate)
  (ad-set-arg 1 t)
  ad-do-it)

(setq helm-gtags-ignore-case t)
(setq helm-gtags-path-style 'relative)
;; grep for euc-jp.
;; (setq helm-grep-default-command "grep -a -d skip %e -n%cH -e `echo %p | lv -Ia -Oej` %f | lv -Os -Ia ")
;; (setq helm-grep-default-recurse-command "grep -a -d recurse %e -n%cH -e `echo %p | lv -Ia -Oej` %f | lv -Os -Ia ")

(el-get-bundle wgrep
  (with-eval-after-load-feature 'wgrep
  (setq wgrep-enable-key "r")))

(add-hook 'helm-gtags-mode-hook
	  #'(lambda ()
	      (local-set-key (kbd "M-.") 'helm-gtags-find-tag)))

(global-set-key (kbd "C-;") 'helm-for-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-z C-r") 'helm-resume)
(global-set-key (kbd "C-z C-f") 'helm-mac-spotlight)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-h b") 'helm-descbinds)
(global-set-key (kbd "C-z l") 'helm-ls-git-ls)


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

(with-eval-after-load-feature 'howm
  (require 'helm-howm)
  (defvar hh:howm-data-directory howm-directory)
  (setq hh:menu-list nil)
  (setq hh:recent-menu-number-limit 100)

  (defun helm-howm-do-ag ()
    (interactive)
    (helm-grep-ag-1
     hh:howm-data-directory))
  ;; use for grep
  (defun helm-howm-do-grep ()
    (interactive)
    (helm-do-grep-1
     (list (car (split-string hh:howm-data-directory "\n"))) '(4) nil '("*.txt" "*.md")))

  (when (executable-find "rg")
    (setq howm-view-use-grep t)
    (setq howm-view-grep-command "rg")
    (setq howm-view-grep-option "-nH --no-heading --color never")
    (setq howm-view-grep-extended-option nil)
    (setq howm-view-grep-fixed-option "-F")
    (setq howm-view-grep-expr-option nil)
    (setq howm-view-grep-file-stdin-option nil))

  (global-set-key (kbd "C-z ,") 'hh:menu-command)
  (global-set-key (kbd "C-z .") 'hh:resume)
  (global-set-key (kbd "C-z s") 'helm-howm-do-grep)
  (global-set-key (kbd "C-z x") 'helm-howm-do-ag))

(el-get-bundle helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c y") 'helm-yas-complete)

(el-get-bundle helm-swoop)
(cl-defun helm-swoop-nomigemo (&key $query ($multiline current-prefix-arg))
  (interactive)
  (let (helm-migemo-mode)
    (helm-swoop :$query $query :$multiline $multiline)))

(defun isearch-forward-or-helm-swoop-or-helm-occur (use-helm-swoop)
  (interactive "p")
  (let (current-prefix-arg
        (helm-swoop-pre-input-function 'ignore))
    (call-interactively
     (case use-helm-swoop
       (1 'isearch-forward)		; C-s
       (4 (if (< 1000000 (buffer-size)) 'helm-occur 'helm-swoop)) ; C-u C-s
       (16 'helm-swoop-nomigemo)))))				  ; C-u C-u C-s
(global-set-key (kbd "C-s") 'isearch-forward-or-helm-swoop-or-helm-occur)

;; (el-get-bundle helm-lsp
;;   :type github
;;   :pkgname "emacs-lsp/helm-lsp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; popwin settings
;;;

(el-get-bundle popwin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; auto-save settings
;;;

(el-get-bundle auto-save-buffers-enhanced
  :type github
  :pkgname "kentaro/auto-save-buffers-enhanced")
(setq auto-save-buffers-enhanced-interval 1.5)
(setq auto-save-buffers-enhanced-save-scratch-buffer-to-file-p t)
(setq auto-save-buffers-enhanced-file-related-with-scratch-buffer
      (concat howm-directory "scratch.txt"))
(auto-save-buffers-enhanced t)
(global-set-key "\C-xas" 'auto-save-buffers-enhanced-toggle-activity)

(el-get-bundle! scratch-pop in zk-phi/scratch-pop)
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

(el-get-bundle! deferred)
(el-get-bundle! inertial-scroll in kiwanami/emacs-inertial-scroll
  (setq inertias-initial-velocity 50)
  (setq inertias-friction 120)
  (setq inertias-update-time 60)
  (setq inertias-rest-coef 0.1)
  (global-set-key (kbd "C-v") 'inertias-up)
  (global-set-key (kbd "M-v") 'inertias-down))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Emacs DBI settings
;;;
;;; cpan RPC::EPC::Service DBI DBD::SQLite DBD::Pg DBD::mysql
;;; e.g.) dbi:Pg:dbname=dbname;host=hostname;password=password
;;;

;; (el-get-bundle edbi
;;   (with-eval-after-load-feature 'edbi
;;     (setq edbi:query-result-fix-header nil
;; 	  edbi:ds-history-list-num 50
;; 	  edbi:query-result-column-max-width nil)))

;;; sqlite-dump
;;; original code was http://download.tuxfamily.org/user42/sqlite-dump.el
(el-get-bundle sqlite-dump
  :type github
  :pkgname "nanasess/sqlite-dump")
(modify-coding-system-alist 'file "\\.\\(db\\|sqlite\\)\\'" 'raw-text-unix)
(add-to-list 'auto-mode-alist '("\\.\\(db\\|sqlite\\)\\'" . sqlite-dump))

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

;; use gpg2 http://blog.n-z.jp/blog/2016-08-20-mac-easypg-gpg2.html
(setq idm-database-file (concat external-directory ".idm-db.gpg"))
(setq idm-copy-action 'kill-new)
(setq idm-gen-password-cmd mkpasswd-command)
(el-get-bundle emacs-id-manager
  :type github
  :autoloads "id-manager"
  :pkgname "kiwanami/emacs-id-manager")
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(setenv "GPG_AGENT_INFO" nil)

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
;;; nginx-mode settings
;;;
(el-get-bundle nginx-mode)

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

(el-get-bundle po-mode)
(setq auto-mode-alist (cons '("\\.po\\'\\|\\.po\\." . po-mode)
			    auto-mode-alist))

(autoload 'po-find-file-coding-system "po-compat")
(modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\."
			    'po-find-file-coding-system)

(define-key minibuffer-local-map (kbd "C-c C-j") 'skk-kakutei)
(define-key minibuffer-local-map (kbd "C-z j") 'skk-isearch-skk-kakutei)
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(global-set-key "\C-co" 'switch-to-minibuffer) ;; Bind to `C-c o'

(setq gc-cons-threshold 800000)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
