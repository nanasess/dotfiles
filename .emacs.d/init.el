;;; init.el --- Emacs initialization file.

;; Author: Kentaro Ohkouchi  <nanasess@fsm.ne.jp>
;; URL: git://github.com/nanasess/dot.emacs.git

;;; Code:
;; (require 'profiler)
;; (profiler-start 'cpu)
;; see https://github.com/syl20bnr/spacemacs/commit/72c89df995ee1e4eb32ab982deb0911093048f20
;; (setq garbage-collection-messages t)
(eval-when-compile (require 'cl))
(eval '(eval-when-compile (require 'cl)))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(defvar user-initial-directory (locate-user-emacs-file "init.d/"))
(defvar user-site-lisp-directory (locate-user-emacs-file "site-lisp/"))
(defvar user-misc-directory (locate-user-emacs-file "etc/"))
(defvar user-bin-directory (locate-user-emacs-file "bin/"))
(defvar external-directory (expand-file-name "~/OneDrive - Skirnir Inc/emacs/"))
(defvar openweathermap-api-key nil)

(setq el-get-bundle-sync t
      el-get-is-lazy nil
      el-get-verbose nil
      el-get-bundle-byte-compile t
      el-get-auto-update-cached-recipes nil
      el-get-user-package-directory (locate-user-emacs-file "el-get-init.d"))

(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get-bundle el-get-lock
  :type github
  :pkgname "tarao/el-get-lock")
(el-get-lock)
(el-get-lock-unlock 'el-get 'seq)

;; (el-get-bundle with-eval-after-load-feature-el
;;   :type github
;;   :pkgname "tarao/with-eval-after-load-feature-el")

;; (el-get-bundle esup)
;; (el-get-bundle! initchart
;;   :type github
;;   :pkgname "yuttie/initchart")
;; (initchart-record-execution-time-of load file)
;; (initchart-record-execution-time-of require feature)
(el-get-bundle compat
  :type github
  :pkgname "phikal/compat.el"
  :build `(("make" ,(format "EMACS=%s" el-get-emacs)))
  :branch "main")
(el-get-bundle awasira/cp5022x.el
  :name cp5022x)

;;; initial load files
(dolist (sys-type (list (symbol-name system-type)
                        (symbol-name window-system)))

  (add-to-list 'load-path
               (expand-file-name
                (concat user-initial-directory "arch/" sys-type)))
  (load "init" t))
(add-to-list 'load-path (expand-file-name user-initial-directory))
(add-to-list 'load-path (expand-file-name user-site-lisp-directory))
(add-to-list 'load-path (expand-file-name (locate-user-emacs-file "secret.d/")))

;;; exec-path settings
(dolist (dir (list "/sbin" "/usr/sbin" "/bin" "/usr/bin" "/usr/local/bin"
                   "/opt/local/sbin" "/opt/local/bin" "/usr/gnu/bin"
                   ;; (expand-file-name "~/Applications/Emacs.app/Contents/Resources/bin")
                   (expand-file-name "~/bin")
                   (expand-file-name "~/.emacs.d/bin")
                   (expand-file-name "~/.emacs.d/el-get/mew/bin")
                   (expand-file-name "~/.local/bin")))

  (when (and (file-exists-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat dir ":" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))

(require 'japanese-init)

(el-get-bundle ddskk
  :type github
  :pkgname "skk-dev/ddskk"
  ;; :info "doc/skk.info"
  :load-path (".")
  :autoloads "skk-autoloads"
  :build `((,el-get-emacs "-batch" "-q" "-no-site-file" "-l" "SKK-MK" "-f" "SKK-MK-compile")
           ;; (,el-get-emacs "-batch" "-q" "-no-site-file" "-l" "SKK-MK" "-f" "SKK-MK-compile-info")
           ("cp" "skk-setup.el.in" "skk-setup.el")))

;;; global key-bindings
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
;; XXX PowerToys hack
(global-set-key (kbd "C-x <right>") 'find-file)
(global-set-key (kbd "C-x <end>") 'eval-last-sexp)

;; https://www.reddit.com/r/emacs/comments/wx7ytn/comment/ilue3ka/
(if (fboundp 'pixel-scroll-precision-mode)
    (progn
      (pixel-scroll-mode 1)
      (pixel-scroll-precision-mode 1)
      (setq pixel-scroll-precision-use-momentum t)
      (setq scroll-step 1)
      (setq pixel-scroll-precision-large-scroll-height nil)
      (setq pixel-scroll-precision-interpolation-factor 2.0)

      (defun smooth-scroll-half-page-down ()
        "Smooth scroll down"
        (interactive)
        (let ((half-height (/ (window-height) 2)))
          (pixel-scroll-precision-interpolate (* 8 (- half-height)))))

      (defun smooth-scroll-half-page-up ()
        "Smooth scroll down"
        (interactive)
        (let ((half-height (/ (window-height) 2)))
          (pixel-scroll-precision-interpolate (* 8 half-height))))

      ;; scroll-up-command
      (global-set-key (kbd "C-v") #'smooth-scroll-half-page-down)
      (global-set-key (kbd "M-v") #'smooth-scroll-half-page-up)))

(setq dired-bind-jump nil)
(setq dired-dwim-target t)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq enable-recursive-minibuffers t)
(setq cua-enable-cua-keys nil)

;;; backup files settings
(add-to-list 'backup-directory-alist (cons "\\.*$" (expand-file-name "~/.bak/")))
(setq delete-old-versions t
      make-backup-files t
      version-control t)

;;; coloring-region settings
(transient-mark-mode 1)

;;; show-paren settings
(show-paren-mode 1)

;;; face settings
(setq visible-bell t)
(el-get-bundle doom-themes)

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
(setq whitespace-global-modes
      '(not dired-mode tar-mode magit-log-mode magit-diff-mode mew-draft-mode))
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

(el-get-bundle dash)
(el-get-bundle shrink-path
  :type github
  :pkgname "zbelial/shrink-path.el"
  :depends (dash f s))
(el-get-bundle memoize)
(el-get-bundle all-the-icons)

(el-get-bundle doom-modeline
  :type github
  :depends (all-the-icons dash eldoc-eval shrink-path)
  :pkgname "seagle0128/doom-modeline")
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

(el-get-bundle symbol-overlay
  :type github
  :pkgname "wolray/symbol-overlay")

;;; uniquify settings
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;; (el-get-bundle emacs-async)
(add-hook 'dired-mode-hook
          #'(lambda ()
              (local-set-key (kbd "C-t") 'other-window)
              (local-set-key (kbd "r") 'wdired-change-to-wdired-mode)))
(add-hook 'dired-load-hook
          #'(lambda ()
              (load "dired-x")))

;;; Indent settings
(setq-default indent-tabs-mode nil)

(el-get-bundle editorconfig)
(el-get-bundle prettier-js)

;;; Misc settings
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

(el-get-bundle emacs-sql-indent
  :type github
  :pkgname "alex-hhh/emacs-sql-indent")

;;; view-mode settings
(add-hook 'view-mode-hook
          #'(lambda ()
              (setq view-read-only t)
              (auto-revert-mode 1)
              (setq line-move-visual nil)))
(with-eval-after-load 'view
  (define-key view-mode-map (kbd "h") 'backward-word)
  (define-key view-mode-map (kbd "l") 'forward-word)
  (define-key view-mode-map (kbd "j") 'next-line)
  (define-key view-mode-map (kbd "k") 'previous-line)
  (define-key view-mode-map " " 'scroll-up)
  (define-key view-mode-map (kbd "b") 'scroll-down))
(add-to-list 'auto-mode-alist '("\\.log$" . view-mode))

;;; frame-size settings
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

(unless (fboundp 'treesit-install-language-grammar)
  (progn
    (el-get-bundle elisp-tree-sitter)
    (global-tree-sitter-mode)
    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)))

(el-get-bundle terminal-here
  :type github
  :pkgname "davidshepherd7/terminal-here")

(el-get-bundle migemo)
(el-get-bundle visual-regexp)
;; (el-get-bundle elpa:undo-tree
;;   (global-undo-tree-mode))
(el-get-bundle easy-kill in leoliu/easy-kill)
(el-get-bundle yasnippet)
(el-get-bundle yasnippet-snippets)

;;; org-mode settings
(setq org-directory (concat external-directory "howm/"))
(setq org-return-follows-link t)
(setq org-startup-folded nil)
(setq org-startup-truncated nil)

(el-get-bundle expand-region)
(el-get-bundle multiple-cursors)
(el-get-bundle dumb-jump
  :type github
  :pkgname "jacktasia/dumb-jump")
(el-get-bundle smart-jump
  :type github
  :depends dumb-jump
  :pkgname "jojojames/smart-jump")
(el-get-bundle tree-mode
  :type github
  :pkgname "emacsorphanage/tree-mode")
(setenv "EDITOR" "emacsclient")
(el-get-bundle transient
  :branch "main")
(el-get-bundle with-editor
  :branch "main")
(el-get-bundle magit
  :branch "main")
(el-get-bundle ghub
  :branch "main")
;; (el-get-bundle forge)
;; (ghub-request "GET" "/user" nil
;;               :forge 'github
;;               :host "api.github.com"
;;               :username "nanasess"
;;               :auth 'forge)

(with-eval-after-load 'smerge-mode
  (define-key smerge-mode-map (kbd "M-n") 'smerge-next)
  (define-key smerge-mode-map (kbd "M-p") 'smerge-prev))

(el-get-bundle howm
  :type git
  :url "git://git.osdn.jp/gitroot/howm/howm.git"
  :build `(("./configure" ,(concat "--with-emacs=" el-get-emacs)) ("make"))
  :prepare (progn
             (defvar howm-menu-lang 'ja)
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
             (defvar howm-view-title-header "Title:")
             (setq howm-view-use-grep t)
             ;; see http://blechmusik.hatenablog.jp/entry/2013/07/09/015124
             (setq howm-process-coding-system 'utf-8-unix)
             (setq howm-todo-menu-types "[-+~!]")
             (defun parse-howm-title () nil)))

(el-get-bundle wgrep)
(el-get-bundle consult
  :type github
  :pkgname "minad/consult"
  :branch "main")
(el-get-bundle marginalia
  :type github
  :pkgname "minad/marginalia"
  :branch "main")
(el-get-bundle orderless
  :type github
  :pkgname "oantolin/orderless")
(el-get-bundle embark
  :type github
  :pkgname "oantolin/embark")
(el-get-bundle vertico
  :type github
  :pkgname "minad/vertico"
  :branch "main"
  :load-path ("." "extensions/")
  :compile ("vertico.el" "extensions/")
  :depends (consult marginalia orderless embark))

(el-get-bundle consult-ls-git
  :type github
  :pkgname "rcj/consult-ls-git"
  :branch "main")
(el-get-bundle consult-flycheck
  :type github
  :pkgname "minad/consult-flycheck"
  :branch "main")
(el-get-bundle consult-dir
  :type github
  :pkgname "karthink/consult-dir")
(el-get-bundle consult-tramp
  :type github
  :pkgname "Ladicle/consult-tramp"
  :branch "main")

;; Setting `init-consult.el` causes an error.
(with-eval-after-load 'consult
  (consult-customize
   consult-ripgrep
   consult-grep
   consult-git-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   ;;  ;; my/command-wrapping-consult       ;; disable auto previews inside my command
   ;;  ;; :preview-key '(:debounce 0.2 any) ;; Option 1: Delay preview
   :preview-key "C-.")               ;; Option 2: Manual preview
  )
(el-get-bundle sudo-edit
  :type github
  :pkgname "nflath/sudo-edit")

(el-get-bundle frame-local
  :type github
  :pkgname "sebastiencs/frame-local")

(el-get-bundle markdown-mode)

(el-get-bundle request)
(el-get-bundle spinner)
(el-get-bundle f)
(el-get-bundle ht)
(el-get-bundle flycheck)
;; (el-get-bundle treemacs
;;   :type github
;;   :pkgname "Alexander-Miller/treemacs"
;;   :load-path ("src/elisp"))

(el-get-bundle copilot
  :type github
  :pkgname "zerolfx/copilot.el"
  :branch "main")
(setq x-gtk-resize-child-frames 'resize-mode)
(el-get-bundle lsp-bridge
  :type github
  :pkgname "manateelazycat/lsp-bridge"
  :depends (posframe markdown-mode yasnippet orderless))

;; (el-get-bundle eldoc-box
;;   :type github
;;   :pkgname "casouri/eldoc-box")

;; (el-get-bundle js2-mode)
;; (el-get-bundle json-mode)
;; (el-get-bundle tide)

(el-get-bundle web-mode
  :type github
  :pkgname "nanasess/web-mode"
  :branch "eccube-engine")
(el-get-bundle yaml-mode)

(el-get-bundle php-mode
  :type github
  :pkgname "emacs-php/php-mode"
  :build `(("make" ,(format "EMACS=%s" el-get-emacs)))
  :load-path ("lisp"))
(el-get-bundle php-runtime
  :type github
  :pkgname "emacs-php/php-runtime.el")
(el-get-bundle php-skeleton
  :type github
  :pkgname "emacs-php/php-skeleton")
(el-get-bundle composer
  :type github
  :pkgname "emacs-php/composer.el"
  :depends (request))
(el-get-bundle phpstan
  :type github
  :pkgname "emacs-php/phpstan.el")

(el-get-bundle bui
  :type github
  :pkgname "alezost/bui.el")
(el-get-bundle groovy-mode
  :type github
  :pkgname "Groovy-Emacs-Modes/groovy-emacs-modes")

(el-get-bundle csv-mode in emacsmirror/csv-mode)
(el-get-bundle csharp-mode)

(el-get-bundle haskell-mode
  :type github
  :pkgname "haskell/haskell-mode"
  ;; :info "."
  ;; :build `(("make" ,(format "EMACS=%s" el-get-emacs) "all"))
)

(el-get-bundle dockerfile-mode)
(el-get-bundle docker-tramp)

(add-to-list 'load-path (concat user-emacs-directory ".mew.d"))
(load "mew-config" t t)
(setq mew-rc-file ".mew")

(el-get-bundle mew
  :type github
  :pkgname "kazu-yamamoto/Mew"
  :prepare
  (progn
    (setq mew-prog-mewl        (concat default-directory "bin/mewl"))
    (setq mew-prog-mime-encode (concat default-directory "bin/mewencode"))
    (setq mew-prog-mime-decode (concat default-directory "bin/mewencode"))
    (setq mew-prog-cmew        (concat default-directory "bin/cmew"))
    (setq mew-prog-est-update  (concat default-directory "bin/mewest"))
    (setq mew-prog-smew        (concat default-directory "bin/smew"))
    (setq mew-mbox-command     (concat default-directory "bin/incm")))
  :build `(("./configure" ,(concat "--with-emacs=" el-get-emacs)) ("make"))
  :load-path ("elisp/"))
;; (el-get-bundle twittering-mode)
(el-get-bundle popwin)

(unless (fboundp 'pixel-scroll-precision-mode)
  (progn
    (el-get-bundle deferred)
    (el-get-bundle inertial-scroll in kiwanami/emacs-inertial-scroll)))

;;; sqlite-dump
;;; original code was http://download.tuxfamily.org/user42/sqlite-dump.el
(el-get-bundle sqlite-dump
  :type github
  :pkgname "nanasess/sqlite-dump")

(defvar mkpasswd-command
  "head -c 10 < /dev/random | uuencode -m - | tail -n 2 |head -n 1 | head -c10")
(autoload 'mkpasswd "mkpasswd" nil t)
(el-get-bundle emacs-id-manager
  :type github
  :autoloads "id-manager"
  :pkgname "nanasess/emacs-id-manager")

(el-get-bundle nginx-mode)
(el-get-bundle po-mode)

;;; brew install plantuml
(el-get-bundle plantuml-mode
  :type github
  :pkgname "skuro/plantuml-mode")
(el-get-bundle mermaid-mode
  :type github
  :pkgname "abrochard/mermaid-mode")
(el-get-bundle fosi
  :type github
  :pkgname "hotoku/fosi"
  :branch "main"
  :load-path ("elisp/")
  :compile ("elisp/fosi.el"))
(autoload 'fosi "fosi" nil t)

(el-get-bundle wakatime-mode)

(el-get-bundle recentf-ext)

(el-get-bundle auto-save-buffers-enhanced
  :type github
  :pkgname "kentaro/auto-save-buffers-enhanced")
(el-get-bundle scratch-pop in zk-phi/scratch-pop)
(el-get-bundle gcmh)
(define-key minibuffer-local-map (kbd "C-x C-j") 'skk-kakutei)
(el-get 'sync)
(ffap-bindings)
;; (setq epa-pinentry-mode 'loopback)
(setq gc-cons-percentage 0.1)

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
;; (profiler-report)
;; (profiler-stop)

