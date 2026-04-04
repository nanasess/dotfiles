;;; init.el --- Emacs initialization file.

;; Author: Kentaro Ohkouchi  <nanasess@fsm.ne.jp>
;; URL: git://github.com/nanasess/dot.emacs.git

;;; Code:
;; see https://github.com/syl20bnr/spacemacs/commit/72c89df995ee1e4eb32ab982deb0911093048f20
(defconst my/saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(defvar user-initial-directory (locate-user-emacs-file "init.d/"))
(defvar user-site-lisp-directory (locate-user-emacs-file "site-lisp/"))
(defvar user-misc-directory (locate-user-emacs-file "etc/"))
(defvar user-bin-directory (locate-user-emacs-file "bin/"))
(defvar external-directory (expand-file-name "~/OneDrive - Skirnir Inc/emacs/"))
(defvar openweathermap-api-key nil)
(setopt debug-on-error t)
(setopt warning-minimum-level :error)

;;;; ============================================================
;;;; elpaca bootstrap
;;;; ============================================================
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; use-package integration
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;;;; ============================================================
;;;; Base libraries (wait for completion before dependents)
;;;; ============================================================
(use-package compat :ensure t)
(use-package dash :ensure t)
(use-package f :ensure t)
(use-package s :ensure t)
(use-package ht :ensure t)
(use-package spinner :ensure t)
(use-package request :ensure t)
(use-package aio :ensure (:host github :repo "skeeto/emacs-aio"))
(elpaca-wait)

;;;; ============================================================
;;;; Load path & initial settings
;;;; ============================================================

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
                   (expand-file-name "~/.ghcup/bin")
                   (expand-file-name "~/.cabal/bin")
                   (expand-file-name "~/bin")
                   (expand-file-name "~/.emacs.d/bin")
                   (expand-file-name "~/.local/bin")
                   (expand-file-name "~/.config/claude/local/")))
  (when (and (file-exists-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat dir ":" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))

;;;; ============================================================
;;;; Japanese input
;;;; ============================================================
(use-package cp5022x
  :ensure (:host github :repo "awasira/cp5022x.el")
  :demand t
  :config
  (define-coding-system-alias 'iso-2022-jp 'cp50220)
  (define-coding-system-alias 'euc-jp 'cp51932))

(unless (require 'japanese-init nil 'noerror)
  (set-language-environment "Japanese")
  (set-default-coding-systems 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8 . utf-8))
  (setenv "LANG" "ja_JP.UTF-8"))

(use-package nskk
  :ensure (:host github :repo "takeokunn/nskk.el" :branch "main"
           :build (:not elpaca-build-autoloads))
  :demand t
  :bind (("C-j" . nskk-toggle-mode)
         ("C-x C-j" . nskk-toggle-mode))
  :custom
  (nskk-dict-user-dictionary-file (concat external-directory "nskk/jisyo"))
  (nskk-dict-system-dictionary-files
   (list (concat external-directory "ddskk/SKK-JISYO.all.utf8")))
  (nskk-show-tooltip t)
  (nskk-use-color-cursor t)
  (nskk-converter-auto-start-henkan t)
  (nskk-henkan-show-candidates-nth 5))

(elpaca-wait)

;;;; ============================================================
;;;; Global key-bindings
;;;; ============================================================
(global-unset-key (kbd "C-M-t"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-\\"))
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-t") 'other-window)
(global-set-key (kbd "C-z C-u") 'other-frame)
(global-set-key (kbd "C-M-g") 'end-of-buffer)
(global-set-key (kbd "C-M-j") 'next-line)
(global-set-key (kbd "C-M-k") 'previous-line)
(global-set-key (kbd "C-M-h") 'backward-char)
(global-set-key (kbd "C-M-l") 'forward-char)
;; XXX PowerToys hack
(global-set-key (kbd "C-x <right>") 'find-file)
(global-set-key (kbd "C-x <end>") 'eval-last-sexp)

;;;; ============================================================
;;;; Scroll settings
;;;; ============================================================
(use-package ultra-scroll
  :ensure (:host github :repo "jdtsmith/ultra-scroll" :branch "main")
  :hook (emacs-startup
         . (lambda ()
             (pixel-scroll-precision-mode t)
             (setopt scroll-conservatively 101
                    scroll-margin 0
                    scroll-step 1
                    pixel-scroll-precision-use-momentum t
                    pixel-scroll-precision-interpolate-mice t
                    pixel-scroll-precision-large-scroll-height 10.0
                    pixel-scroll-precision-interpolation-factor 1.0
                    pixel-scroll-precision-interpolate-page t
                    pixel-scroll-precision-interpolation-total-time 0.25)
             (ultra-scroll-mode 1)

             ;; https://www.reddit.com/r/emacs/comments/13accue/emacs_29_pixelscrollprecisionmode_seems_to_break/
             (defun +pixel-scroll-interpolate-down ()
               "Interpolate a scroll downwards by one page."
               (interactive)
               (if pixel-scroll-precision-interpolate-page
                   (pixel-scroll-precision-interpolate
                    (- (/ (window-text-height nil t) 2)) nil 1)
                 (cua-scroll-up)))

             (defun +pixel-scroll-interpolate-up ()
               "Interpolate a scroll upwards by one page."
               (interactive)
               (if pixel-scroll-precision-interpolate-page
                   (pixel-scroll-precision-interpolate
                    (/ (window-text-height nil t) 2) nil 1)
                 (cua-scroll-down)))
             (global-set-key (kbd "C-v") '+pixel-scroll-interpolate-down)
             (global-set-key (kbd "M-v") '+pixel-scroll-interpolate-up))))

;;;; ============================================================
;;;; Clipboard (pgtk / wl-clipboard)
;;;; ============================================================
;; see http://cha.la.coocan.jp/wp/2024/05/05/post-1300/
(if (featurep 'pgtk)
    (if (and (zerop (call-process "which" nil nil nil "wl-copy"))
             (zerop (call-process "which" nil nil nil "wl-paste")))
        ;; credit: yorickvP on Github
        ;; see https://gist.github.com/yorickvP/6132f237fbc289a45c808d8d75e0e1fb
        (progn
          (setq wl-copy-process nil)
          (defun wl-copy (text)
            (setq wl-copy-process (make-process :name "wl-copy"
                                                :buffer nil
                                                :command '("wl-copy" "-f" "-n")
                                                :connection-type 'pipe
                                                :noquery t))
            (process-send-string wl-copy-process text)
            (process-send-eof wl-copy-process))
          (defun wl-paste ()
            (if (and wl-copy-process (process-live-p wl-copy-process))
                nil
              (when (executable-find "wl-paste")
                (shell-command-to-string "type -a wl-paste > /dev/null 2>&1 && wl-paste -n | tr -d \r"))))
          (setq interprogram-cut-function 'wl-copy)
          (setq interprogram-paste-function 'wl-paste))))

;;;; ============================================================
;;;; Built-in settings
;;;; ============================================================
(use-package emacs
  :ensure nil
  :config
  (setopt dired-bind-jump nil)
  (setopt dired-dwim-target t)
  (setopt ediff-window-setup-function 'ediff-setup-windows-plain)
  (setopt enable-recursive-minibuffers t)
  (setopt cua-enable-cua-keys nil)

  ;; backup files
  (add-to-list 'backup-directory-alist (cons "\\.*$" (expand-file-name "~/.bak/")))
  (setopt delete-old-versions t
         make-backup-files t
         version-control t)

  ;; show-paren
  (show-paren-mode 1)

  ;; visible-bell
  (setopt visible-bell t)

  ;; whitespace
  (require 'whitespace)
  (setopt whitespace-style '(face trailing tabs spaces space-mark tab-mark))
  (setopt whitespace-display-mappings nil)
  (setopt whitespace-trailing-regexp  "\\([ \u00A0]+\\)$")
  (setopt whitespace-space-regexp "\\(\u3000+\\)")
  (setopt whitespace-global-modes
          '(not dired-mode tar-mode magit-log-mode magit-diff-mode mew-draft-mode))
  (global-whitespace-mode t)

  ;; hl-line
  ;; see also http://rubikitch.com/2015/05/14/global-hl-line-mode-timer/
  (global-hl-line-mode 0)
  (defun global-hl-line-timer-function ()
    (global-hl-line-unhighlight-all)
    (let ((global-hl-line-mode t))
      (global-hl-line-highlight)))
  (setq global-hl-line-timer
        (run-with-idle-timer 0.1 t 'global-hl-line-timer-function))

  ;; line/column numbers
  (line-number-mode -1)
  (column-number-mode 1)
  (size-indication-mode 1)
  (global-display-line-numbers-mode t)

  ;; uniquify
  (require 'uniquify)
  (setopt uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setopt uniquify-ignore-buffers-re "*[^*]+*")

  ;; dired
  (add-hook 'dired-mode-hook
            #'(lambda ()
                (local-set-key (kbd "C-t") 'other-window)
                (local-set-key (kbd "r") 'wdired-change-to-wdired-mode)))
  (add-hook 'dired-load-hook
            #'(lambda ()
                (load "dired-x")))

  ;; indent
  (setq-default indent-tabs-mode nil)

  ;; misc
  (setopt indicate-empty-lines t)
  (setopt isearch-lax-whitespace nil)
  (setopt mouse-yank-at-point t)
  (setopt select-enable-clipboard t)
  (setopt select-enable-primary t)
  (setopt save-interprogram-paste-before-kill t)
  (delete-selection-mode 1)

  ;; XXX allow remembering risky and safe variables
  ;; see https://emacs.stackexchange.com/a/44604
  (defun risky-local-variable-p (sym &optional _ignored) nil)
  (defun safe-local-variable-p (sym val) t)

  ;; view-mode
  (add-hook 'view-mode-hook
            #'(lambda ()
                (setopt view-read-only t)
                (auto-revert-mode 1)
                (setopt line-move-visual nil)))
  (with-eval-after-load 'view
    (define-key view-mode-map (kbd "h") 'backward-word)
    (define-key view-mode-map (kbd "l") 'forward-word)
    (define-key view-mode-map (kbd "j") 'next-line)
    (define-key view-mode-map (kbd "k") 'previous-line)
    (define-key view-mode-map " " 'scroll-up)
    (define-key view-mode-map (kbd "b") 'scroll-down))
  (add-to-list 'auto-mode-alist '("\\.log$" . view-mode))

  ;; treesit
  (setopt treesit-font-lock-level 4)
  (setopt treesit-language-source-alist
        '((csharp . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))))

  ;; editor
  (setenv "EDITOR" "emacsclient"))

;;;; ============================================================
;;;; Theme & UI
;;;; ============================================================
(use-package doom-themes
  :ensure t
  :hook (emacs-startup
         . (lambda ()
             (load-theme 'doom-solarized-light t))))

(use-package nerd-icons
  :ensure (:host github :repo "rainstormstudio/nerd-icons.el" :branch "main")
  :config
  (setf (alist-get "php" nerd-icons-extension-icon-alist)
        '(nerd-icons-sucicon "nf-seti-php" :face nerd-icons-lpurple))
  (push '("tpl" nerd-icons-sucicon "nf-seti-smarty" :face nerd-icons-yellow)
        nerd-icons-extension-icon-alist)
  (push '("twig" nerd-icons-sucicon "nf-seti-twig" :face nerd-icons-lgreen)
        nerd-icons-extension-icon-alist))

(use-package shrink-path
  :ensure (:host github :repo "zbelial/shrink-path.el"))

(use-package doom-modeline
  :ensure t
  :hook (emacs-startup . doom-modeline-mode)
  :custom
  (doom-modeline-vcs-max-length 999)
  (doom-modeline-buffer-file-name-style 'buffer-name))

(use-package symbol-overlay
  :ensure (:host github :repo "wolray/symbol-overlay")
  :bind ("M-i" . symbol-overlay-put))

;;;; ============================================================
;;;; Frame size utilities
;;;; ============================================================
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

;;;; ============================================================
;;;; Completion framework (vertico, consult, marginalia, orderless, embark)
;;;; ============================================================
(use-package orderless
  :ensure (:host github :repo "oantolin/orderless")
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides nil))

(use-package marginalia
  :ensure (:host github :repo "minad/marginalia" :branch "main"))

(use-package consult
  :ensure (:host github :repo "minad/consult" :branch "main")
  :bind (("C-;" . consult-buffer)
         ([remap goto-line] . consult-goto-line)
         ("C-M-s" . consult-line)
         ("C-x C-d" . consult-dir)
         ("C-z s" . consult-howm-do-ag)
         ("C-z l" . consult-ls-git))
  :custom
  (consult-narrow-key ">")
  (consult-widen-key "<")
  (consult-preview-key "M-.")
  :config
  (defun consult-howm-do-ag ()
    (interactive)
    (consult-ripgrep howm-directory))
  (consult-customize
   consult-ripgrep
   consult-grep
   consult-git-grep
   consult-bookmark consult-recent-file consult-xref
   :preview-key "C-."))

(use-package embark
  :ensure (:host github :repo "oantolin/embark")
  :bind ("C-," . embark-act)
  :config
  (with-eval-after-load 'consult
    (require 'embark-consult)
    (define-key embark-file-map "s" #'sudo-edit)))

(use-package vertico
  :ensure (:host github :repo "minad/vertico" :branch "main"
           :files ("*.el" "extensions/*.el"))
  :hook (emacs-startup
         . (lambda ()
             (vertico-mode)
             (marginalia-mode)
             (savehist-mode)
             (add-to-list 'savehist-additional-variables 'kill-ring)
             (add-to-list 'savehist-additional-variables 'log-edit-comment-ring)
             (add-to-list 'savehist-additional-variables 'search-ring)
             (add-to-list 'savehist-additional-variables 'regexp-search-ring)))
  :bind (:map vertico-map
         ("C-l" . vertico-directory-up)
         ("C-j" . vertico-directory-enter)
         ("M-v" . vertico-next-group)
         ("C-v" . vertico-previous-group)
         ("C-z C-r" . vertico-repeat))
  :custom
  (vertico-count 20)
  :config
  (setopt read-file-name-completion-ignore-case t
         read-buffer-completion-ignore-case t
         completion-ignore-case t)
  (require 'consult)
  (require 'orderless)
  (require 'marginalia)
  (require 'savehist)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package consult-ls-git
  :ensure (:host github :repo "rcj/consult-ls-git" :branch "main"))

(use-package consult-flycheck
  :ensure (:host github :repo "minad/consult-flycheck" :branch "main"))

(use-package consult-dir
  :ensure (:host github :repo "karthink/consult-dir"))

(use-package consult-tramp
  :ensure (:host github :repo "Ladicle/consult-tramp" :branch "main")
  :custom
  (consult-tramp-method "sshx"))

(use-package sudo-edit
  :ensure (:host github :repo "nflath/sudo-edit"))

(use-package wgrep
  :ensure t
  :custom
  (wgrep-enable-key "r"))

;;;; ============================================================
;;;; Editing support
;;;; ============================================================
(use-package terminal-here
  :ensure (:host github :repo "davidshepherd7/terminal-here")
  :custom
  (terminal-here-mac-terminal-command 'iterm2))

(use-package migemo
  :ensure t
  :defer t
  :init
  (defvar migemo-dictionary
    (concat external-directory "migemo/dict/utf-8/migemo-dict"))
  (when (file-exists-p migemo-dictionary)
    (setopt migemo-command "cmigemo"
            migemo-options '("-q" "--emacs" "-i" "\a")
            migemo-user-dictionary nil
            migemo-regex-dictionary nil
            migemo-use-pattern-alist t
            migemo-use-frequent-pattern-alist t
            migemo-pattern-alist-length 10000
            migemo-coding-system 'utf-8-unix))
  (add-hook 'isearch-mode-hook #'(lambda ()
                                   (unless (featurep 'migemo)
                                     (require 'migemo))
                                   (migemo-init))))

(use-package visual-regexp
  :ensure t
  :bind ("M-%" . vr/query-replace))

(use-package undo-tree
  :ensure (:host github :repo "emacsmirror/undo-tree")
  :hook (emacs-startup . global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  (undo-tree-auto-save-history t)
  (undo-tree-enable-undo-in-region t)
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo-tree" user-emacs-directory)))))

(use-package easy-kill
  :ensure (:host github :repo "leoliu/easy-kill"))

;; Copy menu with transient (M-w)
(defun my/copy-buffer-file-name ()
  "Copy full path to kill ring."
  (interactive)
  (if-let ((f (buffer-file-name)))
      (progn (kill-new f) (message "Copied: %s" f))
    (message "Buffer has no file")))

(defun my/copy-buffer-file-name-nondirectory ()
  "Copy file name only to kill ring."
  (interactive)
  (if-let ((f (buffer-file-name)))
      (let ((name (file-name-nondirectory f)))
        (kill-new name) (message "Copied: %s" name))
    (message "Buffer has no file")))

(defun my/copy-buffer-directory ()
  "Copy directory to kill ring."
  (interactive)
  (if-let ((f (buffer-file-name)))
      (let ((dir (file-name-directory f)))
        (kill-new dir) (message "Copied: %s" dir))
    (message "Buffer has no file")))

(defun my/copy-buffer-file-name-with-line ()
  "Copy file:line format to kill ring."
  (interactive)
  (if-let ((f (buffer-file-name)))
      (let ((loc (format "%s:%d" f (line-number-at-pos))))
        (kill-new loc) (message "Copied: %s" loc))
    (message "Buffer has no file")))

(defun my/copy-or-menu ()
  "Copy region if active, otherwise show copy menu."
  (interactive)
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end))
    (if (fboundp 'my/copy-dwim)
        (my/copy-dwim)
      (message "Copy menu not available. Run M-x magit-status to load transient first."))))

(global-set-key (kbd "M-w") #'my/copy-or-menu)

(use-package yasnippet
  :ensure t
  :hook (emacs-startup . yas-global-mode)
  :bind (:map yas-minor-mode-map
         ([(tab)] . nil)
         ("TAB" . nil)))

(use-package yasnippet-snippets
  :ensure t)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region)
  :init
  (setopt shift-select-mode nil))

(use-package multiple-cursors
  :ensure t
  :bind ("<C-M-return>" . mc/edit-lines))

(use-package prettier-js
  :ensure t)

;;;; ============================================================
;;;; SQL
;;;; ============================================================
(use-package sql-indent
  :ensure (:host github :repo "alex-hhh/emacs-sql-indent")
  :hook (sql-mode . (lambda ()
                      (set (make-local-variable 'sql-product) 'sqlite)
                      (sql-indent-enable)
                      (setq sqlind-basic-offset 4))))

;;;; ============================================================
;;;; org-mode
;;;; ============================================================
(setopt org-directory (concat external-directory "howm/"))
(setopt org-return-follows-link t)
(setopt org-startup-folded nil)
(setopt org-startup-truncated nil)

;;;; ============================================================
;;;; Git / Magit
;;;; ============================================================
(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :bind ("C-z m" . magit-status)
  :config
  (with-eval-after-load 'transient
    (transient-define-prefix my/copy-dwim ()
      "Select what to copy."
      [["File Info"
        ("f" "Full path" my/copy-buffer-file-name :transient nil)
        ("n" "File name only" my/copy-buffer-file-name-nondirectory :transient nil)
        ("d" "Directory" my/copy-buffer-directory :transient nil)
        ("l" "File:line" my/copy-buffer-file-name-with-line :transient nil)]
       ["Text (easy-kill)"
        ("w" "Word" (lambda () (interactive) (easy-kill ?w)) :transient nil)
        ("s" "Symbol" (lambda () (interactive) (easy-kill ?s)) :transient nil)
        ("L" "Line" (lambda () (interactive) (easy-kill ?l)) :transient nil)
        ("-" "Defun" (lambda () (interactive) (easy-kill ?-)) :transient nil)]]))

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

  (setopt magit-diff-refine-hunk t)
  (add-to-list 'magit-process-password-prompt-regexps "^パスフレーズを入力: ?$")
  (define-key magit-mode-map "v" #'endless/visit-pull-request-url)
  (define-key magit-log-mode-map (kbd "j") 'magit-section-forward)
  (define-key magit-log-mode-map (kbd "k") 'magit-section-backward)
  (remove-hook 'server-switch-hook 'magit-commit-diff))

(with-eval-after-load 'smerge-mode
  (define-key smerge-mode-map (kbd "M-n") 'smerge-next)
  (define-key smerge-mode-map (kbd "M-p") 'smerge-prev))

;;;; ============================================================
;;;; howm (commented out — preserved for future use)
;;;; ============================================================
(setopt howm-directory (concat external-directory "howm/"))
(setopt howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.md")
;; (el-get-bundle howm
;;   :type github
;;   :pkgname "kaorahi/howm"
;;   :build `(("./configure" ,(concat "--with-emacs=" el-get-emacs)) ("make"))
;;   :prepare (progn
;;              (defvar howm-menu-lang 'ja)
;;              ...))

;; see https://stackoverflow.com/a/384346
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(global-set-key (kbd "C-z c") 'howm-create)
(global-set-key (kbd "C-c ,c") 'howm-create)

;;;; ============================================================
;;;; Markdown
;;;; ============================================================
(use-package markdown-mode
  :ensure t
  :mode (("\\.\\(markdown\\|md\\)\\'" . gfm-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t)
  (markdown-indent-on-enter 'indent-and-new-item)
  :bind (:map markdown-mode-map
         ("<S-tab>" . markdown-shifttab)))

(use-package polymode
  :ensure (:host github :repo "polymode/polymode"))

(use-package poly-markdown
  :ensure (:host github :repo "polymode/poly-markdown"))

;;;; ============================================================
;;;; Flycheck
;;;; ============================================================
(use-package flycheck
  :ensure t)

;;;; ============================================================
;;;; Programming languages
;;;; ============================================================

;;; TypeScript
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . tsx-ts-mode))

;;; jq
(use-package jq-mode
  :ensure (:host github :repo "ljos/jq-mode"))

;;; web-mode (user fork)
(use-package web-mode
  :ensure (:host github :repo "nanasess/web-mode" :branch "eccube-engine")
  :mode (("\\.tpl\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
         ("\\.twig\\'" . web-mode)
         ("\\.html\\'" . web-mode))
  :config
  (setopt web-mode-enable-block-face t)
  (setopt web-mode-enable-current-column-highlight nil)
  (setopt web-mode-enable-auto-indentation nil)
  (add-hook 'web-mode-hook
            #'(lambda ()
                (setopt web-mode-enable-auto-indentation nil)))
  (add-hook 'web-mode-hook
            #'(lambda ()
                (when (string-equal "tpl" (file-name-extension buffer-file-name))
                  (web-mode-set-engine "eccube")))))

;;; YAML
(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml$")

;;; PHP
(add-to-list 'auto-mode-alist '("\\.\\(inc\\|php[s34]?\\)$" . php-ts-mode))
(with-eval-after-load 'php-ts-mode
  (electric-indent-local-mode t)
  (electric-layout-mode t)
  (electric-pair-local-mode t))

(use-package php-runtime
  :ensure (:host github :repo "emacs-php/php-runtime.el"))

(use-package php-skeleton
  :ensure (:host github :repo "emacs-php/php-skeleton"))

(use-package composer
  :ensure (:host github :repo "emacs-php/composer.el"))

(use-package phpstan
  :ensure (:host github :repo "emacs-php/phpstan.el"))

;;; Groovy
(use-package groovy-mode
  :ensure (:host github :repo "Groovy-Emacs-Modes/groovy-emacs-modes"))

;;; CSV
(use-package csv-mode
  :ensure t)

;;; F#
(use-package fsharp-mode
  :ensure (:host github :repo "fsharp/emacs-fsharp-mode"))

;;; Haskell
(use-package haskell-mode
  :ensure (:host github :repo "haskell/haskell-mode")
  :config
  (setopt haskell-stylish-on-save t)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

;;; Dockerfile
(use-package dockerfile-mode
  :ensure t)

;;; Terraform
(use-package terraform-mode
  :ensure t
  :custom
  (terraform-format-on-save t))

;;; Nginx
(use-package nginx-mode
  :ensure t)

;;; Mermaid
(use-package mermaid-mode
  :ensure (:host github :repo "abrochard/mermaid-mode")
  :custom
  (mermaid-output-format ".pdf")
  :bind (:map mermaid-mode-map
         ("TAB" . mermaid-indent-line)
         ("<tab>" . mermaid-indent-line)))

;;; ebuild-mode (Gentoo)
(use-package ebuild-mode
  :ensure (:url "https://anongit.gentoo.org/git/proj/ebuild-mode.git"
           :pre-build (("make"))))

;;;; ============================================================
;;;; Email (oauth2)
;;;; ============================================================
(use-package oauth2
  :ensure (:host github :repo "emacsmirror/oauth2"))

;;;; ============================================================
;;;; Misc tools
;;;; ============================================================
(use-package bui
  :ensure (:host github :repo "alezost/bui.el"))

(use-package popwin
  :ensure t)

(use-package sqlite-dump
  :ensure (:host github :repo "nanasess/sqlite-dump")
  :init
  (modify-coding-system-alist 'file "\\.\\(db\\|sqlite\\)\\'" 'raw-text-unix)
  (add-to-list 'auto-mode-alist '("\\.\\(db\\|sqlite\\)\\'" . sqlite-dump)))

(defvar mkpasswd-command
  "head -c 10 < /dev/random | uuencode -m - | tail -n 2 |head -n 1 | head -c10")
(autoload 'mkpasswd "mkpasswd" nil t)

(use-package fosi
  :ensure (:host github :repo "hotoku/fosi" :branch "main"
           :files ("elisp/*.el"))
  :commands fosi)

(use-package shell-maker
  :ensure (:host github :repo "xenodium/shell-maker" :branch "main"))

(use-package mcp
  :ensure (:host github :repo "lizqwerscott/mcp.el"))

;; (el-get-bundle wakatime-mode)
;; (add-to-list 'load-path (concat user-emacs-directory ".wakatime.d"))
;; (load "wakatime-config" t t)
;; (add-hook 'emacs-startup-hook 'global-wakatime-mode)
;; (with-eval-after-load 'wakatime-mode
;;   (setopt wakatime-cli-path "/usr/bin/wakatime"))

(use-package recentf-ext
  :ensure t
  :hook (emacs-startup
         . (lambda ()
             (setopt recentf-max-saved-items 50000)
             (recentf-mode 1))))

(use-package auto-save-buffers-enhanced
  :ensure (:host github :repo "kentaro/auto-save-buffers-enhanced")
  :config
  (setopt auto-save-buffers-enhanced-interval 30)
  (setopt auto-save-buffers-enhanced-save-scratch-buffer-to-file-p t)
  (setopt auto-save-buffers-enhanced-file-related-with-scratch-buffer
        (concat howm-directory "scratch.txt"))
  (auto-save-buffers-enhanced t)
  :bind ("C-x a s" . auto-save-buffers-enhanced-toggle-activity))

(use-package gcmh
  :ensure t
  :demand t
  :config
  (gcmh-mode 1)
  (setopt gcmh-verbose t))

;;;; ============================================================
;;;; Minibuffer extras
;;;; ============================================================
(define-key minibuffer-local-map (kbd "C-x C-j") 'nskk-kakutei)

;; npm i -g vscode-json-languageserver
;; for json format
;; see https://qiita.com/saku/items/d97e930ffc9ca39ac976
(defun jq-format (beg end)
  (interactive "r")
  (shell-command-on-region beg end "jq ." nil t))

;;;; ============================================================
;;;; Finalize
;;;; ============================================================
(elpaca-wait)
(ffap-bindings)
(setq gc-cons-percentage 0.1)
(setq file-name-handler-alist my/saved-file-name-handler-alist)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
