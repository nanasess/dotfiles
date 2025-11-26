;;; init.el --- Emacs initialization file.

;; Author: Kentaro Ohkouchi  <nanasess@fsm.ne.jp>
;; URL: git://github.com/nanasess/dot.emacs.git

;;; Code:
;; (require 'profiler)
;; (profiler-start 'cpu)
;; see https://github.com/syl20bnr/spacemacs/commit/72c89df995ee1e4eb32ab982deb0911093048f20
;; (setq garbage-collection-messages t)
(defconst my/saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
;; see https://zenn.dev/zk_phi/books/cba129aacd4c1418ade4/viewer/4d0a9dde1043c6eaffad
;; (defvar setup-tracker--level 0)
;; (defvar setup-tracker--parents nil)
;; (defvar setup-tracker--times nil)

;; (when load-file-name
;;   (push load-file-name setup-tracker--parents)
;;   (push (current-time) setup-tracker--times)
;;   (setq setup-tracker--level (1+ setup-tracker--level)))

;; (add-variable-watcher
;;  'load-file-name
;;  (lambda (_ v &rest __)
;;    (cond ((equal v (car setup-tracker--parents))
;;           nil)
;;          ((equal v (cadr setup-tracker--parents))
;;           (setq setup-tracker--level (1- setup-tracker--level))
;;           (let* ((now (current-time))
;;                  (start (pop setup-tracker--times))
;;                  (elapsed (+ (* (- (nth 1 now) (nth 1 start)) 1000)
;;                              (/ (- (nth 2 now) (nth 2 start)) 1000))))
;;             (with-current-buffer (get-buffer-create "*setup-tracker*")
;;               (save-excursion
;;                 (goto-char (point-min))
;;                 (dotimes (_ setup-tracker--level) (insert "> "))
;;                 (insert
;;                  (file-name-nondirectory (pop setup-tracker--parents))
;;                  " (" (number-to-string elapsed) " msec)\n")))))
;;          (t
;;           (push v setup-tracker--parents)
;;           (push (current-time) setup-tracker--times)
;;           (setq setup-tracker--level (1+ setup-tracker--level))))))
(eval-when-compile (require 'cl))
(eval '(eval-when-compile (require 'cl)))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(defvar user-initial-directory (locate-user-emacs-file "init.d/"))
(defvar user-site-lisp-directory (locate-user-emacs-file "site-lisp/"))
(defvar user-misc-directory (locate-user-emacs-file "etc/"))
(defvar user-bin-directory (locate-user-emacs-file "bin/"))
(defvar external-directory (expand-file-name "~/OneDrive - Skirnir Inc/emacs/"))
(defvar openweathermap-api-key nil)
(setq debug-on-error t)
(setq warning-minimum-level :error)

(setopt el-get-bundle-sync t
        el-get-is-lazy t
        el-get-verbose nil
        el-get-bundle-byte-compile t
        el-get-auto-update-cached-recipes nil)

(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp))
  (with-eval-after-load 'el-get-git
    (setopt el-get-git-shallow-clone t)))

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
(with-eval-after-load 'cp5022x
    (define-coding-system-alias 'iso-2022-jp 'cp50220)
    (define-coding-system-alias 'euc-jp 'cp51932))

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
                   (expand-file-name "~/.ghcup/bin")
                   (expand-file-name "~/.cabal/bin")
                   (expand-file-name "~/bin")
                   (expand-file-name "~/.emacs.d/bin")
                   (expand-file-name "~/.emacs.d/el-get/mew/bin")
                   (expand-file-name "~/.local/bin")
                   (expand-file-name "~/.config/claude/local/")))

  (when (and (file-exists-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat dir ":" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))

(unless (require 'japanese-init nil 'noerror)
  (set-language-environment "Japanese")
  (set-default-coding-systems 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8 . utf-8))
  (setenv "LANG" "ja_JP.UTF-8"))

(el-get-bundle ddskk
  :type github
  :pkgname "skk-dev/ddskk"
  ;; :info "doc/skk.info"
  ;; :load-path (".")
  ;; :autoloads "skk-autoloads"
  :features ("skk-setup")
  :build `(("sh" "-c" "echo \"(setq SKK_SET_JISYO t)\" > SKK-CFG")
           (,el-get-emacs "-batch" "-q" "-no-site-file" "-l" "SKK-MK" "-f" "SKK-MK-compile")
           (,el-get-emacs "-batch" "-q" "-no-site-file" "-l" "SKK-MK" "-f" "SKK-MK-compile")
           ;; (,el-get-emacs "-batch" "-q" "-no-site-file" "-l" "SKK-MK" "-f" "SKK-MK-compile-info")
           ("cp" "skk-setup.el.in" "skk-setup.el")))
(setopt
      skk-server-host nil
      skk-server-portnum nil
      skk-user-directory (concat external-directory "ddskk")
      skk-init-file (concat user-initial-directory "skk-init.el")
      skk-isearch-start-mode 'latin)
(setq skk-preload nil)

;;; global key-bindings
(add-hook
 'emacs-startup-hook
 #'(lambda ()
     (which-key-mode 1)))
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

(el-get-bundle ultra-scroll
  :type github
  :pkgname "jdtsmith/ultra-scroll"
  :branch "main")
(add-hook 'emacs-startup-hook
          #'(lambda ()
              (pixel-scroll-precision-mode t)
              (setq scroll-conservatively 101 ; important!
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
                     ;; Don't use an interpolation factor,
                     ;; since we want exactly 1 page to be scrolled.
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
              (global-set-key (kbd "M-v") '+pixel-scroll-interpolate-up)))

;; see http://cha.la.coocan.jp/wp/2024/05/05/post-1300/
;; you need to install "wl-clipboard" first.
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
                nil ; should return nil if we're the current paste owner
              (when (executable-find "wl-paste")
                (shell-command-to-string "type -a wl-paste > /dev/null 2>&1 && wl-paste -n | tr -d \r"))))
          (setq interprogram-cut-function 'wl-copy)
          (setq interprogram-paste-function 'wl-paste))))

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
;; (el-get-bundle puni
;;   :type github
;;   :pkgname "AmaiKinono/puni")
;; (add-hook
;;  'emacs-startup-hook
;;  #'(lambda ()
;;      (puni-global-mode)
;;      (add-hook 'term-mode-hook #'puni-disable-puni-mode)))

;;; face settings
(setq visible-bell t)
(el-get-bundle doom-themes)
(add-hook
 'emacs-startup-hook
 #'(lambda ()
     (require 'doom-themes)
     ;; use solarized.
     (load-theme 'doom-solarized-light t)
     (with-eval-after-load 'vertico
       (custom-set-faces
        `(vertico-group-title ((t (:foreground ,(doom-color 'base7)))))))
     (with-eval-after-load 'corfu
       (custom-set-faces
        `(corfu-annotations ((t (:foreground ,(doom-color 'green)))))))))

(require 'whitespace)
(setq whitespace-style
      '(face trailing tabs spaces space-mark tab-mark))
(setq whitespace-display-mappings nil)
(setq whitespace-trailing-regexp  "\\([ \u00A0]+\\)$")
(setq whitespace-space-regexp "\\(\u3000+\\)")
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
(el-get-bundle nerd-icons.el
  :type github
  :pkgname "rainstormstudio/nerd-icons.el"
  :branch "main")
(with-eval-after-load 'nerd-icons
  (setf (alist-get "php" nerd-icons-extension-icon-alist)
        '(nerd-icons-sucicon "nf-seti-php" :face nerd-icons-lpurple))
  (push '("tpl" nerd-icons-sucicon "nf-seti-smarty" :face nerd-icons-yellow)
        nerd-icons-extension-icon-alist)
  (push '("twig" nerd-icons-sucicon "nf-seti-twig" :face nerd-icons-lgreen)
        nerd-icons-extension-icon-alist))

(el-get-bundle doom-modeline
  :type github
  :depends (all-the-icons dash eldoc-eval shrink-path nerd-icons.el)
  :pkgname "seagle0128/doom-modeline")
(with-eval-after-load 'doom-modeline-core
  (setopt doom-modeline-vcs-max-length 999)
  (setopt doom-modeline-buffer-file-name-style 'buffer-name))

(add-hook 'emacs-startup-hook 'doom-modeline-mode)

(line-number-mode -1)
(column-number-mode 1)
(size-indication-mode 1)
(global-display-line-numbers-mode t)

(el-get-bundle symbol-overlay
  :type github
  :pkgname "wolray/symbol-overlay")
(global-set-key (kbd "M-i") 'symbol-overlay-put)

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
(editorconfig-mode 1)
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
(add-hook 'sql-mode-hook #'(lambda ()
                             (set (make-local-variable 'sql-product) 'sqlite)
                             (sql-indent-enable)
                             (setq sqlind-basic-offset 4)))

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

(setopt treesit-font-lock-level 4)
(unless (fboundp 'treesit-install-language-grammar)
  (progn
    (el-get-bundle elisp-tree-sitter)
    (global-tree-sitter-mode)
    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)))

(el-get-bundle terminal-here
  :type github
  :pkgname "davidshepherd7/terminal-here")
(setopt terminal-here-mac-terminal-command 'iterm2)

(el-get-bundle migemo)
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
                                 (migemo-init)))

(el-get-bundle visual-regexp)
(define-key global-map (kbd "M-%") 'vr/query-replace)

(el-get-bundle undo-tree
  :type github
  :pkgname "emacsmirror/undo-tree")
(add-hook 'emacs-startup-hook
          #'(lambda ()
              (global-undo-tree-mode)))
(setopt undo-tree-visualizer-timestamps t)
(setopt undo-tree-visualizer-diff t)
(setopt undo-tree-auto-save-history t)
(setopt undo-tree-enable-undo-in-region t)
(setopt undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo-tree" user-emacs-directory))))

(el-get-bundle easy-kill in leoliu/easy-kill)
(with-eval-after-load 'easy-kill
  (global-set-key [remap kill-ring-save] 'easy-kill))

(el-get-bundle yasnippet)
(add-hook 'emacs-startup-hook 'yas-global-mode)
(with-eval-after-load 'yasnippet
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil))
(el-get-bundle yasnippet-snippets)

;;; org-mode settings
(setopt org-directory (concat external-directory "howm/"))
(setopt org-return-follows-link t)
(setopt org-startup-folded nil)
(setopt org-startup-truncated nil)

(el-get-bundle expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
;; see https://github.com/magnars/expand-region.el/issues/220
(setq shift-select-mode nil)

(el-get-bundle multiple-cursors)
(global-set-key (kbd "<C-M-return>") 'mc/edit-lines)

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
(el-get-bundle shell-maker
  :type github
  :pkgname "xenodium/shell-maker"
  :branch "main")

(el-get-bundle copilot
  :type github
  :pkgname "copilot-emacs/copilot.el"
  :branch "main")
(add-hook 'prog-mode-hook 'copilot-mode)
(defun copilot-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (indent-for-tab-command)))
(with-eval-after-load 'copilot
  (define-key copilot-mode-map (kbd "TAB") #'copilot-tab)
  (define-key copilot-mode-map [(tab)] #'copilot-tab)
  (define-key copilot-mode-map (kbd "C-TAB") #'copilot-accept-completion-by-word)
  (define-key copilot-mode-map (kbd "C-<tab>") #'copilot-accept-completion-by-word)
  (define-key copilot-mode-map (kbd "C-z n") #'copilot-next-completion)
  (define-key copilot-mode-map (kbd "C-z p") #'copilot-previous-completion))
(el-get-bundle polymode
  :type github
  :pkgname "polymode/polymode")
(el-get-bundle poly-markdown
  :type github
  :pkgname "polymode/poly-markdown")
(el-get-bundle aio
  :type github
  :pkgname "skeeto/emacs-aio")
(el-get-bundle request
  :type github
  :pkgname "tkf/emacs-request")
(el-get-bundle mcp.el
  :type github
  :pkgname "lizqwerscott/mcp.el")
(el-get-bundle copilot-chat.el
  :type github
  :pkgname "chep/copilot-chat.el"
  :depends (polymode poly-markdown aio request shell-maker mcp.el))
(setopt copilot-chat-frontend 'markdown)
(setopt copilot-chat-commit-model "claude-haiku-4.5")

(el-get-bundle llama
  :type github
  :pkgname "tarsius/llama"
  :branch "main")
(el-get-bundle elpa:seq)
(el-get-bundle cond-let
  :type github
  :pkgname "tarsius/cond-let"
  :branch "main")
(el-get-bundle transient
  :branch "main"
  :depends (seq compat cond-let))
(el-get-bundle with-editor
  :branch "main")
(el-get-bundle magit
  :type github
  :pkgname "magit/magit"
  :depends (transient with-editor compat cond-let)
  :load-path "lisp/"
  :compile "lisp/"
  :build `(("make" ,(format "EMACSBIN=%s" el-get-emacs) "lisp")
           ("touch" "lisp/magit-autoloads.el"))
  :branch "main")
(with-eval-after-load 'git-commit
  ;; It is recommended to run `git config --global commit.verbose true`
  (add-hook 'git-commit-setup-hook #'copilot-mode)
  (add-hook 'git-commit-setup-hook 'copilot-chat-insert-commit-message))

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

;; (el-get-bundle ghub
;;   :branch "main")
;; (el-get-bundle forge)
;; (ghub-request "GET" "/user" nil
;;               :forge 'github
;;               :host "api.github.com"
;;               :username "nanasess"
;;               :auth 'forge)

(with-eval-after-load 'smerge-mode
  (define-key smerge-mode-map (kbd "M-n") 'smerge-next)
  (define-key smerge-mode-map (kbd "M-p") 'smerge-prev))

(setopt howm-directory (concat external-directory "howm/"))
(setopt howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.md")
(el-get-bundle howm
  :type github
  :pkgname "kaorahi/howm"
  :build `(("./configure" ,(concat "--with-emacs=" el-get-emacs)) ("make"))
  :prepare (progn
             (defvar howm-menu-lang 'ja)
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
(with-eval-after-load 'howm
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
      (when (and file-name (string-match "\\.md" file-name))
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
  (when (executable-find "rg")
    (setq howm-view-use-grep t)
    (setq howm-view-grep-command "rg")
    (setq howm-view-grep-option "-nH --no-heading --color never")
    (setq howm-view-grep-extended-option nil)
    (setq howm-view-grep-fixed-option "-F")
    (setq howm-view-grep-expr-option nil)
    (setq howm-view-grep-file-stdin-option nil))

  (defun parse-howm-title ()
    (let* ((file-name (buffer-file-name)))
      (when (and file-name (string-match "\\.md" file-name))
        (if (save-excursion
              (goto-char (point-min))
              (re-search-forward "^Title: \\(.*\\)$" nil t))
            (match-string 1)))))

  ;; see https://stackoverflow.com/a/384346
  (defun rename-file-howm-title ()
    (interactive)
    (let ((name (buffer-name))
          (filename (buffer-file-name))
          (new-name (parse-howm-title))
          (new-filename (format "%s.md" (parse-howm-title))))
      (if (not (string-empty-p new-name))
          (if (not (string= new-filename "nil.md"))
              (if (not filename)
                  (message "Buffer '%s' is not visiting a file!" name)
                (if (get-buffer new-filename)
                    (message "A buffer named '%s' already exists!" new-name)
                  (progn
                    (rename-file filename new-filename 1)
                    (rename-buffer new-filename)
                    (set-visited-file-name new-filename)
                    (set-buffer-modified-p nil))))))))
  (add-hook 'howm-mode-hook
            #'(lambda ()
                (add-hook 'before-save-hook 'rename-file-howm-title nil 'local))))

(autoload 'howm-mode "howm" "Hitori Otegaru Wiki Modoki" t)
(autoload 'howm-create "howm" "Hitori Otegaru Wiki Modoki" t)

;;; Add howm-directory/.dir-locals
;;
;; ((nil
;;   (eval
;;    (lambda ()
;;      (when (string-match "\\.txt" (file-name-nondirectory buffer-file-name))
;;        (howm-mode)
;;        (gfm-mode))))))

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

(el-get-bundle wgrep)
(with-eval-after-load 'wgrep
  (setq wgrep-enable-key "r"))

(el-get-bundle consult
  :type github
  :pkgname "minad/consult"
  :branch "main")
(with-eval-after-load 'consult
  (setq consult-narrow-key ">")
  (setq consult-widen-key "<")
  (setq consult-preview-key "M-.")
  (global-set-key (kbd "C-;") 'consult-buffer)
  (global-set-key [remap goto-line] 'consult-goto-line)
  (global-set-key (kbd "C-M-s") 'consult-line)
  (global-set-key (kbd "C-x C-d") 'consult-dir)
  (defun consult-howm-do-ag ()
    (interactive)
    (consult-ripgrep howm-directory))
  (global-set-key (kbd "C-z s") 'consult-howm-do-ag)
  (global-set-key (kbd "C-z l") 'consult-ls-git))

(el-get-bundle marginalia
  :type github
  :pkgname "minad/marginalia"
  :branch "main")
(el-get-bundle orderless
  :type github
  :pkgname "oantolin/orderless")
(with-eval-after-load 'orderless
  ;; see https://github.com/minad/corfu/wiki#basic-example-configuration-with-orderless
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides nil))

(el-get-bundle embark
  :type github
  :pkgname "oantolin/embark")
(global-set-key (kbd "C-,") 'embark-act)
(with-eval-after-load 'consult
  (with-eval-after-load 'embark
    (require 'embark-consult)
    (define-key embark-file-map "s" #'sudo-edit)))

(el-get-bundle vertico
  :type github
  :pkgname "minad/vertico"
  :branch "main"
  :load-path ("." "extensions/")
  :compile ("vertico.el" "extensions/")
  :depends (consult marginalia orderless embark))
(add-hook 'emacs-startup-hook
          #'(lambda ()
              (vertico-mode)
              (marginalia-mode)
              (savehist-mode)
              (add-to-list 'savehist-additional-variables 'kill-ring)
              (add-to-list 'savehist-additional-variables 'log-edit-comment-ring)
              (add-to-list 'savehist-additional-variables 'search-ring)
              (add-to-list 'savehist-additional-variables 'regexp-search-ring)))

(with-eval-after-load 'vertico
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
(with-eval-after-load 'consult-tramp
  (setq consult-tramp-method "sshx"))

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
(with-eval-after-load 'markdown-mode
  (add-to-list 'auto-mode-alist '("\\.\\(markdown\\|md\\)\\'" . gfm-mode))
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-header-scaling t)
  (setq markdown-indent-on-enter 'indent-and-new-item)
  (define-key markdown-mode-map (kbd "<S-tab>") #'markdown-shifttab))

(el-get-bundle request)
(el-get-bundle spinner)
(el-get-bundle f)
(el-get-bundle ht)
(el-get-bundle flycheck)
;; (el-get-bundle treemacs
;;   :type github
;;   :pkgname "Alexander-Miller/treemacs"
;;   :load-path ("src/elisp"))

(setq x-gtk-resize-child-frames 'resize-mode)
(el-get-bundle lsp-bridge
  :type github
  :pkgname "manateelazycat/lsp-bridge"
  :depends (posframe markdown-mode yasnippet orderless))
(add-hook 'prog-mode-hook
          #'(lambda ()
              ;; (setq lsp-bridge-enable-mode-line nil)
              (global-lsp-bridge-mode)))
(with-eval-after-load 'lsp-bridge
  ;; curl -O https://releases.hashicorp.com/terraform-ls/0.32.4/terraform-ls_0.32.4_linux_amd64.zip && unzip terraform-ls_0.32.4_linux_amd64.zip
  (defun sm-try-smerge ()
    "Searches for merge conflict markers and prevents lsp-bridge-mode if found."
    (when (and (buffer-file-name)
               (save-excursion
                 (goto-char (point-min))
                 (re-search-forward "^<<<<<<< " nil t)))
      (when (bound-and-true-p lsp-bridge-mode)
        (lsp-bridge-mode -1))
      (message "lsp-bridge-mode disabled due to merge conflict markers")
      (smerge-mode 1)))

  (add-hook 'find-file-hook 'sm-try-smerge t)
  (defun lsp-bridge--mode-line-format ()
    "Compose the LSP-bridge's mode-line."
    (setq-local mode-face
                (if (lsp-bridge-epc-live-p lsp-bridge-epc-process)
                    'lsp-bridge-alive-mode-line
                  'lsp-bridge-kill-mode-line))

    (when lsp-bridge-server
      (propertize "橋"'face mode-face)))
  ;; see https://docs.astral.sh/uv/
  (setopt lsp-bridge-python-command "uv")
  (setq lsp-bridge-php-lsp-server "phpactor")
  (setq lsp-bridge-python-lsp-server "pyright")
  ;; dotnet tool install --global csharp-ls
  (setq lsp-bridge-csharp-lsp-server "csharp-ls")
  (setq acm-candidate-match-function 'orderless-flex)
  (setq lsp-bridge-enable-hover-diagnostic t)
  (setq acm-enable-doc-markdown-render t)
  (setq acm-enable-copilot t)
  (setq acm-backend-copilot-node-path "/usr/bin/node")
  (autoload 'lsp-bridge--with-file-buffer "lsp-bridge")
  (setq lsp-bridge-enable-with-tramp nil)
  (setq lsp-bridge-diagnostic-max-number 300)
  ;; (setq lsp-bridge-enable-log t)
  ;; (setq lsp-bridge-enable-debug t)
  ;; (setq lsp-bridge-signature-show-function 'lsp-bridge-signature-posframe)
  (setopt acm-enable-tabnine nil)
  (global-set-key [remap xref-find-definitions] #'lsp-bridge-find-def)
  (global-set-key [remap xref-pop-marker-stack] #'lsp-bridge-find-def-return)
  (global-set-key (kbd "M-.") #'lsp-bridge-find-def)
  (global-set-key (kbd "M-,") #'lsp-bridge-find-def-return)
  (global-set-key (kbd "M-n") #'lsp-bridge-diagnostic-jump-next)
  (global-set-key (kbd "M-p") #'lsp-bridge-diagnostic-jump-prev)
  (global-set-key (kbd "C-z i") #'lsp-bridge-diagnostic-list)
  (define-key acm-mode-map (kbd "<tab>") nil))

(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . tsx-ts-mode))
(el-get-bundle jq-mode
  :type github
  :pkgname "ljos/jq-mode")

(el-get-bundle web-mode
  :type github
  :pkgname "nanasess/web-mode"
  :branch "eccube-engine")
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(with-eval-after-load 'web-mode
  (setq web-mode-enable-block-face t)
                                        ;    (setq web-mode-enable-current-element-highlight nil)
  (setq web-mode-enable-current-column-highlight nil)
  (setq web-mode-enable-auto-indentation nil)
  (add-hook 'web-mode-hook
            #'(lambda ()
                (setq web-mode-enable-auto-indentation nil)))
  ;; (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook
            #'(lambda ()
                (when (string-equal "vue" (file-name-extension buffer-file-name))
                  (setup-tide-mode))))
  (add-hook 'web-mode-hook
            #'(lambda ()
                (when (string-equal "tpl" (file-name-extension buffer-file-name))
                  (web-mode-set-engine "eccube")))))

(el-get-bundle yaml-mode)
;; npm i -g yaml-language-server
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

(add-to-list 'auto-mode-alist '("\\.\\(inc\\|php[s34]?\\)$" . php-ts-mode))
(with-eval-after-load 'php-ts-mode
  (with-eval-after-load 'lsp-bridge
    (add-hook 'php-ts-mode-hook #'(lambda ()
                                    (push '(php-ts-mode . lsp-bridge-php-lsp-server) lsp-bridge-single-lang-server-mode-list)
                                    (lsp-bridge-mode 1))))
  (electric-indent-local-mode t)
  (electric-layout-mode t)
  ;; (setq-local electric-layout-rules '((?{ . around)))
  (electric-pair-local-mode t)
  (with-eval-after-load 'skk
    (add-to-list 'context-skk-programming-mode 'php-ts-mode)))

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
(el-get-bundle emacs-fsharp-mode
  :type github
  :pkgname "fsharp/emacs-fsharp-mode"
  :depends (jsonrpc))

(el-get-bundle haskell-mode
  :type github
  :pkgname "haskell/haskell-mode"
  ;; :info "."
  ;; :build `(("make" ,(format "EMACS=%s" el-get-emacs) "all"))
)
(with-eval-after-load 'haskell-mode
  (setq haskell-stylish-on-save t)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

(el-get-bundle dockerfile-mode)
(el-get-bundle oauth2
  :type github
  :pkgname "emacsmirror/oauth2")
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
(with-eval-after-load 'mew
  (require 'mm-version))
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
(modify-coding-system-alist 'file "\\.\\(db\\|sqlite\\)\\'" 'raw-text-unix)
(add-to-list 'auto-mode-alist '("\\.\\(db\\|sqlite\\)\\'" . sqlite-dump))

(defvar mkpasswd-command
  "head -c 10 < /dev/random | uuencode -m - | tail -n 2 |head -n 1 | head -c10")
(autoload 'mkpasswd "mkpasswd" nil t)

(el-get-bundle nginx-mode)
(el-get-bundle po-mode)
(setq auto-mode-alist (cons '("\\.po\\'\\|\\.po\\." . po-mode)
                            auto-mode-alist))

(autoload 'po-find-file-coding-system "po-compat")
(modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\."
                            'po-find-file-coding-system)

(el-get-bundle mermaid-mode
  :type github
  :pkgname "abrochard/mermaid-mode")
(with-eval-after-load 'mermaid-mode
  (setq mermaid-output-format ".pdf")
  (define-key mermaid-mode-map (kbd "TAB") 'mermaid-indent-line)
  (define-key mermaid-mode-map (kbd "<tab>") 'mermaid-indent-line))

(el-get-bundle fosi
  :type github
  :pkgname "hotoku/fosi"
  :branch "main"
  :load-path ("elisp/")
  :compile ("elisp/fosi.el"))
(autoload 'fosi "fosi" nil t)

(el-get-bundle terraform-mode)
(with-eval-after-load 'terraform-mode
  (setq terraform-format-on-save t))

(el-get-bundle ebuild-mode
  :type git
  :url "https://anongit.gentoo.org/git/proj/ebuild-mode.git"
  :build `(("make" ,(format "EMACS=%s" el-get-emacs))))

(el-get-bundle wakatime-mode)
(add-to-list 'load-path (concat user-emacs-directory ".wakatime.d"))
(load "wakatime-config" t t)
(add-hook 'emacs-startup-hook 'global-wakatime-mode)
(with-eval-after-load 'wakatime-mode
  (setopt wakatime-cli-path "/usr/bin/wakatime"))

(el-get-bundle recentf-ext)
(add-hook 'emacs-startup-hook
          #'(lambda ()
              (setopt recentf-max-saved-items 50000)
              (recentf-mode 1)))

(el-get-bundle auto-save-buffers-enhanced
  :type github
  :pkgname "kentaro/auto-save-buffers-enhanced")
(setq auto-save-buffers-enhanced-interval 30)
(setq auto-save-buffers-enhanced-save-scratch-buffer-to-file-p t)
(setq auto-save-buffers-enhanced-file-related-with-scratch-buffer
      (concat howm-directory "scratch.txt"))
(auto-save-buffers-enhanced t)
(global-set-key "\C-xas" 'auto-save-buffers-enhanced-toggle-activity)

(el-get-bundle gcmh)
(gcmh-mode 1)
(with-eval-after-load 'gcmh
  (setq gcmh-verbose t))
(define-key minibuffer-local-map (kbd "C-x C-j") 'skk-kakutei)
;; npm i -g vscode-json-languageserver
;; for json format
;; see https://qiita.com/saku/items/d97e930ffc9ca39ac976
(defun jq-format (beg end)
  (interactive "r")
  (shell-command-on-region beg end "jq ." nil t))
(setq treesit-language-source-alist
      ;; tree-sitter-php is installed by `php-ts-mode-install-parser`
      '((csharp . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))))

(el-get 'sync)
(ffap-bindings)
;; (setq epa-pinentry-mode 'loopback)
(setq gc-cons-percentage 0.1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(vertico-group-title ((t (:foreground "#788484")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(jsonrpc queue seq)))
;; (profiler-report)
;; (profiler-stop)
(setq file-name-handler-alist my/saved-file-name-handler-alist)
