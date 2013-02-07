;;; helm-howm.el --- Helm completion for howm

;; Copyright (C) 2009-2011 kitokitoki
;;               2012-2030 mori_dev

;; Author: kitokitoki <mori.dev.asdf@gmail.com>
;; Keywords: helm, howm
;; Prefix: helm-howm-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Installation:

;; install requires libraries:
;; `migemo'                    http://0xcc.net/migemo/
;; `helm.el'               http://www.emacswiki.org/emacs/helm.el
;; `helm-config.el'        http://www.emacswiki.org/emacs/helm-config.el
;; `helm-match-plugin.el'  http://www.emacswiki.org/emacs/helm-match-plugin.el
;; `helm-migemo.el'        http://www.emacswiki.org/emacs/helm-migemo.el
;; `howm'                      http://howm.sourceforge.jp/index-j.html

;; `helm-howm.el'          http://github.com/kitokitoki/helm-howm (this file)

;;; Setting Sample

;; (require 'helm-howm)
;;
;; (setq helm-howm-recent-menu-number-limit 600)
;; (setq helm-howm-data-directory "/path/to/howm-directory")
;; (global-set-key (kbd "C-2") 'helm-howm-menu-command)
;; (global-set-key (kbd "C-3") 'helm-cached-howm-menu)
;;
;; (defun helm-buffers ()
;;   (interactive)
;;   (helm-other-buffer
;;    '(helm-c-source-buffers+-howm-title
;;      helm-c-source-recentf
;;      ...
;;      )
;;    "*Buffer+File*"))
;; (global-set-key (kbd "M-h") 'helm-buffers)
;;
;; or
;;
;; (setq helm-sources
;;       (list
;;         helm-c-source-buffers+-howm-title ;これを追加
;;         ;; helm-c-source-buffers はコメントアウト
;;         helm-c-source-recentf など
;;         ...
;;         ))

;;; Bug Report:
;;
;; If you have problem, send a bug report via M-x helm-howm-send-bug-report.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of helm-howm.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "helm-howm.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x helm-howm-send-bug-report and M-x insert-buffer *Backtrace*
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Japanese, please write in Japanese:-)

;; Change Log
;; 1.0.7: ファイル名ではなくタイトルを一覧表示する
;;        helm-c-source-buffers+-howm-title を追加
;; 1.0.6: 専用の helm-resume を作成
;; 1.0.5: メニューリストに検索などの項目を追加。メニューソースでの (migemo)を廃止
;; 1.0.4: アクション"Open Marked howm file", "Delete file(s)" を作成
;; 1.0.3: メニュー用のソースを新規作成
;; 1.0.2: ファイル削除、新ウィンドウで開く、新フレームで開くアクションを追加
;;        リファクタリング
;; 1.0.1: 新しいメモをつくる機能を追加, migemo 対応
;; 1.0.0: 新規作成

;;; Commentary:

;;; Code:

(require 'cl)
(require 'helm)
(require 'helm-match-plugin)
(require 'helm-migemo nil t)
(require 'howm)
(require 'howm-menu)

(defvar helm-howm-recent-menu-number-limit 10)
(defvar helm-howm-persistent-action-buffer "*howm-tmp*")
(defvar helm-howm-menu-buffer "*helm-howm-menu*")
(defvar helm-howm-default-title "")
(defvar helm-howm-data-directory "/path/to/howm-data-directory")
(defvar helm-howm-use-migemo nil)

;;; Version

(defconst helm-howm-version "1.0.8"
  "The version number of the file helm-howm.el.")

(defun helm-howm-version (&optional here)
  "Show the helm-howm version in the echo area.
With prefix arg HERE, insert it at point."
  (interactive "P")
  (let ((version (format "helm-howm version %s" helm-howm-version)))
    (message version)
    (if here
      (insert version))))


(defvar helm-c-howm-recent
  '((name . "最近のメモ")
    (init . (lambda ()
              (with-current-buffer (helm-candidate-buffer 'global)
                (insert (mapconcat 'identity
                                   (helm-howm-get-recent-title-list
                                    (howm-recent-menu helm-howm-recent-menu-number-limit))
                                   "\n")))))
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (action .
      (("Open howm file(s)" . helm-howm-find-files)
       ("Open howm file in other window" .
          (lambda (candidate)
            (find-file-other-window
             (helm-howm-select-file-by-title candidate))))
       ("Open howm file in other frame" .
          (lambda (candidate)
            (find-file-other-frame
             (helm-howm-select-file-by-title candidate))))
       ("Create new memo" .
          (lambda (template)
            (helm-howm-create-new-memo "")))
       ("Create new memo on region" .
          (lambda (template)
            (helm-howm-create-new-memo (helm-howm-set-selected-text))))
       ("Delete file(s)" . helm-howm-delete-marked-files)))
    (persistent-action . helm-howm-persistent-action)
    (cleanup .
      (lambda ()
        (helm-aif (get-buffer helm-howm-persistent-action-buffer)
          (kill-buffer it))))))

(when helm-howm-use-migemo
  (push '(migemo) helm-c-howm-recent))

(defun helm-howm-persistent-action (candidate)
  (let ((buffer (get-buffer-create helm-howm-persistent-action-buffer)))
      (with-current-buffer buffer
        (erase-buffer)
        (insert-file-contents (helm-howm-select-file-by-title candidate))
        (goto-char (point-min)))
      (pop-to-buffer buffer)
      (howm-mode t)))

(defun helm-howm-select-file-by-title (title)
  (loop for recent-menu-x in (howm-recent-menu helm-howm-recent-menu-number-limit)
        for list-item-file  = (first recent-menu-x)
        for list-item-name  = (second recent-menu-x)
        if (string-equal title list-item-name)
          return list-item-file))

(defun helm-howm-find-files (candidate)
  (helm-aif (helm-marked-candidates)
      (dolist (i it)
        (find-file (helm-howm-select-file-by-title i)))
    (find-file (helm-howm-select-file-by-title candidate))))

(defun helm-howm-get-recent-title-list (recent-menu-list)
  (loop for recent-menu-x in recent-menu-list
        for list-item-name  = (second recent-menu-x)
        collect list-item-name))

(defun helm-howm-create-new-memo (text)
  (let (memo-text str
        (cbuf (current-buffer)))
    (setq str text)
    (howm-create-file-with-title helm-howm-default-title nil nil nil nil)
    (save-excursion
      (goto-char (point-max))
      (insert str))
    (goto-char (point-min))
    (end-of-line)))

(defun helm-howm-delete-marked-files (candidate)
  (helm-aif (helm-marked-candidates)
      (if (y-or-n-p (format "Delete *%s Files " (length it)))
          (progn
            (dolist (i it)
              (set-text-properties 0 (length i) nil i)
              (delete-file
                (helm-howm-select-file-by-title i)))
            (message "%s Files deleted" (length it)))
          (message "(No deletions performed)"))
    (set-text-properties 0 (length candidate) nil candidate)
    (if (y-or-n-p
         (format "Really delete file `%s' " (helm-howm-select-file-by-title candidate)))
        (progn
          (delete-file
            (helm-howm-select-file-by-title candidate))
          (message "1 file deleted"))
        (message "(No deletions performed)"))))

(defun helm-howm-set-selected-text ()
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    ""))

(defvar helm-howm-menu-list
      '(("c [メモを作成]" . "(helm-howm-create-new-memo \"\")")
        ("cr[リージョンからメモを作成]" . "(helm-howm-create-new-memo (helm-howm-set-selected-text))")
        ("s [固定]" . "(howm-list-grep-fixed)")
        ("g [正規]" . "(howm-list-grep)")
        ("m [roma]" . "(howm-list-migemo)")
        ("y [予定]" . "(howm-list-todo)")
        ("t [Todo]" . "(howm-list-schedule)")))

(defvar helm-c-source-howm-menu
  '((name . "メニュー")
    (candidates . helm-howm-menu-list)
    (type . sexp)))

(defun helm-cached-howm-menu ()
  (interactive)
  (let ((helm-display-function 'helm-howm-display-buffer))
    (if (get-buffer helm-howm-menu-buffer)
        (helm-resume helm-howm-menu-buffer)
      (helm-howm-menu-command))))

(defun helm-howm-menu-command ()
  (interactive)
  (let ((helm-display-function 'helm-howm-display-buffer))
    (helm-other-buffer
     '(helm-c-source-howm-menu
       helm-c-howm-recent)
     helm-howm-menu-buffer)))

(defun helm-howm-resume ()
  (interactive)
  (when (get-buffer helm-howm-menu-buffer)
    (helm-resume helm-howm-menu-buffer)))

(defun helm-howm-display-buffer (buf)
  "左右分割で表示する"
  (delete-other-windows)
  (split-window (selected-window) nil t)
  (pop-to-buffer buf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; howm のファイルは日付形式のため，複数開いていると見分けにくい。
;; helm-c-source-buffers+-howm-title では、一覧時にタイトルを表示する

(defvar helm-c-source-buffers+-howm-title
  '((name . "Buffers")
    (candidates . helm-c-buffer-list)
    (real-to-display . helm-howm-title-real-to-display)
    (type . buffer)
    (candidate-transformer
         helm-c-skip-current-buffer
         helm-c-highlight-buffers
         helm-c-skip-boring-buffers)
    (persistent-action . helm-c-buffers+-persistent-action)
    (persistent-help . "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))

;; helm-c-buffers-persistent-kill and helm-c-switch-to-buffer are defined at helm-config.el.
(defun helm-c-buffers+-persistent-action (candidate)
  (if current-prefix-arg
      (helm-c-buffers-persistent-kill candidate)
    (helm-c-switch-to-buffer candidate)))

(defun helm-howm-title-real-to-display (file-name)
  (with-current-buffer (get-buffer file-name)
    (if howm-mode
      (helm-howm-title-get-title file-name)
    file-name)))

(defun helm-howm-title-get-title (buffer)
  (with-current-buffer buffer
    (let ((point (point-min)))
      (save-excursion
        (goto-char (point-min))
        (end-of-line)
        (buffer-substring point (point))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; e.x, (global-set-key (kbd "C-c e") (helm-howm-fixed-term-command "emacs"))
(defun helm-howm-fixed-term-command (initial)
  (lexical-let ((initial initial))
    (lambda () (interactive) (helm 'helm-c-source-howm-recent initial))))

;; experimental code
;(helm-howm-get-filename (list howm-directory))
(defun helm-howm-get-filename (file-list)
    (loop for x in file-list
          with path-list = nil
          when (file-directory-p x)
            for path-list =
              (append
                (helm-howm-get-filename
                 (remove-if
                  (lambda(y) (string-match "\\.$\\|\\.svn" y))
                  (directory-files x t)))
                path-list)
          else
            collect x into path-list
          end
          finally return path-list))

(defvar helm-c-source-howm-contents-grep
  `((name . "helm-howm-contents-grep")
    (grep-candidates . ,(helm-howm-get-filename (list howm-directory)))
    (header-name . (lambda (x) (concat x ": " helm-pattern)))
    (candidate-number-limit . 99999)))
;; (helm 'helm-c-source-howm-contents-grep)

;;;; Bug report
(defvar helm-howm-maintainer-mail-address
  (concat "mor" "i.dev.asdf@gm" "ail.com"))
(defvar helm-howm-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of helm-howm.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"helm-howm.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defun helm-howm-send-bug-report ()
  (interactive)
  (reporter-submit-bug-report
   helm-howm-maintainer-mail-address
   "helm-howm.el"
   (apropos-internal "^eldoc-" 'boundp)
   nil nil
   helm-howm-bug-report-salutation))

(provide 'helm-howm)
