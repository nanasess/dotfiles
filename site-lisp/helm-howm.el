;;; anything-howm.el --- Helm completion for howm

;; Copyright (C) 2009-2011 kitokitoki
;;               2012-2030 mori_dev

;; Author: kitokitoki <mori.dev.asdf@gmail.com>
;; Keywords: helm, howm
;; Prefix: ah:

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
;; (setq ah:recent-menu-number-limit 600)
;; (global-set-key (kbd "C-2") 'ah:menu-command)
;; (global-set-key (kbd "C-3") 'ah:cached-howm-menu)
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

;; Change Log
;; 1.1.0: リファクタ helm-c-source-howm-recent の内部の無名関数に名前を付与
;; 1.0.9: prefix を helm-howm- から ah: へ変更
;; 1.0.8: 拡張子 .homn での判定処理を
;;        howm-directory 以下の howm-mode かに変更
;;        migemo をオプション化
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

(defvar ah:recent-menu-number-limit 10)
(defvar ah:persistent-action-buffer "*howm-tmp*")
(defvar ah:menu-buffer "*helm-howm-menu*")
(defvar ah:default-title "")
(defvar ah:use-migemo nil)

(defvar ah:howm-full-path-directory (expand-file-name howm-directory))


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

(defun helm-migemo-match-fn (candidate)
  (or (string-match (regexp-quote helm-input) candidate)
      (string-match helm-pattern candidate)))

(defvar helm-c-source-howm-recent
  '((name    . "最近のメモ")
    (init    . helm-c-howm-recent-init)
    (candidates-in-buffer)
    (candidates . helm-candidates-in-buffer)
    (volatile)
    (match helm-migemo-match-fn)
    (candidate-number-limit . 10000000)
    (action .
      (("Open howm file(s)" . ah:find-files)
       ("Open howm file in other window" .
          (lambda (candidate)
            (find-file-other-window
             (ah:select-file-by-title candidate))))
       ("Open howm file in other frame" .
          (lambda (candidate)
            (find-file-other-frame
             (ah:select-file-by-title candidate))))
       ("Create new memo" .
          (lambda (template)
            (ah:create-new-memo "")))
       ("Create new memo on region" .
          (lambda (template)
            (ah:create-new-memo (ah:set-selected-text))))
       ("Delete file(s)" . ah:delete-marked-files)))
    (persistent-action . helm-howm-persistent-action)
    (cleanup . helm-c-howm-recent-cleanup)))

(defun helm-c-howm-recent-init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (insert (mapconcat 'identity
                       (ah:get-recent-title-list
                        (howm-recent-menu ah:recent-menu-number-limit))
                       "\n"))))

(defun helm-c-howm-recent-cleanup ()
  (helm-aif (get-buffer ah:persistent-action-buffer)
      (kill-buffer it)))

(defun helm-howm-persistent-action (candidate)
  (let ((buffer (get-buffer-create ah:persistent-action-buffer)))
      (with-current-buffer buffer
        (erase-buffer)
        (insert-file-contents (ah:select-file-by-title candidate))
        (goto-char (point-min)))
      (pop-to-buffer buffer)
      (howm-mode t)))

(when ah:use-migemo
  (push '(migemo) helm-c-source-howm-recent))

(defun ah:select-file-by-title (title)
  (loop for recent-menu-x in (howm-recent-menu ah:recent-menu-number-limit)
        for list-item-file  = (first recent-menu-x)
        for list-item-name  = (second recent-menu-x)
        if (string-equal title list-item-name)
          return list-item-file))

(defun ah:find-files (candidate)
  (helm-aif (helm-marked-candidates)
      (dolist (i it)
        (find-file (ah:select-file-by-title i)))
    (find-file (ah:select-file-by-title candidate))))

(defun ah:get-recent-title-list (recent-menu-list)
  (loop for recent-menu-x in recent-menu-list
        for list-item-name  = (second recent-menu-x)
        collect list-item-name))

(defun ah:create-new-memo (text)
  (let (memo-text str
        (cbuf (current-buffer)))
    (setq str text)
    (howm-create-file-with-title ah:default-title nil nil nil nil)
    (save-excursion
      (goto-char (point-max))
      (insert str))
    (goto-char (point-min))
    (end-of-line)))

(defun ah:delete-marked-files (candidate)
  (helm-aif (helm-marked-candidates)
      (if (y-or-n-p (format "Delete *%s Files " (length it)))
          (progn
            (dolist (i it)
              (set-text-properties 0 (length i) nil i)
              (delete-file
                (ah:select-file-by-title i)))
            (message "%s Files deleted" (length it)))
          (message "(No deletions performed)"))
    (set-text-properties 0 (length candidate) nil candidate)
    (if (y-or-n-p
         (format "Really delete file `%s' " (ah:select-file-by-title candidate)))
        (progn
          (delete-file
            (ah:select-file-by-title candidate))
          (message "1 file deleted"))
        (message "(No deletions performed)"))))

(defun ah:set-selected-text ()
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    ""))

(defvar ah:menu-list
      '(("c [メモを作成]" . "(ah:create-new-memo \"\")")
        ("cr[リージョンからメモを作成]" . "(ah:create-new-memo (ah:set-selected-text))")
        ("s [固定]" . "(howm-list-grep-fixed)")
        ("g [正規]" . "(howm-list-grep)")
        ("m [roma]" . "(howm-list-migemo)")
        ("y [予定]" . "(howm-list-todo)")
        ("t [Todo]" . "(howm-list-schedule)")))

(defvar helm-c-source-howm-menu
  '((name . "メニュー")
    (candidates . ah:menu-list)
    (type . sexp)))

(defun ah:cached-howm-menu ()
  (interactive)
  (let ((helm-display-function 'ah:display-buffer))
    (if (get-buffer ah:menu-buffer)
        (helm-resume ah:menu-buffer)
      (ah:menu-command))))


(defun ah:menu-command ()
  (interactive)
  (let ((helm-display-function 'ah:display-buffer))
    (helm-other-buffer
     '(helm-c-source-howm-menu
       helm-c-source-howm-recent)
     ah:menu-buffer)))

(defun ah:resume ()
  (interactive)
  (when (get-buffer ah:menu-buffer)
    (helm-resume ah:menu-buffer)))

(defun ah:display-buffer (buf)
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
    (real-to-display . ah:title-real-to-display)
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

(defun ah:title-real-to-display (file-name)
  (with-current-buffer (get-buffer file-name)
    (if (and howm-mode
             (ah:in-howm-dir-p file-name))
      (ah:title-get-title file-name)
    file-name)))

(defun ah:in-howm-dir-p (file-name)
  (ah:!! (string-match ah:howm-full-path-directory
                       (buffer-file-name (get-buffer file-name)))))

(defun ah:!! (arg)
  (not (not arg)))

(defun ah:title-get-title (buffer)
  (with-current-buffer buffer
    (let ((point (point-min)))
      (save-excursion
        (goto-char (point-min))
        (end-of-line)
        (buffer-substring point (point))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; e.x, (global-set-key (kbd "C-c e") (helm-howm-fixed-term-command "emacs"))
(defun ah:fixed-term-command (initial)
  (lexical-let ((initial initial))
    (lambda () (interactive) (helm 'helm-c-source-howm-recent initial))))


;; experimental code
;(ah:get-filename (list howm-directory))
(defun ah:get-filename (file-list)
    (loop for x in file-list
          with path-list = nil
          when (file-directory-p x)
            for path-list =
              (append
                (ah:get-filename
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
    (grep-candidates . ,(ah:get-filename (list howm-directory)))
    (header-name . (lambda (x) (concat x ": " helm-pattern)))
    (candidate-number-limit . 99999)))
;; (helm 'helm-c-source-howm-contents-grep)

(provide 'helm-howm)
