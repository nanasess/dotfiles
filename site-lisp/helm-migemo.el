;;; helm-migemo.el --- Migemo plug-in for helm

;; Copyright (C) 2007-2012 rubikitch
;; Copyright (C) 2012 Yuhei Maeda <yuhei.maeda_at_gmail.com>
;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Maintainer: Yuhei Maeda <yuhei.maeda_at_gmail.com>
;; Version: 1.19
;; Package-version: 1.19
;; Package-Requires: ((helm "20120811"))
;; Created: 2009-04-13 
;; Keywords: matching, convenience, tools, i18n
;; URL: https://github.com/myuhe/helm-migemo.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Migemo extension of `helm'. Use `helm-migemo' instead of
;; `helm'. If `helm-migemo' is invoked with prefix argument,
;; `helm' is migemo-ized. This means that pattern matching of
;; `helm' candidates is done by migemo-expanded `helm-pattern'.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `helm-migemo'
;;    `helm' with migemo extension.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;; If you want to use migemo search source-locally, add (migemo) to
;; the source. It sets match and search attribute appropriately for
;; migemo.

;;; Setting:

;; (require 'helm-migemo)
;; (define-key global-map [(control ?:)] 'helm-migemo)

;;; Bug:

;; Simultaneous use of (candidates-in-buffer), (search
;; . migemo-forward) and (delayed) scrambles *helm* buffer. Maybe
;; because of collision of `migemo-process' and `run-with-idle-timer'

;;; History:

;; $Log: helm-migemo.el,v $

;; Revision 1.19  2012-08-18 02:16:22  myuhe
;; Port to helm.
;;
;; Revision 1.18  2009-06-07 17:52:22  rubikitch
;; New macro `helm-migemize-command'.
;;
;; Revision 1.17  2009/06/04 20:32:00  rubikitch
;; migemo is soft-required now; this file has no effect unless migemo is installed.
;;
;; Revision 1.16  2008/10/03 20:43:18  rubikitch
;; Use with helm-match-plugin.el
;;
;; Revision 1.15  2008/10/03 20:01:46  rubikitch
;; refactoring
;;
;; Revision 1.14  2008/08/25 08:29:02  rubikitch
;; `helm-migemo': helm-args
;;
;; Revision 1.13  2008/08/24 20:39:53  rubikitch
;; prevent the unit test from being byte-compiled.
;;
;; Revision 1.12  2008/08/24 18:01:25  rubikitch
;; *** empty log message ***
;;
;; Revision 1.11  2008/08/24 08:23:30  rubikitch
;; Rename `helm-candidates-buffer' -> `helm-candidate-buffer'
;;
;; Revision 1.10  2008/08/24 01:54:21  rubikitch
;; migemo attribute
;;
;; Revision 1.9  2008/08/19 21:38:09  rubikitch
;; match attribute bug fix
;;
;; Revision 1.8  2008/08/19 21:30:29  rubikitch
;; plug-in
;;
;; Revision 1.7  2008/08/10 22:45:02  rubikitch
;; Bug info
;;
;; Revision 1.6  2008/08/08 03:40:51  rubikitch
;; require migemo
;;
;; Revision 1.5  2008/08/08 03:38:34  rubikitch
;; add search attribute
;; unit tests
;;
;; Revision 1.4  2007/12/26 08:36:01  rubikitch
;; changed match priority
;;
;; Revision 1.3  2007/12/25 19:55:59  rubikitch
;; patch is not needed anymore.
;;
;; Revision 1.2  2007/12/25 13:05:46  rubikitch
;; speed up by memoization
;;
;; Revision 1.1  2007/12/25 12:03:25  rubikitch
;; Initial revision
;;

;;; Code:

(eval-when-compile (require 'helm))
(require 'migemo nil t)
(require 'helm-match-plugin nil t)
(defvar helm-use-migemo nil
  "[Internal] If non-nil, `helm' is migemo-ized.")
(defun helm-migemo (with-migemo &rest helm-args)
  "`helm' with migemo extension.
With prefix arugument, `helm-pattern' is migemo-ized, otherwise normal `helm'."
  (interactive "P")
  (let ((helm-use-migemo with-migemo))
    (apply 'helm helm-args)))

(defvar helm-previous-migemo-info '("" . "")
  "[Internal] Previous migemo query for helm-migemo.")
(defun* helm-string-match-with-migemo (str &optional (pattern helm-pattern))
  "Migemo version of `string-match'."
  (unless (string= pattern (car helm-previous-migemo-info))
    (setq helm-previous-migemo-info (cons pattern (migemo-get-pattern pattern))))
  (string-match (cdr helm-previous-migemo-info) str))

(when (memq 'helm-compile-source--match-plugin helm-compile-source-functions)
  (defun* helm-mp-3migemo-match (str &optional (pattern helm-pattern))
    (loop for (pred . re) in (helm-mp-3-get-patterns pattern)
          always (funcall pred (helm-string-match-with-migemo str re))))
  (defun helm-mp-3migemo-search (pattern &rest ignore)
    (helm-mp-3-search-base migemo-forward migemo-forward bol eol))
  (defun helm-mp-3migemo-search-backward (pattern &rest ignore)
    (helm-mp-3-search-base migemo-backward migemo-backward eol bol)))
;; (helm-string-match-with-migemo "日本語入力" "nihongo")
;; (helm-string-match-with-migemo "日本語入力" "nyuuryoku")
;; (helm-mp-3migemo-match "日本語入力" "nihongo nyuuryoku")
(defun helm-compile-source--migemo (source)
  (if (not (featurep 'migemo))
      source
    (let* ((match-identity-p 
            (or (assoc 'candidates-in-buffer source)
                (equal '(identity) (assoc-default 'match source))))
           (use-match-plugin
            (memq 'helm-compile-source--match-plugin helm-compile-source-functions))
           (matcher (if use-match-plugin
                        'helm-mp-3migemo-match
                      'helm-string-match-with-migemo))
           (searcher (if (assoc 'search-from-end source)
                         (if use-match-plugin
                             'helm-mp-3migemo-search-backward
                           'migemo-backward)
                       (if use-match-plugin
                           'helm-mp-3migemo-search
                         'migemo-forward))))
      (cond (helm-use-migemo
             `((delayed)
               (search ,@(assoc-default 'search source) ,searcher)
               ,(if match-identity-p
                    '(match identity)
                  `(match ,matcher
                          ,@(assoc-default 'match source)))
               ,@source))
            ((assoc 'migemo source)
             `((search ,searcher)
               ,(if match-identity-p
                    '(match identity)
                  `(match ,matcher))
               ,@source))
            (t source)))))
(add-to-list 'helm-compile-source-functions 'helm-compile-source--migemo t)

(defvar helm-migemize-command-idle-delay 0.1
  "`helm-idle-delay' for migemized command.")
(defmacro helm-migemize-command (command)
  "Use migemo in COMMAND when selectiong candidate by `helm'.
Bind `helm-use-migemo' = t in COMMAND.
`helm-migemize-command-idle-delay' is used instead of  `helm-idle-delay'."
  `(defadvice ,command (around helm-use-migemo activate)
     (let ((helm-use-migemo t)
           (helm-idle-delay helm-migemize-command-idle-delay))
       ad-do-it)))


(provide 'helm-migemo)

;; How to save (DO NOT REMOVE!!)
;; (progn (magit-push) (emacswiki-post "helm-migemo.el"))
;;; helm-migemo.el ends here
