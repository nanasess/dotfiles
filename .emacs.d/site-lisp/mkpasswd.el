;;; mkpasswd.el --- Password Generator

;; Copyright (C) 2010 Kentaro Ohkouchi <nanasess@fsm.ne.jp>

;; Author: Kentaro Ohkouchi <nanasess@fsm.ne.jp>
;; Created: 2010-10-17

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;;; Code:

(defun mkpasswd ()
  "make password strings."
  (interactive)
  (let ((buf (get-buffer-create " *mkpasswd*")) ret)
    (call-process-shell-command mkpasswd-command nil buf nil)
    (with-current-buffer buf (setq ret (buffer-string)))
    (kill-buffer buf)
    (insert ret)
    (kill-new ret)))
