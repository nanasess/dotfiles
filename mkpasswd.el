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

(defvar mkpasswd-strings "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890."
  "Source of the password strigs.")
(defvar mkpasswd-length 10
  "Password length.")

(defun mkpasswd ()
  "make password strings."
  (interactive)
  (let ((str_pass "")
	(rand_pos 0))
    (random t)
    (while (< (length str_pass) mkpasswd-length)
      (setq rand_pos (random (length mkpasswd-strings)))
      (setq str_pass (concat str_pass
			     (substring mkpasswd-strings
					rand_pos (+ rand_pos 1)))))
    (insert str_pass)
    (kill-new str_pass)))
