;;; japanese-holidays.el --- calendar functions for the Japanese calendar

;; Copyright (C) 1999 Takashi Hattori <hattori@sfc.keio.ac.jp>
;; Copyright (C) 2005 Hiroya Murata <lapis-lazuli@pop06.odn.ne.jp>

;; Author: Takashi Hattori <hattori@sfc.keio.ac.jp>
;;	Hiroya Murata <lapis-lazuli@pop06.odn.ne.jp>
;; Keywords: calendar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Original program created by T. Hattori 1999/4/20

;; このプログラムは、calender で表示出来る様に日本の祝日を設定します。
;; 使用するには、このファイルを load-path の通った所に置き、~/.emacs に
;; 以下の設定を追加します。

;;  (add-hook 'calendar-load-hook
;;            (lambda ()
;;              (require 'japanese-holidays)
;;              (setq calendar-holidays
;;                    (append japanese-holidays local-holidays other-holidays))))
;;  (setq mark-holidays-in-calendar t)

;; “きょう”をマークするには以下の設定を追加します。
;;  (add-hook 'today-visible-calendar-hook 'calendar-mark-today)

;; 日曜日を赤字にする場合、以下の設定を追加します。
;;  (setq calendar-weekend-marker 'diary)
;;  (add-hook 'today-visible-calendar-hook 'calendar-mark-weekend)
;;  (add-hook 'today-invisible-calendar-hook 'calendar-mark-weekend)

;;; Code:
;;

(eval-when-compile
  (require 'cl)
  (defvar displayed-month)
  (defvar displayed-year)
  (when noninteractive
    (require 'holidays)))

(autoload 'solar-equinoxes/solstices "solar")

(defcustom japanese-holidays
  '(;; 明治6年太政官布告第344号
    (holiday-range
     (holiday-fixed 1 3 "元始祭") '(10 14 1873) '(7 20 1948))
    (holiday-range
     (holiday-fixed 1 5 "新年宴会") '(10 14 1873) '(7 20 1948))
    (holiday-range
     (holiday-fixed 1 30 "孝明天皇祭") '(10 14 1873) '(9 3 1912))
    (holiday-range
     (holiday-fixed 2 11 "紀元節") '(10 14 1873) '(7 20 1948))
    (holiday-range
     (holiday-fixed 4 3 "神武天皇祭") '(10 14 1873) '(7 20 1948))
    (holiday-range
     (holiday-fixed 9 17 "神嘗祭") '(10 14 1873) '(7 5 1879))
    (holiday-range
     (holiday-fixed 11 3 "天長節") '(10 14 1873) '(9 3 1912))
    (holiday-range
     (holiday-fixed 11 23 "新嘗祭") '(10 14 1873) '(7 20 1948))
    ;; 明治11年太政官布告23号
    (let* ((equinox (solar-equinoxes/solstices 0 displayed-year))
	   (m (extract-calendar-month equinox))
	   (d (truncate (extract-calendar-day equinox))))
      (holiday-range
       (holiday-fixed m d "春季皇霊祭") '(6 5 1878) '(7 20 1948)))
    (let* ((equinox (solar-equinoxes/solstices 2 displayed-year))
	   (m (extract-calendar-month equinox))
	   (d (truncate (extract-calendar-day equinox))))
      (holiday-range
       (holiday-fixed m d "秋季皇霊祭") '(6 5 1878) '(7 20 1948)))
    ;; 明治12年太政官布告27号
    (holiday-range
     (holiday-fixed 10 17 "神嘗祭") '(7 5 1879) '(7 20 1948))
    ;; 休日ニ関スル件 (大正元年勅令第19号)
    (holiday-range
     (holiday-fixed 7 30 "明治天皇祭") '(9 3 1912) '(3 3 1927))
    (holiday-range
     (holiday-fixed 8 31 "天長節") '(9 3 1912) '(3 3 1927))
    ;; 大正2年勅令259号
    (holiday-range
     (holiday-fixed 10 31 "天長節祝日") '(10 31 1913) '(3 3 1927))
    ;; 休日ニ関スル件改正ノ件 (昭和2年勅令第25号)
    (holiday-range
     (holiday-fixed 4 29 "天長節") '(3 3 1927) '(7 20 1948))
    (holiday-range
     (holiday-fixed 11 3 "明治節") '(3 3 1927) '(7 20 1948))
    (holiday-range
     (holiday-fixed 12 25 "大正天皇祭") '(3 3 1927) '(7 20 1948))
    ;; 国民の祝日に関する法律の一部を改正する法律 (昭和60年法律第103号)
    (holiday-national
     ;; 国民の祝日に関する法律の一部を改正する法律 (昭和48年法律第10号)
     (holiday-substitute
      (nconc
       ;; 国民の祝日に関する法律 (昭和23年法律第178号)
       (holiday-range
	(holiday-fixed 1 1 "元日") '(7 20 1948))
       (holiday-range
	(holiday-fixed 1 15 "成人の日") '(7 20 1947) '(1 1 2000))
       (let* ((equinox (solar-equinoxes/solstices 0 displayed-year))
	      (m (extract-calendar-month equinox))
	      (d (truncate (extract-calendar-day equinox))))
	 ;; 春分の日は、厳密には前年2月の官報により決定される
	 (holiday-range
	  (holiday-fixed m d "春分の日") '(7 20 1948)))
       (holiday-range
	(holiday-fixed 4 29 "天皇誕生日") '(7 20 1948) '(2 17 1989))
       (holiday-range
	(holiday-fixed 5 3 "憲法記念日") '(7 20 1948))
       (holiday-range
	(holiday-fixed 5 5 "こどもの日") '(7 20 1948))
       (let* ((equinox (solar-equinoxes/solstices 2 displayed-year))
	      (m (extract-calendar-month equinox))
	      (d (truncate (extract-calendar-day equinox))))
	 ;; 秋分の日は、厳密には前年2月の官報により決定される
	 (holiday-range
	  (holiday-fixed m d "秋分の日") '(7 20 1948)))
       (holiday-range
	(holiday-fixed 11 3 "文化の日") '(7 20 1948))
       (holiday-range
	(holiday-fixed 11 23 "勤労感謝の日") '(7 20 1948))
       ;; 国民の祝日に関する法律の一部を改正する法律 (昭和41年法律第86号)
       ;;   建国記念の日となる日を定める政令 (昭和41年政令第376号)
       (holiday-range
	(holiday-fixed 2 11 "建国記念の日") '(6 25 1966))
       (holiday-range
	(holiday-fixed 9 15 "敬老の日") '(6 25 1966) '(1 1 2003))
       (holiday-range
	(holiday-fixed 10 10 "体育の日") '(6 25 1966) '(1 1 2000))
       ;; 国民の祝日に関する法律の一部を改正する法律 (平成元年法律第5号)
       (holiday-range
	(holiday-fixed 4 29 "みどりの日") '(2 17 1989) '(1 1 2007))
       (holiday-range
	(holiday-fixed 12 23 "天皇誕生日") '(2 17 1989))
       ;; 国民の祝日に関する法律の一部を改正する法律 (平成7年法律第22号)
       (holiday-range
	(holiday-fixed 7 20 "海の日") '(1 1 1996) '(1 1 2003))
       ;; 国民の祝日に関する法律の一部を改正する法律 (平成10年法律第141号)
       (holiday-range
	(holiday-float 1 1 2 "成人の日") '(1 1 2000))
       (holiday-range
	(holiday-float 10 1 2 "体育の日") '(1 1 2000))
       ;; 国民の祝日に関する法律及び老人福祉法の一部を改正する法律 (平成13年法律第59号)
       (holiday-range
	(holiday-float 7 1 3 "海の日") '(1 1 2003))
       (holiday-range
	(holiday-float 9 1 3 "敬老の日") '(1 1 2003))
       ;; 国民の祝日に関する法律の一部を改正する法律 (平成17年法律第43号)
       (holiday-range
	(holiday-fixed 4 29 "昭和の日") '(1 1 2007))
       (holiday-range
	(holiday-fixed 5 4 "みどりの日") '(1 1 2007)))))
    (filter-visible-calendar-holidays
     '(;; 皇太子明仁親王の結婚の儀の行われる日を休日とする法律 (昭和34年法律第16号)
       ((4 10 1959) "明仁親王の結婚の儀")
       ;; 昭和天皇の大喪の礼の行われる日を休日とする法律 (平成元年法律第4号)
       ((2 24 1989) "昭和天皇の大喪の礼")
       ;; 即位礼正殿の儀の行われる日を休日とする法律 (平成2年法律第24号)
       ((11 12 1990) "即位礼正殿の儀")
       ;; 皇太子徳仁親王の結婚の儀の行われる日を休日とする法律 (平成5年法律第32号)
       ((6 9 1993) "徳仁親王の結婚の儀"))))
  "*Japanese holidays.
See the documentation for `calendar-holidays' for details."
  :type 'sexp
  :group 'holidays)

(defcustom holiday-substitute-name "振替休日"
  "*Name of substitute holiday."
  :type 'string
  :group 'holidays)

(defcustom holiday-national-name "国民の休日"
  "*Name of national holiday."
  :type 'string
  :group 'holidays)

(eval-and-compile
  (defun holiday-make-sortable (date)
    (+ (* (nth 2 date) 10000) (* (nth 0 date) 100) (nth 1 date))))

(defun holiday-range (holidays &optional from to)
  (let ((from (and from (holiday-make-sortable from)))
	(to   (and to   (holiday-make-sortable to))))
    (delq nil
	  (mapcar
	   (lambda (holiday)
	     (let ((date (holiday-make-sortable (car holiday))))
	       (when (and (or (null from) (<= from date))
			  (or (null to) (< date to)))
		 holiday)))
	   holidays))))

(defun holiday-find-date (date holidays)
  (let ((sortable-date (holiday-make-sortable date))
	matches)
    (dolist (holiday holidays)
      (when (= sortable-date (holiday-make-sortable (car holiday)))
	(setq matches (cons holiday matches))))
    matches))

(defun holiday-add-days (date days)
  (calendar-gregorian-from-absolute
   (+ (calendar-absolute-from-gregorian date) days)))

(defun holiday-subtract-date (from other)
  (- (calendar-absolute-from-gregorian from)
     (calendar-absolute-from-gregorian other)))

(defun holiday-substitute (holidays)
  (let (substitutes substitute)
    (dolist (holiday holidays)
      (let ((date (car holiday)))
	(when (and (>= (holiday-make-sortable date)
		       (eval-when-compile
			 (holiday-make-sortable '(4 12 1973))))
		   (= (calendar-day-of-week date) 0))
	  (setq substitutes
		(cons
		 (list (holiday-add-days date 1)
		       (format "%s (%s)"
			       holiday-substitute-name
			       (cadr holiday)))
		 substitutes)))))
    (when (setq substitutes
		(filter-visible-calendar-holidays substitutes))
      (setq substitutes (sort substitutes
			      (lambda (l r)
				(< (holiday-make-sortable (car l))
				   (holiday-make-sortable (car r))))))
      (while (setq substitute (car substitutes))
	(setq substitutes (cdr substitutes))
	(if (holiday-find-date (car substitute) holidays)
	    (let* ((date (car substitute))
		   (sortable-date (holiday-make-sortable date)))
	      (when (>= sortable-date
			(eval-when-compile
			  (holiday-make-sortable '(1 1 2007))))
		(setq substitutes
		      (cons
		       (list (holiday-add-days date 1) (cadr substitute))
		       substitutes))))
	  (setq holidays (cons substitute holidays)))))
    (filter-visible-calendar-holidays holidays)))

(defun holiday-national (holidays)
  (when holidays
    (setq holidays (sort holidays
			 (lambda (l r)
			   (< (holiday-make-sortable (car l))
			      (holiday-make-sortable (car r))))))
    (let* ((rest holidays)
	   (curr (pop rest))
	   prev nationals)
      (while (setq prev curr
		   curr (pop rest))
	(when (= (holiday-subtract-date (car curr) (car prev)) 2)
	  (let* ((date (holiday-add-days (car prev) 1))
		 (sotable-date (holiday-make-sortable date)))
	    (when (cond
		   ((>= sotable-date
			(eval-when-compile
			  (holiday-make-sortable '(1 1 2007))))
		    (catch 'found
		      (dolist (holiday (holiday-find-date date holidays))
			(unless (string-match
				 (regexp-quote holiday-substitute-name)
				 (cadr holiday))
			  (throw 'found nil)))
		      t))
		   ((>= sotable-date
			(eval-when-compile
			  (holiday-make-sortable '(12 27 1985))))
		    (not (or (= (calendar-day-of-week date) 0)
			     (holiday-find-date date holidays)))))
	      (setq nationals (cons (list date holiday-national-name)
				    nationals))))))
      (setq holidays (nconc holidays
			    (filter-visible-calendar-holidays nationals)))))
  holidays)

(defvar calendar-weekend '(0)
  "*List of days of week to be marked as holiday.")

(defvar calendar-weekend-marker nil)

(defun calendar-mark-weekend ()
  (let ((m displayed-month)
	(y displayed-year))
    (increment-calendar-month m y -1)
    (calendar-for-loop
     i from 1 to 3 do
     (let ((sunday (- 1 (calendar-day-of-week (list m 1 y))))
	   (last (calendar-last-day-of-month m y)))
       (while (<= sunday last)
	 (mapcar (lambda (x)
		   (let ((d (+ sunday x)))
		     (and (<= 1 d)
			  (<= d last)
			  (mark-visible-calendar-date
			   (list m d y)
			   calendar-weekend-marker))))
		 calendar-weekend)
	 (setq sunday (+ sunday 7))))
     (increment-calendar-month m y 1))))


(provide 'japanese-holidays)

;;; japanese-holidays.el ends here
