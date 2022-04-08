;;; flasher-core.el --- Flasher core API -*- lexical-binding: t; -*-
;;
;; Filename: flasher-core.el
;; Description: Flasher core API
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (org "9.4") (emacsql "3.0.0") (emacsql-sqlite "1.0.0"))
;; Author: Mora Unie Youer <mora_unie_youer@riseup.net>
;; Maintainer: Mora Unie Youer <mora_unie_youer@riseup.net>
;; Copyright (c) 2022 Mora Unie Youer
;; Created: April 02, 2022
;; URL: https://github.com/mora-unie-youer/flasher
;;      https://gitlab.com/mora-unie-youer/flasher
;;      https://notabug.org/mora-unie-youer/flasher
;;      https://codeberg.org/mora-unie-youer/flasher

;;; Commentary:
;;
;; Flasher core API

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:
(require 'flasher)

(defun flasher-core-scope ()
  "Convert `flasher-directories' to scope suitable for `org-map-entries'."
  (let (files)
    (dolist (dir flasher-directories)
      (setq files (append files (directory-files-recursively dir "\\.org$"))))
    files))

(defun flasher-core-map-cards (func)
  "Call FUNC at each entry marked with Flasher card tag."
  (let ((org-tags-exclude-from-inheritance (list flasher-card-tag)))
    (org-map-entries func (concat "+" flasher-card-tag) (flasher-core-scope))))

(defun flasher-core-get-card-id ()
  "Function that can be mapped in `flasher-core-map-entries' to get card ID."
  (org-id-get-create))

(defun flasher-core-sync-cards ()
  "Add all new cards to Flasher database."
  (dolist (id (flasher-core-map-cards #'flasher-core-get-card-id))
    (let ((card (flasher-db-get-card id)))
      (unless card (flasher-db-create-card id)))))

(defun flasher-core-card-due (card)
  "Return TIME, CARD is scheduled to."
  (let ((last-result (flasher-db-get-last-card-result card)))
    (if (not last-result)
        (current-time)
      (let ((interval-time (days-to-time (nth 5 last-result)))
            (result-date (nth 6 last-result)))
        (time-add result-date interval-time)))))

(defun flasher-core-card-overdue (card)
  "Return:
- 0 if CARD is new, or if it scheduled for review today.
- A negative integer - CARD is scheduled that many days in the future.
- A positive integer - CARD is scheduled that many days in the past."
  (let ((due (flasher-core-card-due card))
        (time (current-time)))
    (- (time-to-days time) (time-to-days due))))

(defun flasher-core-card-overdue-p (card &optional days-overdue)
  "Return non-nil if CARD should be considered 'overdue'.
CARD is scheduled DAYS-OVERDUE days in the past. If argument is not given it is
extracted from the CARD."
  (unless days-overdue
    (setq days-overdue (flasher-core-card-overdue card)))
  (let ((interval (nth 2 card)))
    (and (> days-overdue 0)
         (> (/ days-overdue interval) flasher-card-interval-overdue-factor))))

(defun flasher-core-card-age (card)
  "Return number of days elapsed since CARD was first reviewed."
  (let ((first-result (flasher-db-get-first-card-result card))
        (time (current-time)))
    (if (not first-result)
        0
      (- (time-to-days time) (time-to-days (nth 6 first-result))))))

(defun flasher-core-card-status (card)
  "Return a list (STATUS DUE AGE) of CARD.
DUE is the number of days overdue, zero being due today, -1 being scheduled
1 day in the future.
AGE is the number of days elapsed since the item was learned for the first time.
STATUS is one of the following values:
- :new
- :failed
- :overdue
- :young
- :old"
  (let ((interval (nth 2 card))
        (due (flasher-core-card-overdue card))
        (age (flasher-core-card-age card)))
    (list (cond
           ((= age 0) :new)
           ((= interval 0) :failed)
           ((flasher-core-card-overdue-p card due) :overdue)
           ((<= interval flasher-card-intervals-before-old) :young)
           (t :old))
          due age)))

(defun flasher-core-card-status-by-id (id)
  "Return a list (STATUS DUE AGE) of card with ID.
Look at function `flasher-core-card-status'"
  (flasher-core-card-status (flasher-db-get-card id)))

(provide 'flasher-core)

;;; flasher-core.el ends here
