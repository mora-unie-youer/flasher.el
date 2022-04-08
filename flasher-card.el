;;; flasher-card.el --- Flasher card API -*- lexical-binding: t; -*-
;;
;; Filename: flasher-card.el
;; Description: Flasher card API
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
;; Flasher card API

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

(defgroup flasher-card nil
  "Flasher card API."
  :group 'flasher)

(defcustom flasher-card-tag "card"
  "Tag for marking headlines as flashcards."
  :group 'flasher-card
  :type 'string)

(define-widget 'flasher-card-difficulty 'lazy
  "Difficulty factor in Flasher card entries."
  :group 'flasher
  :type 'float)

(define-widget 'flasher-card-interval 'lazy
  "Interval count in Flasher card entries."
  :group 'flasher
  :type 'integer)

(defcustom flasher-card-initial-interval 1
  "Initial interval count for learned card."
  :group 'flasher-card
  :type 'flasher-card-interval)

(defcustom flasher-card-initial-difficulty 1.0
  "Initial difficulty that will be set to card."
  :group 'flasher
  :type 'flasher-card-difficulty)

(defcustom flasher-card-intervals-before-old 10
  "When item's interval is above this value, it's no longer considered 'young'."
  :group 'flasher-card
  :type 'flasher-card-interval)

(defcustom flasher-card-interval-overdue-factor 0.2
  "Multiply factor to check if item is overdue.
Item is considered overdue, when its scheduled review date is more than
FLASHER-CARD-INTERVAL-OVERDUE-FACTOR * LAST-INTERVAL days in the past."
  :group 'flasher-card
  :type 'float)


(defvar flasher-card-types '()
  "Alist for registering card types.
Entries should be lists (name setup-fn flip-fn update-fn).
Use `flasher-card-register-type' for adding card types.")

(defcustom flasher-card-type-property "CARD_TYPE"
  "Property used to store card type."
  :group 'flasher-card
  :type 'string)

(defun flasher-card-type (type)
  "Return card TYPE. If TYPE doesn't exist, printing error."
  (if-let ((card-type (alist-get type flasher-card-types nil nil #'string=)))
      card-type
    (error "Card type '%s' doesn't exist" type)))

(defun flasher-card-type-p (type)
  "Return non-nil if TYPE exists."
  (not (null (flasher-card-type type))))

(defun flasher-card--type-setup-fn (type)
  "Get the setup function for a card of TYPE."
  (cl-first (flasher-card-type type)))

(defun flasher-card--type-flip-fn (type)
  "Get the flip function for a card of TYPE."
  (cl-second (flasher-card-type type)))

(defun flasher-card--type-update-fn (type)
  "Get the update function for a card of TYPE."
  (cl-third (flasher-card-type type)))

(defun flasher-card-register-type (name setup-fn flip-fn update-fn)
  "Register a new card type.
NAME is name of the new type.
SETUP-FN is function for initializing a new card of this type.
FLIP-FN is function for flipping a card during review.
UPDATE-FN is function to update a card when it's contents have changed."
  (push (list name setup-fn flip-fn update-fn) flasher-card-types))


(defun flasher-card-p ()
  "Check if current heading is a card."
  (member flasher-card-tag (org-get-tags nil 'local)))

(defun flasher-card-init (type)
  "Initialize the card with TYPE."
  (if (flasher-card-p)
      (error "Heading is already a card"))
  (when (flasher-card-type-p type)
    (org-back-to-heading)
    (org-set-property flasher-card-type-property type)
    (flasher-core--add-tag flasher-card-tag)
    (org-id-get-create)
    (flasher-card-update)))

(defun flasher-card-update ()
  "Update card in Flasher."
  (let* ((id (org-id-get))
         (type (org-entry-get (point) flasher-card-type-property))
         (card (car (flasher-db-query [:select * :from cards :where (= id $s1)] id))))
    (cond
     ((null id) (error "%s:%d: No card ID" buffer-file-name (line-number-at-pos)))
     ((null type) (error "%s:%d: No card type" buffer-file-name (line-number-at-pos)))
     ((null card) (flasher-db-query [:insert-into cards :values $v1] (vector id type)))
     ((not (string= type (cl-second card)))
      (flasher-db-query [:update cards :set [(= type $s2)] :where (= id $s1)] id type)
      (flasher-db-query [:delete-from results :where (= card-id $s1)] id)))))

(defun flasher-card-sync ()
  "Add all new cards to Flasher."
  (interactive)
  (flasher-core--map-cards #'flasher-card-update))

(defun flasher-card-first-result (&optional id)
  "Return first result of card at point or with ID."
  (unless id (setq id (org-id-get)))
  (car (flasher-db-query [:select * :from results :where (= card-id $s1)
                          :order-by (asc date) :limit 1] id)))

(defun flasher-card-last-result (&optional id)
  "Return last result of card at point or with ID."
  (unless id (setq id (org-id-get)))
  (car (flasher-db-query [:select * :from results :where (= card-id $s1)
                          :order-by (desc date) :limit 1] id)))

(defun flasher-card-age (&optional id first-result)
  "Return number of days elapsed since card at point or with ID was first reviewed.
FIRST-RESULT can be specified to reduce number of database calls."
  (unless id (setq id (org-id-get)))
  (unless first-result (setq first-result (flasher-card-first-result id)))
  (cond ((null first-result) 0)
        (t (- (time-to-days (current-time)) (time-to-days (cl-sixth first-result))))))

(defun flasher-card-due (&optional id last-result)
  "Return TIME, card at point or with ID is scheduled to.
LAST-RESULT can be specified to reduce number of database calls."
  (unless id (setq id (org-id-get)))
  (unless last-result (setq last-result (flasher-card-last-result id)))
  (cond ((null last-result) (current-time))
        (t (time-add (cl-sixth last-result) (cl-fifth last-result)))))

(defun flasher-card-overdue (&optional id last-result)
  "Return for CARD at point or with ID:
- 0 if CARD is new, or if it scheduled for review today.
- A negative integer - CARD is scheduled that many days in the future.
- A positive integer - CARD is scheduled that many days in the past.
LAST-RESULT can be specified to reduce number of database calls."
  (unless last-result (setq last-result (flasher-card-last-result id)))
  (- (time-to-days (current-time)) (time-to-days (flasher-card-due id last-result))))

(defun flasher-card-overdue-p (&optional id days-overdue last-result)
  "Return non-nil if CARD at point or with ID should be considered 'overdue'.
CARD is scheduled DAYS-OVERDUE days in the past. If argument is not given it is
extracted from the CARD.
LAST-RESULT can be specified to reduce number of database calls."
  (unless id (setq id (org-id-get)))
  (unless last-result (setq last-result (flasher-card-last-result id)))
  (unless days-overdue (setq days-overdue (flasher-card-overdue id last-result)))
  (let ((interval (cl-fifth last-result)))
    (and (> days-overdue 0)
         (> (/ days-overdue interval) flasher-card-interval-overdue-factor))))

(defun flasher-card-status (&optional id first-result last-result)
  "Fetch status list (STATUS DUE AGE) of card at point or with ID.
DUE is the number of days overdue, zero being due today, -1 being scheduled
1 day in the future.
AGE is the number of days elapsed since the item was learned for the first time.
STATUS is one of the following values:
- :new
- :failed
- :overdue
- :young
- :old
FIRST-RESULT, LAST-RESULT can be specified to reduce number of database calls."
  (unless id (setq id (org-id-get)))
  (unless first-result (setq first-result (flasher-card-first-result id)))
  (unless last-result (setq first-result (flasher-card-last-result id)))
  (let ((interval (if last-result (cl-fifth last-result) 0))
        (age (flasher-card-age id first-result))
        (due (flasher-card-overdue id last-result)))
    (list (cond ((= age 0) :new)
                ((= interval 0) :failed)
                ((flasher-card-overdue-p id due last-result) :overdue)
                ((<= interval flasher-card-intervals-before-old) :young)
                (t :old))
          due age)))

(provide 'flasher-card)

;;; flasher-card.el ends here
