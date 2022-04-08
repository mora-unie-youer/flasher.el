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
    (let ((id (org-id-get-create)))
      (flasher-db-query [:insert-into cards
                         :values $v1]
                        (vector id type flasher-card-initial-difficulty 0)))))

(provide 'flasher-card)

;;; flasher-card.el ends here
