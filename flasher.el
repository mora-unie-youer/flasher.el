;;; flasher.el --- Simple and extensible flashcard system  -*- lexical-binding: t; -*-
;;
;; Filename: flasher.el
;; Description: Simple and extensible flashcard system
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (org "9.4") (emacsql "3.0.0") (emacsql-sqlite "1.0.0"))
;; Author: Mora Unie Youer <mora_unie_youer@riseup.net>
;; Maintainer: Mora Unie Youer <mora_unie_youer@riseup.net>
;; Copyright (c) 2022 Mora Unie Youer
;; Created: March 24 2022
;; URL: https://github.com/mora-unie-youer/flasher
;;      https://gitlab.com/mora-unie-youer/flasher
;;      https://notabug.org/mora-unie-youer/flasher
;;      https://codeberg.org/mora-unie-youer/flasher

;;; Commentary:
;;
;; Simple and extensible flashcard system

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

(require 'eieio)

(require 'emacsql)
(require 'emacsql-sqlite)

(require 'org)

(defgroup flasher nil
  "Manage, learn and review flashcards in Emacs."
  :group 'external)

(define-widget 'flasher-difficulty 'lazy
  "Difficulty factor in Flasher card entries."
  :group 'flasher
  :type 'float)

(define-widget 'flasher-interval 'lazy
  "Interval count in Flasher card entries."
  :group 'flasher
  :type 'integer)

(defcustom flasher-directories '("~/.flasher")
  "Directories to search for flashcards."
  :group 'flasher
  :type 'directory)

(defcustom flasher-card-tag "card"
  "Tag for marking headlines as flashcards."
  :group 'flasher
  :type 'string)

(defcustom flasher-card-initial-difficulty 1.0
  "Initial difficulty that will be set to card."
  :group 'flasher
  :type 'flasher-difficulty)

(defcustom flasher-card-initial-interval 0
  "Initial interval count until card review.
You shouldn't change this as it can lead to some bugs, I guess."
  :group 'flasher
  :type 'flasher-interval)

(defcustom flasher-card-intervals-before-old 10
  "When item's interval is above this value, it's no longer considered 'young'."
  :group 'flasher
  :type 'flasher-interval)

(defcustom flasher-card-interval-overdue-factor 0.2
  "Multiply factor to check if item is overdue.
Item is considered overdue, when its scheduled review date is more than
FLASHER-CARD-INTERVAL-OVERDUE-FACTOR * LAST-INTERVAL days in the past."
  :group 'flasher
  :type 'float)

(defun flasher-entry-p (&optional marker)
  "Is MARKER, or the point, in a 'flasher card'?
This will return NIL if the point is inside a subheading of a card."
  (save-excursion
    (when marker
      (switch-to-buffer (marker-buffer marker))
      (goto-char marker))
    (member flasher-card-tag (org-get-tags nil t))))

(provide 'flasher)

(cl-eval-when (load eval)
  (require 'flasher-db)
  (require 'flasher-core)
  (require 'flasher-session)
  (require 'flasher-dashboard)
  (require 'flasher-learn)
  (require 'flasher-review))

;;; flasher.el ends here
