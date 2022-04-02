;;; flasher.el --- Simple and extensible flashcard system  -*- lexical-binding: t; -*-
;;
;; Filename: flasher.el
;; Description: Simple and extensible flashcard system
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (org "9.4"))
;; Author: Mora Unie Youer <mora_unie_youer@riseup.net>
;; Maintainer: Mora Unie Youer <mora_unie_youer@riseup.net>
;; Copyright (c) 2022 Mora Unie Youer
;; Created: Mar 24 2022
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

(defgroup flasher nil
  "Manage, learn and review flashcards in Emacs."
  :group 'external)

(defcustom flasher-directories '("~/.flasher")
  "Directories to search for flashcards."
  :group 'flasher
  :type 'string)

(defcustom flasher-card-tag "card"
  "Tag for marking headlines as flashcards."
  :group 'flasher
  :type 'string)

(defcustom flasher-maximum-new-cards-per-session 30
  "Each Flasher session will have at most this many new cards to learn.
NIL = unlimited."
  :group 'flasher
  :type '(choice integer (const nil)))

(defcustom flasher-maximum-session-duration 30
  "Maximum duration of Flasher session in minutes.
NIL = unlimited."
  :group 'flasher
  :type '(choice integer (const nil)))

(defclass flasher-session ()
  ((results :initform nil
            :documentation "Stores all results during session."))
  "Object used for Flasher session."
  :group 'flasher)

(defvar flasher-current-session nil
  "If non-nil, it is an `flasher-session' object which is the current session.")

(defvar flasher-last-session nil
  "If non-nil, it is an `flasher-session' object which is the last session.
This can be used to resume the last session.")

(defgroup flasher-dashboard nil
  "Flasher dashboard mode."
  :group 'flasher)

(define-derived-mode flasher-dashboard-mode special-mode "Flasher Dashboard"
  "This mode is used to display Flasher dashboard."
  :group 'flasher-dashboard)

(defgroup flasher-learn nil
  "Flasher learning mode."
  :group 'flasher)

(define-derived-mode flasher-learn-mode special-mode "Flasher Learn"
  "This mode is used to learn new flashcards."
  :group 'flasher-learn)

(defgroup flasher-review nil
  "Flasher reviewing mode."
  :group 'flasher)

(define-derived-mode flasher-review-mode special-mode "Flasher Review"
  "This mode is used to review learned flashcards."
  :group 'flasher-review)

(provide 'flasher)

;;; flasher.el ends here
