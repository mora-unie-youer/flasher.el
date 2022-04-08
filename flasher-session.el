;;; flasher-session.el --- Flasher session API -*- lexical-binding: t; -*-
;;
;; Filename: flasher-session.el
;; Description: Flasher session API
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
;; Flasher session API

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

(defgroup flasher-session nil
  "Flasher session API."
  :group 'flasher)

(defcustom flasher-session-maximum-new-cards-count 30
  "Each non-custom Flasher session will have at most this many cards to learn.
NIL = unlimited."
  :group 'flasher-session
  :type '(choice integer (const nil)))

(defcustom flasher-session-maximum-review-cards-count 100
  "Each non-custom Flasher session will have at most this many cards to review.
NIL = unlimited."
  :group 'flasher-session
  :type '(choice integer (const nil)))

(defclass flasher-session ()
  ()
  "Object used for Flasher session."
  :group 'flasher)

(defvar flasher-session-current nil
  "If non-nil, it is an `flasher-session' object which is the current session.")

(defvar flasher-session-last nil
  "If non-nil, it is an `flasher-session' object which is the last session.
This can be used to resume the last session.")

(provide 'flasher-session)

;;; flasher-session.el ends here
