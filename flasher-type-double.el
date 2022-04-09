;;; flasher-type-double.el --- Flasher 'double card -*- lexical-binding: t; -*-
;;
;; Filename: flasher-type-double.el
;; Description: Flasher 'double card
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
;; Flasher 'double card

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

(defgroup flasher-type-double nil
  "Flasher 'double card."
  :group 'flasher)

(defun flasher-type-double-init ()
  "Initialize 'double card."
  (interactive)
  (flasher-card-init "double"))

(defun flasher-type-double-var-init (&optional id)
  "Initialize 'double card variants for card with ID."
  (flasher-card--update-variants '("front" "back") id))

(defun flasher-type-double-setup (_variant)
  "Prepare a 'double card for review.")

(defun flasher-type-double-flip ()
  "Flip 'double card.")

(defun flasher-type-double-update ()
  "Update review data for 'double card.")

(flasher-card-register-type
 'double
 'flasher-type-double-var-init
 'flasher-type-double-setup
 'flasher-type-double-flip
 'flasher-type-double-update)

(provide 'flasher-type-double)

;;; flasher-type-double.el ends here
