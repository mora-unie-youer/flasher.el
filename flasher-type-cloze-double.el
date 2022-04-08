;;; flasher-type-cloze-double.el --- Flasher 'cloze-double card -*- lexical-binding: t; -*-
;;
;; Filename: flasher-type-cloze-double.el
;; Description: Flasher 'cloze-double card
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
;; Flasher 'cloze-double card

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

(defgroup flasher-type-cloze-double nil
  "Flasher 'cloze-double card."
  :group 'flasher)

(defun flasher-type-cloze-double-init ()
  "Initialize 'cloze-double card."
  (interactive))

(defun flasher-type-cloze-double-setup ()
  "Prepare a 'cloze-double card for review.")

(defun flasher-type-cloze-double-flip ()
  "Flip 'cloze-double card.")

(defun flasher-type-cloze-double-update ()
  "Update review data for 'cloze-double card.")

(flasher-card-register-type
 'cloze-double
 'flasher-type-cloze-double-setup
 'flasher-type-cloze-double-flip
 'flasher-type-cloze-double-update)

(provide 'flasher-type-cloze-double)

;;; flasher-type-cloze-double.el ends here
