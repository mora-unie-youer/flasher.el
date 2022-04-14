;;; flasher-review.el --- Flasher review mode -*- lexical-binding: t; -*-
;;
;; Filename: flasher-review.el
;; Description: Flasher review mode
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
;; Flasher review mode

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

(defgroup flasher-review nil
  "Flasher reviewing mode."
  :group 'flasher)

(defcustom flasher-review-buffer-name "*Flasher Review*"
  "Name of the buffer to use for displaying the review view."
  :group 'flasher-review
  :type 'string)

(define-derived-mode flasher-review-mode special-mode "Flasher Review"
  "This mode is used to review learned flashcards."
  :group 'flasher-review)

(defmacro flasher-review-with-buffer (&rest body)
  "Eval BODY with Flasher review buffer."
  (declare (indent defun))
  `(with-current-buffer (get-buffer-create flasher-review-buffer-name) ,@body))

(defun flasher-review-view ()
  "Show the Flasher review view in the review buffer."
  (let ((cards (flasher-core--map-cards #'flasher-card--get-info))
        (inhibit-read-only t))
    (flasher-review-with-buffer
      (erase-buffer)
      (insert (propertize "Flasher Review\n\n" 'face 'org-level-1)))
    (pcase-dolist (`(,id ,type ,variants) cards)
      (org-id-goto id)
      (dolist (variant variants)
        (funcall (flasher-card--type-setup-fn type) (cl-second variant))
        (funcall (flasher-card--type-flip-fn type))
        (flasher-review-with-buffer (insert "\n\n\n"))))))

;;;###autoload
(defun flasher-review ()
  "Open Flasher review."
  (interactive)
  (flasher-review-view)
  (switch-to-buffer flasher-review-buffer-name)
  (goto-char (point-min))
  (flasher-review-mode))

(provide 'flasher-review)

;;; flasher-review.el ends here
