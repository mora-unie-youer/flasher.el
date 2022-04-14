;;; flasher-type-cloze.el --- Flasher 'cloze card -*- lexical-binding: t; -*-
;;
;; Filename: flasher-type-cloze.el
;; Description: Flasher 'cloze card
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
;; Flasher 'cloze card

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

(defgroup flasher-type-cloze nil
  "Flasher 'cloze card."
  :group 'flasher)

(defconst flasher-type-cloze--regex (rx "{"
                                        (: "{" (group (+ (not (any "}")))) "}")
                                        (? "{" (group (+ (not (any "}")))) "}")
                                        (? "@" (group (+ digit)))
                                        "}")
  "Regular expression to match cloze holes.")

(defconst flasher-type-cloze--variant-regex (rx (group (| "front" "back"))
                                                (? (group (+ digit))))
  "Regular expression to match cloze variants.")

(defun flasher-type-cloze--parse-variant (variant)
  "Return (SIDE . INDEX) by parsing cloze card VARIANT."
  (save-match-data
    (string-match flasher-type-cloze--variant-regex variant)
    (let ((side (match-string 1 variant))
          (id (match-string 2 variant)))
      (cons side (if id (string-to-number id))))))

(defun flasher-type-cloze--parse-holes ()
  "Return list of (ID . HOLES) parsed from cloze holes."
  (let (holes (next-id 0))
    (while (re-search-forward flasher-type-cloze--regex nil t)
      (let* ((id-string (match-string 3))
             (id (if id-string (string-to-number id-string)))
             (holes-id (alist-get id holes)))
        (cond
         ((or (null id) (= id next-id))
          (setq id next-id)
          (cl-incf next-id)
          (push (cons id (list (match-data))) holes))
         ((null holes-id) (error "IDs must be in order"))
         (t (push (match-data) holes-id)))))
    holes))

(defun flasher-type-cloze-init ()
  "Initialize 'cloze card."
  (interactive)
  (flasher-card-init "cloze"))

(defun flasher-type-cloze--get-variants (side &optional variants holes)
  "Generate 'cloze card VARIANTS from HOLES for SIDE."
  (unless holes (setq holes (flasher-type-cloze--parse-holes)))
  (if holes
      (dolist (holes-id holes)
        (push (concat side (number-to-string (car holes-id))) variants))
    (push side variants))
  variants)

(defun flasher-type-cloze-var-init (&optional id)
  "Initialize 'cloze card variants for card with ID."
  (let ((back (flasher-core--card-back-side))
        variants)
    (with-temp-buffer
      (save-excursion (insert back "\n"))
      (setq variants (flasher-type-cloze--get-variants "back")))
    (flasher-card--update-variants variants id)))

(defun flasher-type-cloze-setup (variant)
  "Prepare a 'cloze card VARIANT for review.")

(defun flasher-type-cloze-flip ()
  "Flip 'cloze card.")

(defun flasher-type-cloze-update ()
  "Update review data for 'cloze card.")

(flasher-card-register-type
 'cloze
 'flasher-type-cloze-var-init
 'flasher-type-cloze-setup
 'flasher-type-cloze-flip
 'flasher-type-cloze-update)

(provide 'flasher-type-cloze)

;;; flasher-type-cloze.el ends here
