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

(defcustom flasher-type-cloze-type-property "CLOZE_TYPE"
  "Property used to store cloze type."
  :group 'flasher-type-cloze
  :type 'string)

(defface flasher-type-cloze-hole-face
  '((t (:bold t)))
  "Face for Flasher cloze card holes."
  :group 'flasher-type-cloze)

(defconst flasher-type-cloze--regex (rx "{"
                                        (: "{" (group (+ (not (any "}")))) "}")
                                        (? "{" (group (+ (not (any "}")))) "}")
                                        (? "@" (group (+ digit)))
                                        "}")
  "Regular expression to match cloze holes.")

(defconst flasher-type-cloze--variant-regex (rx (group (| "front" "back"))
                                                (? (group (+ digit))))
  "Regular expression to match cloze variants.")

(defvar flasher-type-cloze--text-overlay '()
  "Text overlay.")

(defvar flasher-type-cloze--hint-overlay '()
  "Hint overlay.")

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
      (let* ((data (match-data))
             (id-string (match-string 3))
             (id (if id-string (string-to-number id-string)))
             (holes-id (alist-get id holes)))
        (cond
         ((or (null id) (= id next-id))
          (setq id next-id)
          (cl-incf next-id)
          (push data holes-id))
         ((null holes-id) (error "IDs must be in order"))
         (t (push data holes-id)))
        (setf (alist-get id holes) holes-id)))
    holes))

(defun flasher-type-cloze--hole-visible-p (type id current-id)
  "Check if hole with ID is visible.
TYPE is type of cloze card.
CURRENT-ID is ID of current cloze card variant."
  (pcase type
    ("delete" t)
    ("enum" (< id current-id))
    (_ (error "Unknown cloze card type %s" type))))

(defun flasher-type-cloze--hide-holes (type current-id &optional all-visible)
  "Hide holes of a cloze card with TYPE and CURRENT-ID variant.
ALL-VISIBLE can be used to mark all holes visible."
  (flasher-review-with-buffer
    (let* ((holes (flasher-type-cloze--parse-holes)))
      (goto-char (point-max))
      (pcase-dolist (`(,id . ,holes-id) holes)
        (pcase-dolist (`(,hole-beg ,hole-end ,text-beg ,text-end ,hint-beg ,hint-end) holes-id)
          (unless hint-beg (setq hint-beg text-end hint-end text-end))
          (cond
           ((and (not (null current-id)) (= id current-id))
            (flasher-core--hide-region hole-beg text-beg "")
            (push (flasher-core--make-overlay text-beg text-end 'invisible t)
                  flasher-type-cloze--text-overlay)
            (flasher-core--hide-region text-end hint-beg "")
            (push (flasher-core--overlay-surround
                   (flasher-core--make-overlay hint-beg hint-end 'display "...")
                   (if (= hint-beg hint-end) "[..." "[") "]"
                   'flasher-type-cloze-hole-face)
                  flasher-type-cloze--hint-overlay)
            (flasher-core--hide-region hint-end hole-end "")
            (flasher-core--make-overlay hole-beg hole-end 'face 'flasher-type-cloze-hole-face))
           ((or all-visible (flasher-type-cloze--hole-visible-p type id current-id))
            (flasher-core--hide-region hole-beg text-beg)
            (flasher-core--hide-region text-end hole-end))
           (t (flasher-core--hide-region hole-beg hole-end "..."))))))))

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
  "Prepare a 'cloze card VARIANT for review."
  (setq flasher-type-cloze--text-overlay '())
  (setq flasher-type-cloze--hint-overlay '())
  (let ((back (flasher-core--card-back-side))
        (type (org-entry-get (point) flasher-type-cloze-type-property))
        (variant (flasher-type-cloze--parse-variant variant)))
    (flasher-review-with-buffer
      (save-excursion (insert back "\n"))
      (flasher-type-cloze--hide-holes type (cdr variant)))))

(defun flasher-type-cloze-hint ()
  "Show 'cloze card hole hint."
  (dolist (overlay flasher-type-cloze--hint-overlay)
    (overlay-put overlay 'display nil)))

(defun flasher-type-cloze-flip ()
  "Flip 'cloze card."
  (dolist (overlay flasher-type-cloze--text-overlay)
    (overlay-put overlay 'invisible nil))
  (dolist (overlay flasher-type-cloze--hint-overlay)
    (let ((start (overlay-start overlay)) (end (overlay-end overlay)))
      (flasher-review-with-buffer
        (remove-overlays start end)
        (flasher-core--hide-region start end)))))

(defun flasher-type-cloze-update ()
  "Update review data for 'cloze card.")

(flasher-card-register-type
 'cloze
 'flasher-type-cloze-var-init
 'flasher-type-cloze-setup
 'flasher-type-cloze-hint
 'flasher-type-cloze-flip
 'flasher-type-cloze-update)

(provide 'flasher-type-cloze)

;;; flasher-type-cloze.el ends here
