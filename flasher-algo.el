;;; flasher-algo.el --- Flasher algorithm API -*- lexical-binding: t; -*-
;;
;; Filename: flasher-algo.el
;; Description: Flasher algorithm API
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
;; Flasher algorithm API

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

(defgroup flasher-algo nil
  "Flasher algorithm API."
  :group 'flasher)

(define-widget 'flasher-algo-ease 'lazy
  "Ease factor in Flasher card entries."
  :group 'flasher-algo
  :type 'float)

(define-widget 'flasher-algo-interval 'lazy
  "Interval count in Flasher card entries."
  :group 'flasher-algo
  :type 'integer)

(defcustom flasher-algo-initial-interval 1
  "Initial interval count for learned card."
  :group 'flasher-algo
  :type 'flasher-algo-interval)

(defcustom flasher-algo-initial-ease 2.5
  "Initial ease factor that will be set to card."
  :group 'flasher-algo
  :type 'flasher-algo-ease)

(defcustom flasher-algo-minimum-ease 1
  "Minimum ease factor card can have."
  :group 'flasher-algo
  :type 'flasher-algo-ease)

(defcustom flasher-algo-maximum-ease nil
  "Maximum ease factor card can have.
NIL = unlimited."
  :group 'flasher-algo
  :type '(choice flasher-algo-ease (const nil)))

(defcustom flasher-algo-ease-deltas '((0 . -0.8)
                                      (1 . -0.55)
                                      (2 . -0.3)
                                      (3 . -0.15)
                                      (4 . 0)
                                      (5 . 0.1))
  "Deltas to a cards ease factor depending on its quality."
  :group 'flasher-algo
  :type 'list)

(defcustom flasher-algo-max-interval-count nil
  "Maximum interval count algorithm can return.
NIL = unlimited.

This is used to avoid huge numbers in interval time, so card can be met again
even after perfectly remembering it."
  :group 'flasher-algo
  :type '(choice integer (const nil)))

(defcustom flasher-algo-minimum-fuzz 0.9
  "Lower bound for random interval fuzz factor."
  :group 'flasher-algo
  :type 'float)

(defcustom flasher-algo-maximum-fuzz 1.1
  "Upper bound for random interval fuzz factor."
  :group 'flasher-algo
  :type 'float)

(defun flasher-algo (card-stats quality)
  "Determine the next iteration of CARD-STATS based on QUALITY.
CARD-STATS is (EASE FAILED INTERVAL). Result has the same shape.
EASE is ease factor of card.
FAILED - was card failed before? All new cards are considered failed.
INTERVAL is interval count of card.
QUALITY is the quality of the answer:
  5 - perfect answer
  4 - correct answer took a while
  3 - correct answer recalled with serious difficulty
  2 - incorrect answer; where the correct one seemed easy to recall
  1 - incorrect answer; remembered the correct one
  0 - complete blackout"
  (let ((ease (cl-first card-stats))
        (failed (cl-second card-stats))
        (interval (cl-third card-stats))
        next-ease next-interval)
    (setq next-ease (max flasher-algo-minimum-ease
                         (+ ease (alist-get quality flasher-algo-ease-deltas))))
    (when flasher-algo-maximum-ease
      (setq next-ease (min flasher-algo-maximum-ease next-ease)))
    (setq next-interval (cond ((or failed (< quality 3)) 0)
                              ((= interval 0) 1)
                              ((= interval 1) 4)
                              ((< quality 5) (flasher-algo-fuzz (* 1.25 interval)))
                              (t (flasher-algo-fuzz (* next-ease interval)))))
    (when flasher-algo-max-interval-count
      (setq next-interval (min flasher-algo-max-interval-count next-interval)))
    (list next-ease (< quality 3) next-interval)))

(defun flasher-algo-fuzz (interval)
  "Apply fuzz to INTERVAL.
Multiply INTERVAL by a random factor between `flasher-algo-minimum-fuzz' and
`flasher-algo-maximum-fuzz'"
  (let ((minimum flasher-algo-minimum-fuzz) (maximum flasher-algo-maximum-fuzz))
    (round (* interval (+ minimum (cl-random (- maximum minimum)))))))

(provide 'flasher-algo)

;;; flasher-algo.el ends here
