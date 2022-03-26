;;; flasher.el --- Simple and extensible flashcard system  -*- lexical-binding: t; -*-
;;
;; Filename: flasher.el
;; Description: Simple and extensible flashcard system
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

(defvar flasher-base-interval 21600
  "Duration (in seconds) of one interval to repeat card.
Used to calculate next interval, depending on review result.")

(defvar flasher-base-delta 0.0288
  "Base coefficient used in delta calculation.
Used to adjust difficulty calculation for your needs.")

(defvar flasher-max-interval-count 512
  "Maximum intervals count algorithm can return.
This is used to avoid huge numbers in interval time, so card can be met again
even after perfectly remembering it. Can be set to 0, if you want to not limit
maximum interval.")

(defvar flasher-min-difficulty 0
  "Minimum difficulty card can have.
Can be used to limit increment of interval duration in long term. I don't think
that it can be useful for users because there's max interval count, which acts
much better for this purpose.")

(defun flasher-algo (card-stats result)
  "Determine the next iteration of CARD-STATS based on RESULT.
CARD-STATS is (DIFFICULTY . INTERVAL), the result has the
same shape, with updated values. DIFFICULTY - the previous ease factor of the
card. All cards are initialized with DIFFICULTY of 1. It will decrease for easy
cards, but not below 0 (will slowdown as it approaches zero), and will increase
for difficult cards, but not above 1.
INTERVAL - previous interval between repetitions.
RESULT - the quality of the answer:
  5 - perfect answer
  4 - correct answer took a while
  3 - correct answer recalled with serious difficulty
  2 - incorrect answer; where the correct one seemed easy to recall
  1 - incorrect answer; remembered the correct one
  0 - complete blackout"
  (let ((difficulty (car card-stats))
        (interval   (cdr card-stats))
        delta)
    (setq delta (* flasher-base-delta (- 5 (* 1.8 result))))
    (if (< result 3)
        (cons (min 1 (+ difficulty delta)) 1)
      (progn
        (setq difficulty (max flasher-min-difficulty (* difficulty (+ 1 delta))))
        (setq interval (cond ((null interval) 1)
                             (t (round (/ interval difficulty)))))
        (cons difficulty (cond ((= flasher-max-interval-count 0) interval)
                               (t (min interval flasher-max-interval-count))))))))

(provide 'flasher)

;;; flasher.el ends here
