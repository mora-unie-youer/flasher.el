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

(defcustom flasher-review-learn-count 20
  "Maximum count of cards to learn in a single session.
NIL = unlimited."
  :group 'flasher-review
  :type '(choice integer (const nil)))

(defcustom flasher-review-review-count 50
  "Maximum count of cards to review in a single session.
NIL = unlimited."
  :group 'flasher-review
  :type '(choice integer (const nil)))

(defclass flasher-review-session ()
  ((current-card :initform nil
                 :documentation "Current card in review session.")
   (cards :initarg :cards
          :initform nil
          :documentation "Cards in this review session.")
   (results :initform '(:total 0 :0 0 :1 0 :2 0 :3 0 :4 0 :5 0)
            :documentation "Result statistics for this review session."))
  "Object used for Flasher review session."
  :group 'flasher-review)

(defvar flasher-review--session nil
  "It is an `flasher-review-session' object which represents current session.")

(defun flasher-review--make-session (cards)
  "Create new review session with CARDS."
  (make-instance 'flasher-review-session :cards cards))

(defun flasher-review--add-result (result)
  "Store RESULT in the review history of session."
  (with-slots (results) flasher-review--current-session
    (cl-incf (cl-getf results (intern-soft (concat ":" (number-to-string result)))))
    (cl-incf (cl-getf results :total))))

(defmacro flasher-review-with-buffer (&rest body)
  "Eval BODY with Flasher review buffer."
  (declare (indent defun))
  `(with-current-buffer (get-buffer-create flasher-review-buffer-name) ,@body))

(defmacro flasher-review-with-buffer-start (&rest body)
  "Eval BODY at the start of Flasher review buffer."
  (declare (indent defun))
  `(flasher-review-with-buffer (goto-char (point-min)) ,@body))

(defmacro flasher-review-with-buffer-end (&rest body)
  "Eval BODY at the end of Flasher review buffer."
  (declare (indent defun))
  `(flasher-review-with-buffer (goto-char (point-max)) ,@body))

;;;###autoload
(defun flasher-review (&optional learn-count review-count cards)
  "Open new Flasher review session with CARDS.
LEARN-COUNT is maximum count of new cards to learn.
REVIEW-COUNT is maximum count of cards to review."
  (interactive)
  (unless learn-count (setq learn-count flasher-review-learn-count))
  (unless review-count (setq review-count flasher-review-review-count))
  (unless cards
    (unless flasher-dashboard--cards (flasher-dashboard--cards-reload))
    (setq cards flasher-dashboard--cards))
  (switch-to-buffer flasher-review-buffer-name)
  (flasher-review-mode)
  (flasher-review--set-header-line)
  (if flasher-review--session
      (if (yes-or-no-p "Cards are already being reviewed. Resume session?")
          (flasher-review-resume)
        (setq flasher-review--session nil)
        (flasher-review learn-count review-count cards))
    (let ((variants (flasher-review--shuffle-cards cards)))
      (if (null cards)
          (message "No cards due right now")
        (setq flasher-review--session (flasher-review--make-session variants))
        (flasher-review-next-card)))))

(defun flasher-review-resume ()
  "Resume previous Flasher review session."
  (interactive)
  (if flasher-review--current-session
      (flasher-review-next-card 'resuming)
    (message "No session to resume")))

(defun flasher-review-quit ()
  "Quit Flasher review session."
  (interactive)
  (flasher-core--remove-overlays)
  (kill-current-buffer))

(defun flasher-review--write-task (task)
  "Write TASK to Flasher review buffer."
  (flasher-review-with-buffer-end
    (insert "T:\n" task "\n")))

(defun flasher-review--write-question (question)
  "Write QUESTION to Flasher review buffer."
  (flasher-review-with-buffer-end
    (insert "Q:\n" question "\n")))

(defun flasher-review--write-answer (answer)
  "Write ANSWER to Flasher review buffer."
  (flasher-review-with-buffer-end
    (insert "A:\n" answer "\n")))

(defun flasher-review-next-card (&optional resuming)
  "Show next card in Flasher review session.
If RESUMING is non-nil, use current-card."
  (if (not (null (oref flasher-review--session cards)))
      (condition-case err
          (let ((card (pop (oref flasher-review--session cards))))
            (if (null resuming)
                (setf (oref flasher-review--session current-card) card)
              (push card (oref flasher-review--session cards))
              (setq card (oref flasher-review--session current-card)))
            (org-id-goto (cl-second card))
            (let ((type (flasher-card--get-type))
                  (variant (cl-third card))
                  (inhibit-read-only t))
              (flasher-review-with-buffer (erase-buffer))
              (funcall (flasher-card--type-setup-fn type) variant))
            (switch-to-buffer flasher-review-buffer-name)
            (goto-char (point-max))
            (flasher-review-flip-mode))
        (error (flasher-review-quit)
               (signal (car err) (cdr err))))
    (message "Review done")
    (flasher-review-quit)))

(defun flasher-review-hint ()
  "Show hint for current card in Flasher review session."
  (interactive)
  (condition-case err
      (let ((card (oref flasher-review--session current-card)))
        (org-id-goto (cl-second card))
        (let ((type (flasher-card--get-type))
              (inhibit-read-only t))
          (funcall (flasher-card--type-hint-fn type)))
        (switch-to-buffer flasher-review-buffer-name))
    (error (flasher-review-quit)
           (signal (car err) (cdr err)))))

(defun flasher-review-flip ()
  "Flip current card in Flasher review session."
  (interactive)
  (condition-case err
      (let ((card (oref flasher-review--session current-card)))
        (org-id-goto (cl-second card))
        (let ((type (flasher-card--get-type))
              (inhibit-read-only t))
          (funcall (flasher-card--type-flip-fn type)))
        (switch-to-buffer flasher-review-buffer-name)
        (flasher-review-flip-mode -1)
        (flasher-review-rate-mode))
    (error (flasher-review-quit)
           (signal (car err) (cdr err)))))

(defun flasher-review-skip ()
  "Skip current card in this session."
  (interactive)
  (flasher-review-rate-mode -1)
  (flasher-review-next-card))

(defun flasher-review--assign-variants (card)
  "Return list of CARD variants with assigned random numbers."
  (let* ((type (cl-second card))
         (sort-p (flasher-card--type-sort-p type))
         (variants (flasher-card--filter-due card))
         (numbers (cl-loop for i below (length variants) collect (cl-random 1.0))))
    (when sort-p (setq numbers (sort numbers #'<)))
    (cl-loop for n in numbers for var in variants collect (cons n var))))

(defun flasher-review--shuffle-cards (cards)
  "Return list of shuffled CARDS."
  (let ((variants (mapcan #'flasher-review--assign-variants cards)))
    (mapcar #'cdr (sort variants (lambda (a b) (< (car a) (car b)))))))

(defvar flasher-review-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `flasher-review-mode'.")

(define-derived-mode flasher-review-mode special-mode "Flasher Review"
  "This mode is used to review learned flashcards."
  :group 'flasher-review)

(defvar flasher-review-flip-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `flasher-review-flip-mode'.")

(define-minor-mode flasher-review-flip-mode
  "Minor mode for card flipping."
  :group 'flasher-review
  :init-value nil
  :lighter "Flip"
  :keymap flasher-review-flip-mode-map)

(defvar flasher-review-rate-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `flasher-review-rate-mode'.")

(define-minor-mode flasher-review-rate-mode
  "Minor mode for card rating."
  :group 'flasher-review
  :init-value nil
  :lighter "Rate"
  :keymap flasher-review-rate-mode-map)

(defvar flasher-review-edit-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `flasher-review-edit-mode'.")

(define-minor-mode flasher-review-edit-mode
  "Minor mode for card editing."
  :group 'flasher-review
  :init-value nil
  :lighter "Edit"
  :keymap flasher-review-edit-mode-map)

(provide 'flasher-review)

;;; flasher-review.el ends here
