;;; flasher.el --- Simple and extensible flashcard system  -*- lexical-binding: t; -*-
;;
;; Filename: flasher.el
;; Description: Simple and extensible flashcard system
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (org "9.4"))
;; Author: Mora Unie Youer <mora_unie_youer@riseup.net>
;; Maintainer: Mora Unie Youer <mora_unie_youer@riseup.net>
;; Copyright (c) 2022 Mora Unie Youer
;; Created: March 24 2022
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

(require 'emacsql)
(require 'emacsql-sqlite)

(require 'org)

(defgroup flasher nil
  "Manage, learn and review flashcards in Emacs."
  :group 'external)

(defcustom flasher-directories '("~/.flasher")
  "Directories to search for flashcards."
  :group 'flasher
  :type 'directory)

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

(defun flasher-scope ()
  "Generate scope suitable for `org-map-entries'.
As we have `flasher-directories', we just need to list all files in that
directories."
  (let (files)
    (dolist (directory flasher-directories)
      (setq files (append files
                          (directory-files-recursively directory ".org$"))))
    files))

(defun flasher-map-entries (func)
  "Call FUNC at each entry marked with Flasher card tag."
  (org-map-entries func
                   (concat "+" flasher-card-tag)
                   (flasher-scope)))

(defun flasher-entry-p (&optional marker)
  "Is MARKER, or the point, in a 'flasher card'?
This will return NIL if the point is inside a subheading of a card."
  (save-excursion
    (when marker
      (switch-to-buffer (marker-buffer marker))
      (goto-char marker))
    (member flasher-card-tag (org-get-tags nil t))))

(provide 'flasher)

(cl-eval-when (load eval)
  (require 'flasher-session)
  (require 'flasher-dashboard)
  (require 'flasher-learn)
  (require 'flasher-review))

;;; flasher.el ends here
