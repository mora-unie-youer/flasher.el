;;; flasher-core.el --- Flasher core API -*- lexical-binding: t; -*-
;;
;; Filename: flasher-core.el
;; Description: Flasher core API
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
;; Flasher core API

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

(defgroup flasher-core nil
  "Flasher core API."
  :group 'flasher)

(defun flasher-core--hide-region (from to &optional text face)
  "Hide region FROM ... TO, replacing it with TEXT with FACE."
  (let ((overlay (make-overlay from to nil t)))
    (overlay-put overlay 'category 'flasher)
    (overlay-put overlay 'evaporate t)
    (when face (overlay-put overlay 'face face))
    (if (stringp text)
        (progn
          (overlay-put overlay 'invisible nil)
          (overlay-put overlay 'display text))
      (overlay-put overlay 'invisible t))
    overlay))

(defun flasher-core--make-overlay (from to &rest props)
  "Create an overlay in region FROM ... TO with PROPS."
  (let ((overlay (make-overlay from to nil t)))
    (overlay-put overlay 'category 'flasher)
    (cl-loop for (prop value) on props by #'cddr do
             (overlay-put overlay prop value))
    overlay))

(defun flasher-core--overlay-surround (overlay before after &optional face)
  "Surround OVERLAY with strings BEFORE and AFTER with FACE."
  (overlay-put overlay 'before-string (propertize before 'face face))
  (overlay-put overlay 'after-string (propertize after 'face face))
  overlay)

(defun flasher-core--remove-overlays ()
  "Remove all Flasher related overlays."
  (remove-overlays (point-min) (point-max) 'category 'flasher))

(defun flasher-core--scope ()
  "Convert `flasher-directories' to scope suitable for `org-map-entries'."
  (let (files)
    (dolist (dir flasher-directories)
      (setq files (append files (directory-files-recursively dir "\\.org$"))))
    files))

(defun flasher-core--map-cards (func)
  "Call FUNC at each entry marked with Flasher card tag."
  (with-temp-buffer
    (org-mode)
    (mapc #'insert-file-contents (flasher-core--scope))
    (goto-char (point-min))
    (let ((org-tags-exclude-from-inheritance (list flasher-card-tag))
          rtn rtn1)
      (while (re-search-forward flasher-card--headline-regexp nil t)
        (let ((tags (org-get-tags)))
          (when (member flasher-card-tag tags)
            (setq rtn1 (funcall func))
            (push rtn1 rtn))))
      (nreverse rtn))))

(defun flasher-core--add-tag (tag)
  "Add TAG to the heading at point."
  (org-set-tags
   (cl-remove-duplicates
    (cons tag (org-get-tags nil 'local))
    :test #'string=)))

(defun flasher-core--remove-tag (tag)
  "Add TAG to the heading at point."
  (org-set-tags
   (remove tag (org-get-tags nil 'local))))

(defun flasher-core--heading-text (heading)
  "Return text from HEADING point marker."
  (let (text)
    (with-current-buffer (marker-buffer heading)
      (save-excursion
        (goto-char heading)
        (end-of-line 1)
        (setq text (buffer-substring (min (1+ (point)) (point-max))
                                     (progn (outline-next-heading) (point))))))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward org-drawer-regexp nil t)
        (delete-region (match-beginning 0)
                       (progn (re-search-forward "^[ \t]*:END:.*\n?" nil t)
                              (point))))
      (setq text (replace-regexp-in-string "\n$" "" (buffer-string)))
      (set-text-properties 0 (length text) nil text)
      text)))

(defun flasher-core--card-explain-marker ()
  "Return marker to card's explanation."
  (if-let ((has-tag (member flasher-card-explain-tag (org-get-tags))))
      (save-excursion
        (re-search-backward flasher-card--explain-regexp nil t)
        (point-marker))))

(defun flasher-core--card-task ()
  "Return card's task."
  (if-let ((has-tag (member flasher-card-task-tag (org-get-tags)))
           (heading (save-excursion (re-search-backward flasher-card--task-regexp nil t)
                                    (point-marker))))
      (flasher-core--heading-text heading)))

(defun flasher-core--card-side-heading (side)
  "Return point marker at the beginning of card's SIDE subheading."
  (let ((level (cl-first (org-heading-components)))
        found)
    (org-map-entries (lambda ()
                       (when (let ((components (org-heading-components)))
                               (and (not found)
                                    (= (cl-first components) (1+ level))
                                    (string= (cl-fifth components) side)))
                         (setq found (point-marker))))
                     t 'tree)
    found))

(defun flasher-core--card-front-side ()
  "Return card's front side."
  (if-let ((heading (flasher-core--card-side-heading "Front")))
      (flasher-core--heading-text heading)
    (cl-fifth (org-heading-components))))

(defun flasher-core--card-back-side ()
  "Return card's front side."
  (if-let ((heading (flasher-core--card-side-heading "Back")))
      (flasher-core--heading-text heading)
    (flasher-core--heading-text (point-marker))))

(defun flasher-core--card-side (side)
  "Return card's SIDE."
  (if-let ((heading (flasher-core--card-side-heading side)))
      (flasher-core--heading-text heading)
    (error "Card doesn't have '%s' side" side)))

(provide 'flasher-core)

;;; flasher-core.el ends here
