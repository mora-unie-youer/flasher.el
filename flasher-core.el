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

(defun flasher-core--scope ()
  "Convert `flasher-directories' to scope suitable for `org-map-entries'."
  (let (files)
    (dolist (dir flasher-directories)
      (setq files (append files (directory-files-recursively dir "\\.org$"))))
    files))

(defun flasher-core--map-cards (func)
  "Call FUNC at each entry marked with Flasher card tag."
  (with-temp-buffer
    (mapc #'insert-file-contents (flasher-core--scope))
    (goto-char (point-min))
    (let ((org-tags-exclude-from-inheritance (list flasher-card-tag))
          rtn rtn1)
      (while (re-search-forward flasher-card-headline-regexp nil t)
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

(provide 'flasher-core)

;;; flasher-core.el ends here
