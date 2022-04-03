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

(defun flasher-core-scope ()
  "Convert `flasher-directories' to scope suitable for `org-map-entries'."
  (let (files)
    (dolist (dir flasher-directories)
      (setq files (append files (directory-files-recursively dir ".org$"))))
    files))

(defun flasher-core-map-entries (func)
  "Call FUNC at each entry marked with Flasher card tag."
  (let ((org-tags-exclude-from-inheritance (list flasher-card-tag)))
    (org-map-entries func (concat "+" flasher-card-tag) (flasher-core-scope))))

(defun flasher-core-get-card-id ()
  "Function that can be mapped in `flasher-core-map-entries' to get card IDs."
  (org-id-get-create))

(defun flasher-core-sync-cards ()
  "Add all new cards to Flasher database."
  (dolist (id (flasher-core-map-entries #'flasher-core-get-card-id))
    (let ((card (flasher-db-get-card id)))
      (unless card (flasher-db-create-card id)))))

(provide 'flasher-core)

;;; flasher-core.el ends here
