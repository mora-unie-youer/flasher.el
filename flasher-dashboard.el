;;; flasher-dashboard.el --- Flasher dashboard mode -*- lexical-binding: t; -*-
;;
;; Filename: flasher-dashboard.el
;; Description: Flasher dashboard mode
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
;; Flasher dashboard mode

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

(defgroup flasher-dashboard nil
  "Flasher dashboard mode."
  :group 'flasher)

(defcustom flasher-dashboard-buffer-name "*Flasher Dashboard*"
  "Name of the buffer to use for displaying the dashboard view."
  :group 'flasher-dashboard
  :type 'string)

(define-derived-mode flasher-dashboard-mode special-mode "Flasher Dashboard"
  "This mode is used to display Flasher dashboard."
  :group 'flasher-dashboard)

(defun flasher-dashboard-view ()
  "Show the Flasher dashboard view in the current buffer."
  (let ((buf (get-buffer-create flasher-dashboard-buffer-name))
        (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (insert (propertize "Flasher Dashboard\n\n" 'face 'org-level-1))
      (let ((total 0) (new 0) (failed 0) (overdue 0) (young 0) (old 0))
        (insert "Cards:\n")
        (dolist (card (flasher-db-get-all-cards))
          (cl-incf total)
          (let* ((card-status (flasher-core-card-status card))
                 (status (nth 0 card-status)))
            (cl-case status
              (:new     (cl-incf new))
              (:failed  (cl-incf failed))
              (:overdue (cl-incf overdue))
              (:young   (cl-incf young))
              (:old     (cl-incf old)))
            (insert (format "\tCard %s %s\n" card card-status))))
        (insert (format "Statistics: %d total, %d new, %d failed, %d overdue, %d young and %d old cards\n"
                        total new failed overdue young old))))))

;;;###autoload
(defun flasher-dashboard ()
  "Open Flasher dashboard."
  (interactive)
  (flasher-dashboard-view)
  (switch-to-buffer flasher-dashboard-buffer-name)
  (goto-char (point-min))
  (flasher-dashboard-mode))

(provide 'flasher-dashboard)

;;; flasher-dashboard.el ends here
