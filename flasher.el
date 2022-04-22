;;; flasher.el --- Simple and extensible flashcard system  -*- lexical-binding: t; -*-
;;
;; Filename: flasher.el
;; Description: Simple and extensible flashcard system
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (org "9.4") (emacsql "3.0.0") (emacsql-sqlite "1.0.0"))
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

;;;;;;;;;;;;;
;; Flasher ;;
;;;;;;;;;;;;;

(defgroup flasher nil
  "Manage, learn and review flashcards in Emacs."
  :group 'external)

(defcustom flasher-directories '("~/.flasher")
  "Directories to search for flashcards."
  :group 'flasher
  :type 'directory)

;;;;;;;;;;;;;;;;;
;; Flasher API ;;
;;;;;;;;;;;;;;;;;

(defgroup flasher-card nil
  "Flasher card API."
  :group 'flasher)

(defcustom flasher-card-tag "card"
  "Tag for marking headlines as flashcards."
  :group 'flasher-card
  :type 'string)

(defcustom flasher-card-explain-tag "explain"
  "Tag for marking headlines as explanations."
  :group 'flasher-card
  :type 'string)

(defcustom flasher-card-task-tag "task"
  "Tag for marking headlines as tasks."
  :group 'flasher-card
  :type 'string)

(defcustom flasher-card-deck-property "CARD_DECK"
  "Property used to store card dec."
  :group 'flasher-card
  :type 'string)

(defcustom flasher-card-tags-property "CARD_TAGS"
  "Property used to store card tags."
  :group 'flasher-card
  :type 'string)

(defcustom flasher-card-type-property "CARD_TYPE"
  "Property used to store card type."
  :group 'flasher-card
  :type 'string)

(defcustom flasher-card-modifiers-property "CARD_MODS"
  "Property used to store card type modifiers."
  :group 'flasher-card
  :type 'string)

(defconst flasher-card--explain-heading-regexp
  (rx-to-string `(: bol (+ "*") (+ space) (* any)
                  (group ":" ,flasher-card-explain-tag ":") (* any) eol) t)
  "Regular expression to match headline tagged as card.")

(defconst flasher-card--task-heading-regexp
  (rx-to-string `(: bol (+ "*") (+ space) (* any)
                  (group ":" ,flasher-card-task-tag ":") (* any) eol) t)
  "Regular expression to match headline tagged as card.")

(defgroup flasher-db nil
  "Flasher database API."
  :group 'flasher)

(defcustom flasher-db-location (locate-user-emacs-file "flasher.db")
  "The path to file where Flasher database is stored."
  :group 'flasher-db
  :type 'file)

(defvar flasher-db--connection nil
  "Database connection to Flasher database.")

(defconst flasher-db--schemata
  '((files ([(file :unique)]))
    (decks ([(id integer :primary-key)
             (parent     :not-null)
             (name       :not-null)]
            (:unique [parent name])))
    (cards ([(uuid :primary-key)
             deck]
            (:foreign-key [deck] :references decks [id] :on-delete :cascade)))
    (tags ([(card :not-null)
            tag]
           (:foreign-key [card] :references cards [uuid] :on-delete :cascade)))
    (variants ([(id integer :primary-key)
                (card       :not-null)
                (name       :not-null)]
               (:unique [card name])
               (:foreign-key [card] :references cards [uuid] :on-delete :cascade)))
    (results ([(variant  :not-null)
               (result   :not-null)
               (ease     :not-null)
               (interval :not-null)
               (due      :not-null)]
              (:primary-key [variant due])
              (:foreign-key [variant] :references variants [id] :on-delete :cascade))))
  "Flasher database structure.")

;;;;;;;;;;;;;;;;;;
;; Database API ;;
;;;;;;;;;;;;;;;;;;

(defmacro flasher-db-transaction (&rest body)
  "Eval BODY as database transaction."
  (declare (indent defun))
  `(emacsql-with-transaction (flasher-db) ,@body))

(defun flasher-db ()
  "Entrypoint to the Flasher database.
Initializes and stores database and connection."
  (unless (and flasher-db--connection
               (emacsql-live-p flasher-db--connection))
    (let ((init-db (not (file-exists-p flasher-db-location))))
      (make-directory (file-name-directory flasher-db-location) t)
      (let ((conn (emacsql-sqlite flasher-db-location)))
        (emacsql conn [:pragma (= foreign_keys ON)])
        (emacsql conn [:pragma (= synchronous OFF)])
        (emacsql conn [:pragma (= journal_mode MEMORY)])
        (when-let ((process (emacsql-process conn)))
          (set-process-query-on-exit-flag process nil))
        (setq flasher-db--connection conn)
        (when init-db
          (flasher-db--init)))))
  flasher-db--connection)

(defun flasher-db--init ()
  "Initialize Flasher database."
  (flasher-db-transaction
    (dolist (table flasher-db--schemata)
      (apply #'flasher-db-query [:create-table $i1 $S2] table))))

(defun flasher-db--close ()
  "Close Flasher database connection."
  (when (and flasher-db--connection
             (emacsql-live-p flasher-db--connection))
    (emacsql-close flasher-db--connection)
    (setq flasher-db--connection nil)))

(defun flasher-db-query (sql &rest args)
  "Execute SQL query on Flasher database with ARGS."
  (apply #'emacsql (flasher-db) sql args))

(provide 'flasher)


;;; flasher.el ends here
