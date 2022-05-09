;;; flasher.el --- Simple and extensible flashcard system  -*- lexical-binding: t; -*-
;;
;; Filename: flasher.el
;; Description: Simple and extensible flashcard system
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.2") (org "9.4") (emacsql "3.0.0") (emacsql-sqlite "1.0.0"))
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
(require 'org-id)
(require 'org-agenda)

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

(defcustom flasher-algo-minimum-ease 1.25
  "Minimum ease factor card can have."
  :group 'flasher-algo
  :type 'flasher-algo-ease)

(defcustom flasher-algo-initial-ease 2.0
  "Initial ease factor that will be set to card."
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

(defcustom flasher-card-explain-heading-title "Explain"
  "Heading title for card's explanation."
  :group 'flasher-card
  :type 'string)

(defcustom flasher-card-task-tag "task"
  "Tag for marking headlines as tasks."
  :group 'flasher-card
  :type 'string)

(defcustom flasher-card-task-heading-title "Task"
  "Heading title for card's task."
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

(defcustom flasher-card-title-property "CARD_TITLE"
  "Property used to store card title."
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

(defcustom flasher-card-intervals-before-old 10
  "When item's interval is above this value, it's no longer considered 'young'."
  :group 'flasher-card
  :type 'flasher-card-interval)

(defcustom flasher-card-interval-overdue-factor 0.2
  "Multiply factor to check if item is overdue.
Item is considered overdue, when its scheduled review date is more than
FLASHER-CARD-INTERVAL-OVERDUE-FACTOR * LAST-INTERVAL days in the past."
  :group 'flasher-card
  :type 'float)

(defconst flasher-card--explain-heading-regexp
  (rx-to-string `(: bol (+ "*") (+ space) (* any)
                  (group ":" ,flasher-card-explain-tag ":") (* any) eol) t)
  "Regular expression to match headline tagged as card.")

(defconst flasher-card--task-heading-regexp
  (rx-to-string `(: bol (+ "*") (+ space) (* any)
                  (group ":" ,flasher-card-task-tag ":") (* any) eol) t)
  "Regular expression to match headline tagged as card.")

(defgroup flasher-card-type nil
  "Flasher card types API."
  :group 'flasher)

(defvar flasher-card-types '()
  "Alist for registered card types.
Entries have shape (name sort-p init-fn setup-fn hint-fn flip-fn).
See `flasher-card-types-register' for adding new card types.")

(defgroup flasher-card-type-normal nil
  "Flasher 'normal card types API."
  :group 'flasher-card-type)

(defvar flasher-card-type-normal--side nil
  "Current 'normal card side.")

(defgroup flasher-card-type-cloze nil
  "Flasher 'cloze card types API."
  :group 'flasher-card-type)

(defcustom flasher-card-type-cloze-type-property "CLOZE_TYPE"
  "Property used to store cloze type."
  :group 'flasher-card-type-cloze
  :type 'string)

(defface flasher-card-type-cloze-hole-face
  '((t (:bold t)))
  "Face for Flasher cloze card holes."
  :group 'flasher-card-type-cloze)

(defconst flasher-card-type-cloze--regex
  (rx "{"
      (: "{" (group (+ (not (any "}")))) "}")
      (? "{" (group (+ (not (any "}")))) "}")
      (? "@" (group (+ digit)))
      "}")
  "Regular expression to match cloze holes.")

(defvar flasher-card-type-cloze--variant nil
  "Current 'cloze card variant.")

(defvar flasher-card-type-cloze--text-overlay '()
  "Text overlay for 'cloze card.")

(defvar flasher-card-type-cloze--hint-overlay '()
  "Hint overlay for 'cloze card.")

(defgroup flasher-core nil
  "Flasher core API."
  :group 'flasher)

(defgroup flasher-dashboard nil
  "Flasher dashboard API."
  :group 'flasher)

(defcustom flasher-dashboard-buffer-name "*Flasher: Dashboard*"
  "Name of the buffer to use for displaying the dashboard view."
  :group 'flasher-dashboard
  :type 'string)

(defvar flasher-dashboard--cards nil
  "List of cards currently shown in dashboard.")

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
  '((files ([(file text :unique)]))
    (decks ([(id     integer :primary-key)
             (parent integer)
             (name   object  :not-null)]
            (:foreign-key [parent] :references decks [id] :on-delete :cascade)))
    (cards ([(uuid  object   :primary-key)
             (deck  integer)
             (title object   :not-null)]
            (:foreign-key [deck] :references decks [id] :on-delete :cascade)))
    (tags ([(card object :not-null)
            (tag  text)]
           (:unique [card tag])
           (:foreign-key [card] :references cards [uuid] :on-delete :cascade)))
    (variants ([(id   integer :primary-key)
                (card object  :not-null)
                (side object  :not-null)
                data]
               (:unique [card side data])
               (:foreign-key [card] :references cards [uuid] :on-delete :cascade)))
    (results ([(variant  integer :not-null)
               (result   integer :not-null)
               (ease     float   :not-null)
               (interval integer :not-null)
               (due              :not-null)]
              (:primary-key [variant due])
              (:foreign-key [variant] :references variants [id] :on-delete :cascade))))
  "Flasher database structure.")

(defconst flasher-db--additional
  '("CREATE UNIQUE INDEX decks_parent_name ON decks (name, IFNULL(parent, 0));")
  "Flasher additional database structure.")

(defgroup flasher-deck nil
  "Flasher deck API."
  :group 'flasher)

(defcustom flasher-deck-delimiter "::"
  "Delimiter used for complex deck names."
  :group 'flasher-deck
  :type 'string)

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
      (apply #'flasher-db-query [:create-table $i1 $S2] table))
    (dolist (query flasher-db--additional)
      (if (stringp query)
          (flasher-db-query-string query)
        (apply #'flasher-db-query query)))))

(defun flasher-db--close ()
  "Close Flasher database connection."
  (when (and flasher-db--connection
             (emacsql-live-p flasher-db--connection))
    (emacsql-close flasher-db--connection)
    (setq flasher-db--connection nil)))

(defun flasher-db-query (sql &rest args)
  "Execute SQL query on Flasher database with ARGS."
  (apply #'emacsql (flasher-db) sql args))

(defun flasher-db-query-string (sql)
  "Execute SQL query in string on Flasher database."
  (let ((connection (flasher-db)))
    (emacsql-clear connection)
    (emacsql-send-message connection sql)
    (emacsql-wait connection)
    (emacsql-parse connection)))

;;;;;;;;;;;;;;
;; Core API ;;
;;;;;;;;;;;;;;

(defun flasher-core--list-all-files ()
  "List all .org files in `flasher-directories'."
  (mapcan (lambda (dir) (directory-files-recursively dir "\\.org$")) flasher-directories))

(defun flasher-core--list-indexed-files ()
  "List all .org files that were indexed before."
  (mapcar #'car (flasher-db-query [:select * :from files])))

(defun flasher-core--file-contains-card-p (file)
  "Return non-nil if FILE contain at least one card."
  (member t (org-map-entries (lambda () t)
                             (concat "+" flasher-card-tag) (list file))))

;;;###autoload
(defun flasher-index-sync ()
  "Search for cards in files and add the file if it contain a card."
  (interactive)
  (flasher-db-transaction
    (flasher-db-query [:delete-from files])
    (dolist (file (flasher-core--list-all-files))
      (when (flasher-core--file-contains-card-p file)
        (flasher-db-query [:insert-into files :values $v1] (vector file)))))
  (org-id-update-id-locations (flasher-core--list-indexed-files)))

(defun flasher-core--map-cards (func)
  "Apply FUNC to each entry marked as Flasher card in indexed files."
  (let ((org-tags-exclude-from-inheritance (list flasher-card-tag)))
    (org-map-entries func (concat "+" flasher-card-tag)
                     (flasher-core--list-indexed-files))))

(defun flasher-core--add-tag (tag)
  "Add TAG to the current heading."
  (org-set-tags (cl-remove-duplicates (cons tag (org-get-tags nil 'local))
                                      :test #'string=)))

(defun flasher-core--remove-tag (tag)
  "Remove TAG from the current heading."
  (org-set-tags (remove tag (org-get-tags nil 'local))))

(defun flasher-core--heading-text (heading)
  "Return text from HEADING point marker."
  (org-agenda-get-some-entry-text heading most-positive-fixnum))

(defun flasher-core--heading-match (level title &optional compare-fn)
  "Return non-nil if heading matched LEVEL and TITLE conditions.
COMPARE-FN is used to compare levels."
  (unless compare-fn (setq compare-fn #'=))
  (let ((components (org-heading-components)))
    (and (funcall compare-fn (cl-first components) level)
         (or (null title) (string= (cl-fifth components) title)))))

(defun flasher-core--subheadings ()
  "Return list of subheadings."
  (let* ((heading-components (org-heading-components))
         (level (cl-first heading-components)))
    (delq nil
          (org-map-entries
           (lambda ()
             (let* ((subheading-components (org-heading-components))
                    (sublevel (cl-first subheading-components)))
               (when (= sublevel (1+ level))
                 (cl-fifth subheading-components))))
           t 'tree))))

;;;;;;;;;;;;;;
;; Deck API ;;
;;;;;;;;;;;;;;

(defun flasher-deck--get-children (id &optional filter &rest args)
  "Get deck children for deck with ID. If ID is nil, fetch top-level decks.
FILTER can contain vector with part of SQL for filtering children out.
ARGS is the rest of arguments used in SQL query.
NOTE: argument numbers in FILTER must start from 2 (as first is used for ID)."
  (let* ((base-query [:select id :from decks :where])
         (parent-query (if id [(= parent $s1)] [(is parent nil)]))
         (query (vconcat base-query parent-query (if filter [:and]) filter))
         (children (apply #'flasher-db-query query id args)))
    (mapcar #'car children)))

(defun flasher-deck-create (name)
  "Create deck with NAME."
  (let* ((query [:insert-into decks [parent name] :values $v1])
         (parts (split-string name flasher-deck-delimiter))
         (name (car (last parts))))
    (let* ((other (butlast parts))
           (other-string (string-join other flasher-deck-delimiter))
           (parent (if other (flasher-deck-get-create other-string))))
      (flasher-db-query query (vector parent name))
      (car (flasher-deck--get-children parent [(= name $s2)] name)))))

(defun flasher-deck-get (name)
  "Get deck with NAME. If NAME is not a string, return nil."
  (when (stringp name)
    (let* ((parts (split-string name flasher-deck-delimiter))
           (name (car (last parts))))
      (car (let* ((other (butlast parts))
                  (other-string (string-join other flasher-deck-delimiter))
                  (parent (if other (flasher-deck-get other-string))))
             (flasher-deck--get-children parent [(= name $s2)] name))))))

(defun flasher-deck-get-create (name)
  "Get or create deck with NAME."
  (if-let ((deck (flasher-deck-get name)))
      deck
    (flasher-deck-create name)))

;;;;;;;;;;;;;;
;; Card API ;;
;;;;;;;;;;;;;;

(defun flasher-card--subheading (title &optional heading)
  "Return point marker at the beginning of card's TITLE subheading at HEADING."
  (unless heading (setq heading (point-marker)))
  (save-excursion
    (with-current-buffer (marker-buffer heading)
      (goto-char heading)
      (let ((level (cl-first (org-heading-components)))
            found)
        (org-map-entries (lambda () (when (and (null found)
                                          (flasher-core--heading-match (1+ level) title))
                                 (setq found (point-marker))))
                         t 'tree)
        found))))

(defmacro flasher-card--heading (heading title)
  "Create function with DOC to fetch HEADING with TITLE."
  (let* ((doc (format "Return point marker at the card's %s heading." (symbol-name heading)))
         (tag (intern (concat "flasher-card-" (symbol-name heading) "-tag")))
         (prefix (concat "flasher-card--" (symbol-name heading)))
         (func (intern (concat prefix "-heading")))
         (regexp (intern (concat prefix "-heading-regexp"))))
    `(defun ,func ()
       ,doc
       (if-let ((tag-p (member ,tag (org-get-tags)))
                (level (cl-first (org-heading-components))))
           (save-excursion
             (let (heading)
               (while (and (null heading) (re-search-backward ,regexp nil t))
                 (when (flasher-core--heading-match level nil #'<)
                   (setq heading (point-marker))))
               heading))
         (flasher-card--subheading ,title)))))

(flasher-card--heading explain flasher-card-explain-heading-title)
(flasher-card--heading task flasher-card-task-heading-title)

(defun flasher-card--task (&optional side)
  "Return card's task for SIDE (nth . title)."
  (when-let ((heading (flasher-card--task-heading))
             (heading-text (flasher-core--heading-text heading)))
    (cond
     ((null side) heading-text)
     (t (if-let ((side-heading (flasher-card--subheading (cdr side) heading)))
            (flasher-core--heading-text side-heading)
          (elt (split-string heading-text "\n") (car side)))))))

(defun flasher-card--sides ()
  "Return card's sides."
  (let ((subheadings (flasher-core--subheadings)))
    (cl-pushnew "Front" subheadings :test #'string=)
    (cl-pushnew "Back" subheadings :test #'string=)
    (cl-set-difference subheadings (list flasher-card-explain-heading-title
                                         flasher-card-task-heading-title)
                       :test #'string=)))

(defun flasher-card--front ()
  "Return card's front side."
  (if-let ((heading (flasher-card--subheading "Front")))
      (flasher-core--heading-text heading)
    (cl-fifth (org-heading-components))))

(defun flasher-card--back ()
  "Return card's back side."
  (if-let ((heading (flasher-card--subheading "Back")))
      (flasher-core--heading-text heading)
    (flasher-core--heading-text (point-marker))))

(defun flasher-card--side (side)
  "Return card's SIDE."
  (if-let ((heading (flasher-card--subheading side)))
      (flasher-core--heading-text heading)
    (error "Card doesn't have '%s' side" side)))

(defun flasher-card-p ()
  "Return non-nil if current heading is a card."
  (member flasher-card-tag (org-get-tags nil 'local)))

(provide 'flasher)


;;; flasher.el ends here
