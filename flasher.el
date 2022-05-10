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

(defface flasher-dashboard--new-count
  '((t :inherit 'font-lock-function-name-face))
  "Face used to highlight new count in dashboard."
  :group 'flasher-dashboard)

(defface flasher-dashboard--failed-count
  '((t :inherit 'font-lock-keyword-face))
  "Face used to highlight failed count in dashboard."
  :group 'flasher-dashboard)

(defface flasher-dashboard--review-count
  '((t :inherit 'font-lock-variable-name-face))
  "Face used to highlight review count in dashboard."
  :group 'flasher-dashboard)

(defface flasher-dashboard--overdue-count
  '((t :inherit 'font-lock-constant-face))
  "Face used to highlight overdue count in dashboard."
  :group 'flasher-dashboard)

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

;;;;;;;;;;;;;;;;;;;
;; Algorithm API ;;
;;;;;;;;;;;;;;;;;;;

(defun flasher-algo (variant-info quality)
  "Determine the next iteration of VARIANT-INFO based on QUALITY.
For CARD-STATS see `flasher-card-variant--get-info'. Result has the same shape.
QUALITY is the quality of the answer:
  5 - perfect answer
  4 - correct answer took a while
  3 - correct answer recalled with serious difficulty
  2 - incorrect answer; where the correct one seemed easy to recall
  1 - incorrect answer; remembered the correct one
  0 - complete blackout"
  (pcase-let ((`(,variant ,status ,due ,ease ,failed ,interval) variant-info))
    (let (next-ease next-interval)
      (setq next-ease (max flasher-algo-minimum-ease
                           (+ ease (alist-get quality flasher-algo-ease-deltas))))
      (when flasher-algo-maximum-ease
        (setq next-ease (min flasher-algo-maximum-ease next-ease)))
      (setq next-interval (cond ((or failed (< quality 3)) 0)
                                ((= interval 0) 1)
                                ((= interval 1) 4)
                                (t (flasher-algo-fuzz (* next-ease interval)))))
      (when flasher-algo-max-interval-count
        (setq next-interval (min flasher-algo-max-interval-count next-interval)))
      (cons variant (list status due next-ease (< quality 3) next-interval)))))

(defun flasher-algo-fuzz (interval)
  "Apply fuzz to INTERVAL.
Multiply INTERVAL by a random factor between `flasher-algo-minimum-fuzz' and
`flasher-algo-maximum-fuzz'"
  (let ((minimum flasher-algo-minimum-fuzz) (maximum flasher-algo-maximum-fuzz))
    (round (* interval (+ minimum (cl-random (- maximum minimum)))))))

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
  (substring-no-properties
   (org-agenda-get-some-entry-text heading most-positive-fixnum)))

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
    (pcase side
      ("Front" (flasher-card--front))
      ("Back"  (flasher-card--back))
      (_ (error "Card doesn't have '%s' side" side)))))

(defun flasher-card-p ()
  "Return non-nil if current heading is a card."
  (member flasher-card-tag (org-get-tags nil 'local)))

(defun flasher-card-init (type &optional force)
  "Initialize card at point with TYPE. If FORCE is non-nil, ignore exist error."
  (if (or (not (flasher-card-p)) force)
      (when (flasher-card-type type)
        (org-back-to-heading)
        (org-set-property flasher-card-type-property type)
        (flasher-core--add-tag flasher-card-tag)
        (org-id-add-location (org-id-get-create) buffer-file-name)
        (flasher-card--update))
    (error "Heading is already a card")))

(defmacro flasher-card-goto-id (id &rest body)
  "Eval BODY with moving to card if ID is non-nil."
  (declare (indent defun))
  `(save-excursion
     (when ,id (org-id-goto ,id))
     ,@body))

(defmacro flasher-card-with-id (id &rest body)
  "Eval BODY with card if ID is non-nil. Set ID if it is nil."
  (declare (indent defun))
  `(progn
     (unless ,id (setq ,id (org-id-get)))
     ,@body))

(defmacro flasher-card-goto-with-id (id &rest body)
  "Eval BODY with moving to card if ID is non-nil. Set ID if it is nil."
  (declare (indent defun))
  `(flasher-card-goto-id ,id
     (flasher-card-with-id ,id
       ,@body)))

(defun flasher-card--get-all ()
  "Get all cards."
  (mapcar #'car (flasher-db-query [:select uuid :from cards])))

(defun flasher-card--get-by-deck (id)
  "Get cards by deck ID."
  (let* ((base-query [:select uuid :from cards :where])
         (deck-query (if id [(= deck $s1)] [(is deck nil)]))
         (query (vconcat base-query deck-query)))
    (mapcar #'car (flasher-db-query query id))))

(defun flasher-card--get-deck (&optional id)
  "Get deck name of card at point or with ID."
  (flasher-card-goto-id id
    (or (org-entry-get nil flasher-card-deck-property)
        (cadar (org-collect-keywords (list flasher-card-deck-property))))))

(defun flasher-card--get-modifiers (&optional id)
  "Get modifiers of card at point or with ID."
  (flasher-card-goto-id id
    (org-entry-get-multivalued-property nil flasher-card-modifiers-property)))

(defun flasher-card--get-tags (&optional id)
  "Get tags of card at point or with ID."
  (flasher-card-goto-id id
    (append
     (split-string (cadar (org-collect-keywords (list flasher-card-tags-property))))
     (org-entry-get-multivalued-property nil flasher-card-tags-property))))

(defun flasher-card--get-title (&optional id)
  "Get title of card at point or with ID."
  (flasher-card-goto-id id
    (or (org-entry-get nil flasher-card-title-property)
        (cl-fifth (org-heading-components)))))

(defun flasher-card--get-type (&optional id)
  "Get type of card at point or with ID."
  (flasher-card-goto-id id (org-entry-get nil flasher-card-type-property)))

(defun flasher-card--get-variants (&optional id)
  "Get card variants as (SIDE . (ID DATA)...)... for card at point or with ID."
  (flasher-card-with-id id
    (flasher-db-query [:select [id card side data] :from variants
                       :where (= card $s1) :order-by (asc id)] id)))

(defun flasher-card--get-nfro (&optional id)
  "Get card variants (NEW FAILED REVIEW OVERDUE) for card at point or with ID."
  (flasher-card-with-id id
    (let* ((variants (flasher-card--get-variants id))
           (due-variants (flasher-card-variant--filter-due variants))
           (new 0) (failed 0) (review 0) (overdue 0))
      (dolist (variant due-variants)
        (let ((status (cl-fifth variant)))
          (cond
           ((eq status :new) (cl-incf new))
           ((eq status :failed) (cl-incf failed))
           ((eq status :overdue) (cl-incf overdue))
           (t (cl-incf review)))))
      (list new failed review overdue))))

(defun flasher-card--create (&optional id)
  "Create card at point or with ID."
  (flasher-card-goto-with-id id
    (let ((title (flasher-card--get-title)))
      (flasher-db-query [:insert-or-ignore-into cards [uuid deck title] :values $v1]
                        (vector id nil title)))))

(defun flasher-card--update-deck (&optional id)
  "Update deck for card at point or with ID."
  (flasher-card-goto-with-id id
    (let* ((deck-name (flasher-card--get-deck))
           (deck (flasher-deck-get-create deck-name))
           (current-deck (caar (flasher-db-query [:select deck :from cards
                                                  :where (= uuid $s1)] id))))
      (when (not (eq deck current-deck))
        (flasher-db-query [:update cards :set (= deck $s2) :where (= uuid $s1)]
                          id deck)))))

(defun flasher-card--update-tags (&optional id)
  "Update tags for card at point or with ID."
  (flasher-card-goto-with-id id
    (let ((tags (flasher-card--get-tags))
          (current-tags (apply #'append (flasher-db-query [:select tag :from tags
                                                           :where (= card $s1)] id))))
      (let ((new-tags (cl-set-difference tags current-tags :test #'string=))
            (old-tags (cl-set-difference current-tags tags :test #'string=)))
        (when (not (and (null new-tags) (null old-tags)))
          (dolist (tag old-tags)
            (flasher-db-query [:delete-from tags
                               :where (= card $s1) :and (= tag $s2)] id tag))
          (dolist (tag new-tags)
            (flasher-db-query [:insert-into tags :values $v1] (vector id tag))))))))

(defun flasher-card--update-title (&optional id)
  "Update title for card at point or with ID."
  (flasher-card-goto-with-id id
    (let ((title (flasher-card--get-title))
          (current-title (caar (flasher-db-query [:select title :from cards
                                                  :where (= uuid $s1)] id))))
      (when (not (string= title current-title))
        (flasher-db-query [:update cards :set (= title $s2) :where (= uuid $s1)]
                          id title)))))

(defun flasher-card--extract-variants (variants)
  "Extract VARIANTS from grouped VARIANTS list."
  (let (result)
    (dolist (variant variants)
      (if-let* ((side (car variant))
                (datas (cdr variant))
                (list-p (listp datas)))
          (dolist (data datas)
            (push (cons side data) result))
        (push (cons side nil) result)))
    result))

(defun flasher-card--update-variants (variants &optional id)
  "Update VARIANTS for card at point or with ID."
  (flasher-card-with-id id
    (let* ((variants (flasher-card--extract-variants variants))
           (current-variants (mapcar (lambda (variant) (cons (cadr variant) (caddr variant)))
                                     (flasher-card--get-variants))))
      (let ((new-variants (cl-set-difference variants current-variants :test #'equal))
            (old-variants (cl-set-difference current-variants variants :test #'equal)))
        (when (not (and (null new-variants) (null old-variants)))
          (dolist (variant old-variants)
            (let* ((base-query [:delete-from variants :where (= card $s1)
                                :and (= side $s2) :and])
                   (data-query (if (cdr variant) [(= data $s3)] [(is data nil)]))
                   (query (vconcat base-query data-query)))
              (flasher-db-query query id (car variant) (cdr variant))))
          (dolist (variant new-variants)
            (flasher-db-query [:insert-into variants [card side data] :values $v1]
                              (vector id (car variant) (cdr variant)))))))))

(defun flasher-card--update (&optional id)
  "Update card information in database for card at point or with ID."
  (flasher-card-with-id id
    (flasher-card--create)
    (flasher-card--update-deck)
    (flasher-card--update-tags)
    (flasher-card--update-title)
    (let ((type (flasher-card--get-type)))
      (funcall (flasher-card-type-init-fn type)))))

;;;###autoload
(defun flasher-card-sync ()
  "Synchronize card database."
  (interactive)
  (flasher-db-transaction
    (flasher-core--map-cards #'flasher-card--update)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Card variants API ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun flasher-card-variant--get-info (variant)
  "Return (VARIANT . STATUS) for card VARIANT."
  (cons variant (flasher-card-variant--status variant)))

(defun flasher-card-variant--save-result (new-card-info result)
  "Save card variant RESULT using NEW-CARD-INFO."
  (pcase-let* ((`(,card status due ,ease failed ,interval) new-card-info)
               (id (car card))
               (due (time-add (current-time) (days-to-time interval))))
    (flasher-db-query [:insert-into results :values $v1]
                      (vector id result ease interval due))))

(defun flasher-card-variant--last-result (id)
  "Return last result of card variant with ID."
  (car (flasher-db-query [:select * :from results :where (= variant $s1)
                          :order-by (desc due) :limit 1] id)))

(defun flasher-card-variant--due (id &optional last-result)
  "Return TIME, card variant with ID is scheduled to.
LAST-RESULT can be specified to reduce number of database calls."
  (unless last-result (setq last-result (flasher-card-variant--last-result id)))
  (if last-result (cl-fifth last-result) (current-time)))

(defun flasher-card-variant--overdue (id &optional last-result)
  "Return for card variant with ID:
- 0 if card is new, or if it scheduled for review today.
- A negative integer - card is scheduled that many days in the future.
- A positive integer - card is scheduled that many days in the past.
LAST-RESULT can be specified to reduce number of database calls."
  (unless last-result (setq last-result (flasher-card-variant--last-result id)))
  (- (time-to-days (current-time)) (time-to-days (flasher-card-variant--due id last-result))))

(defun flasher-card-variant--overdue-p (id &optional days-overdue last-result)
  "Return non-nil if card variant with ID should be considered 'overdue'.
Card is scheduled DAYS-OVERDUE days in the past. If argument is not given it is
extracted from the card.
LAST-RESULT can be specified to reduce number of database calls."
  (unless last-result (setq last-result (flasher-card-variant--last-result id)))
  (unless days-overdue (setq days-overdue (flasher-card-variant--overdue id last-result)))
  (let ((interval (cl-fourth last-result)))
    (and (> days-overdue 0)
         (> (/ days-overdue interval) flasher-card-interval-overdue-factor))))

(defun flasher-card-variant--status (id &optional last-result)
  "Fetch status list (STATUS DUE EASE FAILED INTERVAL) of card variant with ID.
DUE is the number of days overdue, see `flasher-card-variant--overdue'.
STATUS is one of the following values:
- :new
- :failed
- :overdue
- :young
- :old
EASE is current ease factor.
FAILED is non-nil when card either new or failed.
INTERVAL is current card interval count.
LAST-RESULT can be specified to reduce number of database calls."
  (unless last-result (setq last-result (flasher-card-variant--last-result id)))
  (let* ((result (if last-result (cl-second last-result) 0))
         (interval (if last-result (cl-fourth last-result) 0))
         (ease (if last-result (cl-third last-result) flasher-algo-initial-ease))
         (due (flasher-card-variant--overdue id last-result))
         (status (cond ((null last-result) :new)
                       ((= interval 0) :failed)
                       ((flasher-card-variant--overdue-p id due last-result) :overdue)
                       ((<= interval flasher-card-intervals-before-old) :young)
                       (t :old))))
    (list status due ease (< result 3) interval)))

(defun flasher-card-variant--filter (variants func)
  "Filter card VARIANTS using FUNC. Variant status is passed to FUNC."
  (declare (indent defun))
  (mapcar #'flasher-card-variant--get-info (seq-filter func variants)))

(defun flasher-card-variant--filter-due (variants)
  "Filter card VARIANTS with saving only due."
  (flasher-card-variant--filter variants
    (lambda (variant) (>= (flasher-card-variant--overdue (cl-first variant)) 0))))

;;;;;;;;;;;;;;;;;;;
;; Card type API ;;
;;;;;;;;;;;;;;;;;;;

(defun flasher-card-type (type)
  "Return card TYPE. If TYPE doesn't exist, errorring."
  (if-let ((card-type (alist-get type flasher-card-types nil nil #'string=)))
      card-type
    (error "Card type '%s' doesn't exist" type)))

(defun flasher-card-type-sort (type)
  "Get sorting for card TYPE."
  (cl-first (flasher-card-type type)))

(defun flasher-card-type-sort-p (type)
  "Do card variants of TYPE need to be sorted?"
  (not (null (flasher-card-type-sort type))))

(defun flasher-card-type-init-fn (type)
  "Get INIT-FN for card TYPE."
  (cl-second (flasher-card-type type)))

(defun flasher-card-type-setup-fn (type)
  "Get SETUP-FN for card TYPE."
  (cl-third (flasher-card-type type)))

(defun flasher-card-type-hint-fn (type)
  "Get HINT-FN for card TYPE."
  (cl-fourth (flasher-card-type type)))

(defun flasher-card-type-flip-fn (type)
  "Get FLIP-FN for card TYPE."
  (cl-fifth (flasher-card-type type)))

(defun flasher-card-type-register (name sort init-fn setup-fn hint-fn flip-fn)
  "Register a new card type.
NAME is name of the new type.
SORT - do card variants need to be sorted? Options: nil, 'side, non-nil.
INIT-FN is function for initializing card in database.
SETUP-FN is function for preparing card for review.
HINT-FN is function for showing hint for a card in review.
FLIP-FN is function for flipping card in review."
  (declare (indent defun))
  (push (list name sort init-fn setup-fn hint-fn flip-fn) flasher-card-types))

(defun flasher-card-type-noop (&rest _args)
  "No operation function that is used in card types.")

;;;;;;;;;;;;;;;;;;;
;; Dashboard API ;;
;;;;;;;;;;;;;;;;;;;

(defun flasher-dashboard--fetch-deck (id)
  "Fetch (ID NAME NFRO CARDS ALL-CARDS CHILDREN) for deck with ID."
  (let* ((name (if id (caar (flasher-db-query [:select name :from decks
                                               :where (= id $s1)] id))
                 "*Default*"))
         (cards (flasher-card--get-by-deck id))
         (cards-nfro (mapcar #'flasher-card--get-nfro cards))
         (nfro (apply #'cl-mapcar #'+ '(0 0 0 0) cards-nfro))
         (children-id (when id (flasher-deck--get-children id)))
         (children (mapcar #'flasher-dashboard--fetch-deck children-id))
         (children-nfro (if children
                            (apply #'cl-mapcar #'+ (mapcar #'cl-third children))
                          '(0 0 0 0)))
         (children-cards (apply #'append (mapcar #'cl-fifth children)))
         (total-nfro (cl-mapcar #'+ nfro children-nfro))
         (total-cards (append cards children-cards)))
    (list id name total-nfro cards total-cards children)))

(defun flasher-dashboard--fetch-decks ()
  "Fetch info for all decks."
  (let ((decks (flasher-deck--get-children nil)))
    (mapcar #'flasher-dashboard--fetch-deck (cons nil decks))))

(defmacro flasher-dashboard-with-buffer (&rest body)
  "Eval BODY in Flasher dashboard buffer."
  (declare (indent defun))
  `(let ((inhibit-read-only t))
     (with-current-buffer (get-buffer-create flasher-dashboard-buffer-name)
       ,@body
       (set-buffer-modified-p nil))))

(defvar flasher-dashboard--deck-keymap
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for deck buttons in dashboard main menu.")

(defun flasher-dashboard--decks-view-deck (deck &optional indent)
  "View DECK in dashboard buffer. INDENT is current level of deck."
  (unless indent (setq indent 0))
  (pcase-let* ((`(,id ,name ,nfro ,cards ,all-cards ,children) deck)
               (`(,n ,f ,r ,o) (mapcar (apply-partially #'format "%4d") nfro)))
    (flasher-dashboard-with-buffer
      (insert (format "%s %s %s %s"
                      (propertize n 'face 'flasher-dashboard--new-count)
                      (propertize f 'face 'flasher-dashboard--failed-count)
                      (propertize r 'face 'flasher-dashboard--review-count)
                      (propertize o 'face 'flasher-dashboard--overdue-count)))
      (insert (make-string (1+ indent) ?\t)
              (format "%s" (propertize name 'face 'bold)))
      (let ((begin (save-excursion (beginning-of-line) (point)))
            (end (point)))
        (put-text-property begin end 'flasher-deck id)
        (put-text-property begin end 'flasher-cards cards)
        (put-text-property begin end 'flasher-all-cards all-cards)
        (put-text-property begin end 'keymap flasher-dashboard--deck-keymap)
        (put-text-property begin end 'help-echo "mouse-1: show this deck"))
      (insert "\n"))
    (dolist (child children)
      (flasher-dashboard--decks-view-deck child (1+ indent)))))

(defvar flasher-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'kill-current-buffer)
    map)
  "Keymap for `flasher-dashboard-mode'.")

(define-derived-mode flasher-dashboard-mode special-mode "Flasher Dashboard"
  "This mode is used to display Flasher dashboard."
  :group 'flasher-dashboard)

(provide 'flasher)


;;; flasher.el ends here
