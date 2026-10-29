;;; libro.el --- Library and reading tracking with vulpea    -*- lexical-binding: t; -*-
;;
;; Author: drpaepper <drpaepper@drpaepper.xyz>
;; Maintainer: drpaepper <drpaepper@drpaepper.xyz>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; License: GPLv3
;;
;; Created: 23 Oct 2026
;;
;; URL: https://github.com/drpaepper/libro
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Read more.
;;
;;; Code:

(require 'vulpea)

;;;###autoload
(defun libro-setup ()
  "Setup `libro' library."
  (seq-each(lambda (x)
             (add-to-list 'org-tags-exclude-from-inheritance x))
           '("library"
             "author"
             "book")))
;;;###autoload
(defvar libro-reading-states
  '(unread
    reading
    read))

;;;###autoload
(defvar libro-author-template
  '(:file-name "library/authors/${timestamp}-${slug}.org"
    :tags ("library" "author"))
  "Capture template for author entity.

Template is a property list accepting following values:

  :file-name (mandatory) - file name relative to
  `vulpea-default-notes-directory'

  :head (optional) - extra note header

  :body (optional) - note body

  :meta (optional) - metadata (associative list)

  :properties (optional) - extra properties to put in PROPERTIES block

  :context (optional) - extra variables for :file-name, :head,
  :body

  :tags (optional) - extra tags in addition to library and author

See `vulpea-create' for more information.")

;;;###autoload
(defvar libro-book-template
  '(:file-name "library/books/${timestamp}-${slug}.org"
    :tags ("library" "book"))
  "Capture template for book entity.

Template is a property list accepting following values:

  :file-name (mandatory) - file name relative to
  `vulpea-default-notes-directory'

  :head (optional) - extra note header

  :body (optional) - note body

  :meta (optional) - metadata (associative list)

  :properties (optional) - extra properties to put in PROPERTIES block

  :context (optional) - extra variables for :file-name, :head,
  :body

  :tags (optional) - extra tags in addition to library and book

See `vulpea-create' for more information.")

(defun libro--read-string (prompt &optional initial-input)
  "Read a string from minibuffer with PROMPT, trimming the result.
INITIAL-INPUT is passed to `read-string'."
  (string-trim (read-string prompt initial-input)))

(defun libro--merge-properties (template-props)
  "Merge TEMPLATE-PROPS with properties from vulpea defaults.
TEMPLATE-PROPS takes precedence over defaults."
  (let ((default-props (plist-get vulpea-create-default-template :properties)))
    (if (and default-props template-props)
        ;; Merge: template props override defaults
        (append template-props
                (seq-remove (lambda (prop)
                              (assoc (car prop) template-props))
                            default-props))
      ;; Return whichever is non-nil, or nil
      (or template-props default-props))))

(defun libro--repeat-while (fn filter &rest args)
  "Repeat FN and return the first unfiltered result.

Repeat cycle stops when `C-g` is used or FILTER returns nil.

ARGS are passed to FN."
  (let (value
        (continue t)
        (inhibit-quit t))
    (with-local-quit
      (while continue
        (setq value (apply fn args))
        (when (null (funcall filter value))
          (setq continue nil))))
    (setq quit-flag nil)
    (when (null continue)
      value)))

;;;###autoload
(defun libro-author-find-file ()
  "Select and find an author note."
  (interactive)
  (find-file (vulpea-note-path (libro-author-select))))

(defun libro-author-select (&optional notes)
  "Select a author note from NOTES.

When NOTES are nil, database is queried to get a list of notes.

When author note does not exist, it may be created using
`vino-author-create' or a pseudonym is added to an existing author.

Return `vulpea-note'."
  (let* ((notes (or notes (vulpea-db-query-by-tags-every '("library" "author"))))
         (note (vulpea-select-from "Author" notes :expand-aliases t)))
    (if (vulpea-note-id note)
        note
      (pcase (completing-read
              (format "Author %s does not exist. What to do?"
                      (vulpea-note-title note))
              '("Create new author"
                "Add pseudonym to author (pen names)"
                "Ignore"))
        (`"Create new author"
         (libro-author-create (vulpea-note-title note)))
        (`"Add pseudonym to author (pen names)"
         (let ((base (vulpea-select-from
                      "Original author"
                      (vulpea-db-query-by-tags-every
                       '("library" "author"))
                      :expand-aliases t)))
           (vulpea-utils-with-note base
             (vulpea-buffer-alias-add (vulpea-note-title note))
             (save-buffer)
             (vulpea-db-update-file (buffer-file-name (buffer-base-buffer))))
           (setf (vulpea-note-title base) (vulpea-note-title note))
           base))
        (_ note)))))

(defun libro-authors-select ()
  "Select multiple author notes.

When author note does not exist, it may be created using
`libro-author-create'.

Return a list, where each entry is a `vulpea-note'."
  (vulpea-select-multiple-from
   "Author" (vulpea-db-query-by-tags-every '("library" "author"))
   :select-fn
   (lambda (_ notes &rest _)
     (libro-author-select notes))
   :expand-aliases t))

;;;###autoload
(cl-defun libro-author-create (&optional title)
  "Create an author note using `libro-author-template'.

Unless TITLE is specified, user is prompted to provide one.

Return `vulpea-note'."
  (interactive)
  (let ((title (or title (libro--read-string "Name: "))))
    (vulpea-create
     title
     (plist-get libro-author-template :file-name)
     :tags (seq-union (plist-get libro-author-template :tags) '("library" "author"))
     :head (plist-get libro-author-template :head)
     :meta (plist-get libro-author-template :meta)
     :body (plist-get libro-author-template :body)
     :context (plist-get libro-author-template :context)
     :properties (libro--merge-properties (plist-get libro-author-template :properties)))))

;;;###autoload
(cl-defun libro-book-create (&optional title)
  "Create a book note using `libro-book-template'.

Optional TITLE of book"
  (interactive)
  (let* ((authors (libro-authors-select))
         (year (libro--repeat-while
                #'read-number
                (lambda (v) (< v 0))
                "Year published: "))
         (title (or title (libro--repeat-while
                           (lambda (p) (libro--read-string p))
                           #'string-empty-p
                           "Title: ")))
         (status (intern
                  (completing-read
                   "Status: "
                   libro-reading-states
                   nil
                   'require-match)))
         (note (vulpea-create
                title
                (plist-get libro-book-template :file-name)
                :tags (seq-union (plist-get libro-book-template :tags)
                                 '("library" "book"))
                :head (plist-get libro-book-template :head)
                :body (plist-get libro-book-template :body)
                :context (plist-get libro-book-template :context)
                :meta (-filter
                       #'cdr
                       (append
                        `(("authors" . ,authors)
                          ("year" . ,year)
                          ("status" . ,status))))))
         (x (when (eq status 'unread)
              (if (y-or-n-p "Create reading todo? ")
                  (vulpea-create (format "TODO Read %s" title)
                                 nil
                                 :body (format "[[id:%s][%s]]" (vulpea-note-id note) title)
                                 :tags '("library")
                                 :parent (car (vulpea-db-query-by-file-path libro-bookshelf)))))))
    note))

;;;###autoload
(cl-defun libro-find-create-book ()
  "Find a book or create one.

The optional STATUS limits the search of the books to those of a certain reading status."
  (interactive)
  (let* ((note (vulpea-find :candidates-fn (lambda (filter-fn)
                                             (seq-filter (or filter-fn #'identity)
                                                         (vulpea-db-query-by-tags-every '("library" "book"))))
                            :create-fn (lambda (title &optional _props)
                                         (libro-book-create title)))))
    note))

(cl-defun libro-select-book (prompt &optional status)
  "Select a book.

PROMPT is a custom prompt.

STATUS is a list of optional status to limit the search."
  (interactive)
  (let ((status (or status (list "unread" "reading" "read"))))
    (vulpea-select prompt :filter-fn (lambda (note)
                                       (and (member "library" (vulpea-note-tags note))
                                            (member "book" (vulpea-note-tags note))
                                            (member (vulpea-meta-get note "status") status))))))

(defun libro--note-of-type-p (note type)
  "Return non-nil if NOTE is a file level note tagged `library' and TYPE."
  (when-let* ((tags (vulpea-note-tags note))
              (level (vulpea-note-level note)))
    (and (equal level 0)
         (seq-contains-p tags "library")
         (seq-contains-p tags type))))

;;;###autoload
(defun libro-author-note-p (note)
  "Return non-nil if NOTE represents libro book."
  (libro--note-of-type-p note "author"))

;;;###autoload
(defun libro-book-note-p (note)
  "Return non-nil if NOTE represents libro book."
  (libro--note-of-type-p note "book"))

;;;###autoload
(defun libro-book-note-get-dwim (&optional note-or-id status)
  "Get a `vulpea-note' representing `libro-book' in a dwim style.

STATUS is a list of status that can be passed to restrict the choice.

If NOTE-OR-ID is an ID, then get a note by that ID. Throws error
if extracted note does not represent a `libro-book'.

If NOTE-OR-ID is a note, then return it. Throws error
if extracted note does not represent a `libro-book'.

If NOTE-OR-ID is nil, try to extract a note from current buffer
or ask for user to select a note. Extractions happens from
`libro-book' buffer. In the latter
case, linked `libro-book' is extracted."
  (cond
   ((null note-or-id)
    (if (eq major-mode 'org-mode)
        (let* ((id (save-excursion
                     (goto-char (point-min))
                     (org-id-get)))
               (note (vulpea-db-get-by-id id))
               (status (or status (list "unread" "reading" "read"))))
          (cond
           ((and (libro-book-note-p note) (member (vulpea-meta-get note "status") status)) note)
           (t (libro-select-book "Book" status))))
      (libro-select-book "Book" status)))
   ((stringp note-or-id)
    (let ((note (vulpea-db-get-by-id note-or-id))
          (status (or status (list "unread" "reading" "read"))))
      (if (and note (libro-book-note-p note) (member (vulpea-meta-get note "status") status))
          note)))
   ((vulpea-note-p note-or-id)
    (let ((status (or status (list "unread" "reading" "read"))))
      (if (and (libro-book-note-p note-or-id) (member (vulpea-meta-get note "status") status))
          note-or-id)))
   (t
    (user-error "Unsupported type of %s" note-or-id))))

;;;###autoload
(cl-defun libro-mark-as-reading (&optional note)
  "Mark a book as currently being read.
NOTE is the book note to mark."

  (interactive)
  (let* ((note (or note (libro-book-note-get-dwim nil (list "unread" "read")))))
    (vulpea-utils-with-note-sync note
      (vulpea-buffer-meta-set "status" "reading"))))

;;;###autoload
(cl-defun libro-mark-as-read (&optional note)
  "Mark a book as read.
NOTE is the book note to mark."

  (interactive)
  (let* ((note (or note (libro-book-note-get-dwim nil (list "reading")))))
    (vulpea-utils-with-note-sync note
      (vulpea-buffer-meta-set "status" "read")
      (vulpea-buffer-meta-set "finished_on" (append (vulpea-meta-get-list note "finished_on") (list (format "[%s]" (format-time-string "%Y-%m-%d %a")))) t))))
(provide 'libro)

;;; libro.el ends here
