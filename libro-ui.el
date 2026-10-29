;;; libro-ui.el --- UI for Libro -*- lexical-binding: t; -*-
;;
;; Author: drpaepper <drpaepper@drpaepper.xyz>
;; Maintainer: drpaepper <drpaepper@drpaepper.xyz>
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
;; UI for libro.
;;
;;; Code:

(require 'vulpea)
(require 'libro)

(defvar libro-ui-columns
  `[("ID" 5 t)
    ("Title" 44 t . (:pad-right 2))
    ("Author" 26 t . (:pad-right 2))
    ("Status" 20 t)])

(cl-defstruct libro-ui-book id title author status)

(defun libro-ui-get-books ()
  (let ((books (vulpea-db-query-by-tags-every (list "library" "book"))))
    (--map
     (make-libro-ui-book
      :id (vulpea-note-id it)
      :title (vulpea-note-title it)
      :author (vulpea-note-title (vulpea-db-get-by-id (vulpea-note-meta-get it "authors" 'link)))
      :status (vulpea-note-meta-get it "status"))
     books)))

(defun libro-ui-render-cell (key book)
  "Render KEY cell for BOOK."
  (pcase (s-downcase key)
    (`"id" (propertize (libro-ui-book-id book) 'face 'font-lock-property-use-face))
    (`"title" (propertize (libro-ui-book-title book) 'face 'link))
    (`"author" (propertize (libro-ui-book-author book) 'face 'font-lock-regexp-face))
    (`"status" (propertize (libro-ui-book-status book) 'face 'font-lock-number-face))))

(defun libro-ui-render-header (books)
  "Render header line for BOOKS."
  (concat
   "Total "
   (propertize (format "%d" (seq-length books)) 'face 'bold)
   " books."))

(defvar-local libro-ui--columns-idx nil)

(defun libro-ui-quit ()
  "Quit from bookshelf UI."
  (interactive)
  (quit-window))

(defun libro-ui-book-id-at (&optional pos)
  "Return the id of the Tabulated List entry at POS.

POS, if omitted or nil, defaults to point.  Signals when there is no
entry there, which is what an empty inventory offers."
  (or (tabulated-list-get-id pos)
      (user-error "There is no book here")))

(defun libro-ui-visit (&optional other-window)
  "Visit book entry at point.

If OTHER-WINDOW, visit the NOTE in another window."
  (interactive)
  (vulpea-visit (libro-ui-book-id-at)
                (or current-prefix-arg other-window)))

(defvar libro-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "q" #'libro-ui-quit)
      (define-key map "u" #'libro-ui-update)
      (define-key map (kbd "<RET>") #'libro-ui-visit)))
  "Keymap for `libro-ui-mode'.")

(define-derived-mode libro-ui-mode tabulated-list-mode "libro-bookshelf"
  "Major mode for listing book entries."
  (setq tabulated-list-printer #'tabulated-list-print-entry)
  (setq tabulated-list-padding 2))

(defun libro-ui-bookshelf ()
  "Open bookshelf."
  (interactive)
  (let ((buffer (get-buffer-create "*libro-bookshelf*")))
    (switch-to-buffer buffer)
    (unless (eq major-mode 'libro-ui-mode)
      (libro-ui-mode))
    (libro-ui-update)))

(defun libro-ui-update ()
  "Update bookshelf."
  (interactive)
  (let* ((books (libro-ui-get-books)))
    (setq tabulated-list-format libro-ui-columns
          tabulated-list-entries
          (->> books
               (--sort (if (string= (libro-ui-book-author it)
                                    (libro-ui-book-author other))
                           (string< (libro-ui-book-title it)
                                    (libro-ui-book-title other))
                         (string< (libro-ui-book-author it)
                                  (libro-ui-book-author other))))
               (--map
                (list
                 (libro-ui-book-id it)
                 (apply
                  #'vector
                  (-map (lambda (col)
                          (libro-ui-render-cell (car col) it))
                        libro-ui-columns)))))
          tabulated-list-sort-key nil
          tabulated-list-use-header-line nil
          header-line-format (libro-ui-render-header books)
          libro-ui--columns-idx (seq-map-indexed (lambda (c idx) (cons (car c) idx))
                                                 libro-ui-columns)))
  (tabulated-list-init-header)
  (tabulated-list-print 'remember-pos))

(defun libro-ui-mark-as-read ()
  "Mark book under cursor as read."
  (interactive)
  (let* ((book (vulpea-db-get-by-id (libro-ui-book-id-at)))
         (status (vulpea-meta-get book "status")))
    (if (string= status "reading")
        (libro-mark-as-read book))
    (libro-ui-update)))

(defun libro-ui-mark-as-reading ()
  "Mark book under cursor as reading."
  (interactive)
  (let* ((book (vulpea-db-get-by-id (libro-ui-book-id-at)))
         (status (vulpea-meta-get book "status")))
    (if (member status '("unread" "read"))
        (libro-mark-as-reading book))
    (libro-ui-update)))

(provide 'libro-ui)
;;; libro-ui.el ends here
