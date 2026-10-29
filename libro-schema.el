;;; libro-schema.el --- Schemata for libro notes-*- lexical-binding: t; -*-
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
;; Vulpea schemas for the libro note types.
;;
;;; Code:

(require 'vulpea)
(require 'libro)

(defvar libro-schema-book-fields
  `((:key "authors" :type note :required t :target0tags ("book" "author"))
    (:key "year" :type number :required t)
    (:key "status" :type symbol :required t :one-of ,(lambda (_note) libro-reading-states))
    (:key "finished_on" :type date :required nil :multiple t))
  "Fields for `libro-book' schema.")

(defvar libro-schema-author-fields nil
  "Fields for `libro-author' schema.

As an author note has no metadata of its own, we set this to nil to
allow for both custom metadata and coverage by collection health.")

;;;###autoload
(defun vino-schema-setup ()
  "Register schemata for libro types."
  (interactive)
  (vulpea-schema-define 'libro-book
                        :predicate #'libro-book-note-p
                        :fields libro-schema-book-fields)
  (vulpea-schema-define 'libro-author
                        :predicate #'libro-author-note-p
                        :fields libro-schema-author-fields))

(provide 'libro-schema)
;;; libro-schema.el ends here
