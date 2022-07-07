;;; nyx-denote.el --- Module for denote              -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lycheese@nanoforge>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(straight-use-package '(denote :type git
			       :host github
			       :repo "protesilaos/denote"))

;; TODO: Handle loading `denote--valid-date' more elegantly
(require 'denote)

(defun my-denote-journal ()
  "Create an entry tagged 'journal' with the date as its title."
  (interactive)
  (let ((denote-file-type 'text)
	(denote-directory "~/Documents/Journal"))
    (denote
     (format-time-string "%A %e %B %Y") ; format like Tuesday 14 June 2022
     "journal"))) ; multiple keywords are a list of strings: '("one" "two")

(defun my-denote-journal-with-date (date)
  "Ask for DATE to write a journal entry.

Journal entries are stored in ~/Documents/journal/ and use plain
text for their `denote-file-type'.

Read the doc string of `denote-date' on what a valid DATE input is.

The title of the note is something like Tuesday 17 June 2020,
though you can modify the `format-time-string' specifiers as
described in its doc string."
  (interactive (list (denote--date-prompt)))
  (when-let ((denote-file-type 'text)
             (denote-directory "~/Documents/Journal/")
             (d (denote--valid-date date))
             (id (format-time-string denote--id-format d))
	     ((denote--barf-duplicate-id id)))
    (denote--prepare-note
     (format-time-string "%A %e %B %Y" d)
     "journal" nil d id)))

(defun my-denote-journal-for-today ()
  "Write a journal entry for today."
  (interactive)
  (my-denote-journal-with-date
   (format-time-string "%Y-%m-%dT00:00:00")))

(provide 'nyx-denote)
;;; nyx-denote.el ends here
