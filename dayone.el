;;; dayone.el --- Utility script for Day One

;; Copyright (C) 2013 by mori-dev

;; Author: mori-dev <mori.dev.asdf@gmail.com>
;; Keywords: Day One, tools, convenience
;; URL: https://github.com/mori-dev/emacs-dayone
;; Version: 0.1
;; Package-Requires: ((uuid "0.0.3") (mustache "1.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; dayone.el is a simple emacs extension for placing the new post from
;; the emacs to Day One(http://dayoneapp.com).  It can post the
;; region when the concerned region is selected and M-x dayone-add-note.
;; When the region is not selected and M-x dayone-add-note is executed,
;; the all contents in the buffer are posted.
;; As for dayone-add-note(), it may be convenient if you assign it to
;; an appropriate key or add alias.
;; For the Day One, the data is managed by either the iCloud or the
;; Dropbox. This emacs extension is supported for storing data of
;; Dropbox only.

;;; Setting:

;; (require 'dayone)
;; (setq dayone-dir (concat (getenv "HOME") "/Dropbox/APP/Day One/Journal.dayone/entries/"))
;; (setq dayone-timezone "America/Sao_Paulo")

;;; Code:

(require 'uuid)
(require 'mustache)
(require 'ht)

(defvar dayone-dir ""
  "It stores the path indicating the directory allocating the data of the
 Day One. It should be set up by your setting file.")

(defvar dayone-timezone "Asia/Tokyo"
  "It stores the time zone value designated by the XML of the Day One note.")

(defvar software-agent "Day One (iOS)/1.11.4"
  "It stores the Software Agent value designated by the XML of the Day One
 note. It seems that the default value works well enough.")

(defvar dayone-os "Ubuntu"
  "It stores the OS Agent value designated by the XML of the Day One note.
It seems that the default value works well enough.")

(defvar dayone-file-contents "")

(defun dayone-date ()
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time)))

(defun dayone-uuid ()
  (uuid-to-stringy (uuid-create)))

(defun dayone-filename (uuid)
  (concat dayone-dir uuid ".doentry"))

(defun dayone-note ()
  (if (region-active-p)
    (buffer-substring-no-properties (region-beginning) (region-end))
  (buffer-string)))

(defun dayone-set-xml (uuid)
  (let ((mustache-partial-paths (list "./"))
        (context (ht ("date" (dayone-date))
                     ("uuid" uuid)
                     ("note" (dayone-note))
                     ("timezone" dayone-timezone)
                     ("os" dayone-os))))
    (setq dayone-file-contents (mustache-render "{{> layout.xml}}" context))))

(defun dayone-write-file (uuid)
  (with-temp-buffer
    (insert dayone-file-contents)
    (write-file (dayone-filename uuid))))

;;;###autoload
(defun dayone-add-note ()
  "It creates the new Day One note."
  (interactive)
  (let ((uuid (dayone-uuid)))
    (dayone-set-xml uuid)
    (dayone-write-file uuid)))

(provide 'dayone)

;;; dayone.el ends here
