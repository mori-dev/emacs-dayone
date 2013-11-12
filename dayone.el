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

;;; Code:

(require 'uuid)
(require 'mustache)
(require 'ht)

(defvar dayone-dir (concat (getenv "HOME") "/Dropbox/アプリ/Day One/Journal.dayone/entries/"))

(defvar dayone-timezone "Asia/Tokyo")
(defvar software-agent "Day One (iOS)/1.11.4")
(defvar dayone-os "Ubuntu")

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
  (interactive)
  (let ((uuid (dayone-uuid)))
    (dayone-set-xml uuid)
    (dayone-write-file uuid)))

(provide 'dayone)

;;; dayone.el ends here
