;;; captitle.el --- Capitalize Titles
;; Copyright (C) 2004 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <lmi@gnus.org>
;; Keywords: extensions, processes

;; This file is not part of GNU Emacs.

;; captitle.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; captitle.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(defvar capitalize-title-uncapitalized-words
  '("a" "an" "the" "and" "or" "nor" "for" "but" "so" "yet"
    "to" "of" "by" "at" "for" "but" "in" "with" "has" "de" "von"
    "vs" "vs." "is" "on")
  "Words not to be capitalized in a title.")

(defun capitalize-title-upper-case-p (word)
  (let ((case-fold-search nil))
    (not (string-match "[a-z]" word))))

(defun capitalize-title (title)
  "Capitalize TITLE."
  (with-syntax-table text-mode-syntax-table
    (let* ((words (delete "" (split-string title)))
	   (cwords nil)
	   (length (length words))
	   (i 0))
      (dolist (word words)
	(push (cond
	       ((and (> (length word) 1)
		     (capitalize-title-upper-case-p word))
		word)
	       ((and (member (downcase word)
			     capitalize-title-uncapitalized-words)
		     (not (zerop i))
		     (< i length))
		(downcase word))
	       (t
		(capitalize word)))
	      cwords)
	(cl-incf i))
      (mapconcat 'identity (nreverse cwords) " "))))

(provide 'captitle)

;;; captitle.el ends here
