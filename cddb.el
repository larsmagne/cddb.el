;;; cddb.el --- cddb interface
;; Copyright (C) 1998, 2002 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: music

;; This file is not part of GNU Emacs.

;; cddb.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; cddb.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'cl)
(require 'expect)
(require 'message)
(require 'captitle)

(defvar cddb-servers '("freedb.org"
		       "us.cddb.com")
  "Server to connect to.")

(defvar cddb-server nil)

(defvar cddb-server-port "888"
  "Port to connect to.")

(defvar cddb-directory "/usr/local/cddb/"
  "*Where the cddb files are.")

(defvar cddb-remote-p t
  "*Whether a remote CDDB server should be used or not.")

(defvar cddb-mode-hook nil
  "Hook run in CDDB buffers.")

(defvar cddb-submit-hook nil
  "Hook run after submitting entries to the CDDB.")

(defvar cddb-postpone-hook nil
  "Hook run after not submitting entries to the CDDB.")

(defvar cddb-categories
  '("rock" "blues" "classical" "country" "data" "folk" "jazz" "misc"
    "newage" "reggae" "soundtrack"))

(defvar cddb-cdrom-device "/dev/scd0"
  "The device name to be given to the cd-discid program.")

(defun cddb-query (entry)
  "Query all cddb servers for ENTRY.
If found, a cons cell where the car is the category and the cdr is a
buffer with the cddb entry will be returned."
  (let* ((discid (cddb-get 'id entry))
	 (nsecs (cddb-get 'length entry))
	 (frames (cddb-get 'frames entry))
	 (ntracks (length frames))
	 result beg stop category file cat)
    (cond
     (t
      (cddb-query-remote entry))
     ((file-exists-p
       (setq file (concat "/music/data/my-cddb/" discid)))
      (save-excursion
	(set-buffer (generate-new-buffer " *cddb*"))
	(insert-file-contents file)
	(cons "rock" (current-buffer))))
     ((file-exists-p
       (setq file (concat cddb-directory "data/new-cdda/" discid)))
      (save-excursion
	(set-buffer (generate-new-buffer " *cddb*"))
	(insert-file-contents file)
	(cons "rock" (current-buffer))))
     ((let ((cats cddb-categories))
	(while cats
	  (setq cat (pop cats))
	  (if (file-exists-p (setq file (concat cddb-directory
						cat "/" discid)))
	      (setq cats nil)
	    (setq file nil)))
	file)
      (when file
	(save-excursion
	  (set-buffer (generate-new-buffer " *cddb*"))
	  (insert-file-contents file)
	  (cons cat (current-buffer)))))
     (cddb-remote-p
      (cddb-query-remote)))))

(defun cddb-query-remote (entry)
  (catch 'found
    (dolist (cddb-server cddb-servers)
      (message "Querying server %s..." cddb-server)
      (let ((result
	     (condition-case ()
		 (cddb-query-remote-1 entry)
	       (quit nil))))
	(when result
	  (message "Querying server %s...done" cddb-server)
	  (throw 'found result))))))
  
(defun cddb-query-remote-1 (entry)
  (let* ((discid (cddb-get 'id entry))
	 (nsecs (cddb-get 'length entry))
	 (frames (cddb-get 'frames entry))
	 (ntracks (length frames))
	 result beg stop category file cat)
    ;; Query the remote CDDB server.
    (with-expect (list "telnet" cddb-server cddb-server-port)
      ;; Get welcome message and log in.
      (expect "^2"
	(expect-send (format
		      (if (string= "us.cddb.com" cddb-server)
			  "cddb hello %s %s xmcd 2.6"
			"cddb hello %s %s cddb 1.0")
		      (user-login-name) (system-name))))
      (expect "^200"
	(expect-send "proto 5"))
      ;; Get accepted and query for disc.
      (expect "^2"
	(expect-send (format "cddb query %s %s %s %s"
			     discid ntracks 
			     (mapconcat (lambda (e)
					  (format "%s" e))
					frames " ")
			     nsecs)))
      (expect-cond
       ;; We found disc.
       ("^200 \\([a-z0-9]+\\)"
	(setq category (match-string 1)))
       ;; Found several exact matches.
       ("^210 "
	(expect "^\\([a-z0-9]+\\) \\([a-f0-9]+\\) ")
	;; Just use the first match.
	(setq category (match-string 1)
	      discid (match-string 2))
	;; Flush to the end of the matches.
	(expect "^\\."))
       ;; We didn't find disc.
       ("^[2345]"
	(setq stop t)))
      (unless stop
	;; Get the disc entry.
	(expect-send
	 (format "cddb read %s %s" category discid))
	(expect-cond
	 ("^210"
	  (forward-line 1)
	  (setq beg (point)))
	 ("^[345]"
	  (setq stop t))))
      (unless stop
	;; Wait for end of entry...
	(expect "^\\."
	  (let ((buf (current-buffer))
		(end (1- (point))))
	    (save-excursion
	      (set-buffer (generate-new-buffer " *cddb*"))
	      (insert-buffer-substring buf beg end)
	      (goto-char (point-min))
	      ;; Clean up the data.
	      (while (search-forward "\r" nil t)
		(replace-match "" t t))
	      (setq result (current-buffer))))))
      (expect-send "quit"))
    ;; Return the info.
    (when result
      (cons category result))))

(defun cddb-get (type entry)
  "Return the value of TYPE in CDDB ENTRY."
  (cdr (assq type entry)))

(defun cddb-parse (file &optional type)
  "Parse CDDB file FILE, which can be a file or a buffer.
Return an alist that contains all the information in the file.
Keys are `frames', `length', `id', `artist', `title', `tracks',
`extension', `revision', and `track-extensions'."
  (save-excursion
    (if (bufferp file)
  	(set-buffer file)
      (when (file-exists-p file)
	(set-buffer (get-buffer-create " *cddb*"))
	(erase-buffer)
	(insert-file-contents file)))
    (let (frames length id artist tracks extension track-extensions
	  client items title revision year)
      (goto-char (point-min))
      (when (search-forward "Track frame offsets:" nil t)
	(forward-line 1)
	(while (looking-at "#[ \t]+\\([0-9]+\\)[ \t]*$")
	  (push (string-to-number (match-string 1)) frames)
	  (forward-line 1)))

      (when (re-search-forward "Disc length: \\([0-9]+\\)" nil t)
	(setq length (string-to-number (match-string 1))))

      (when (re-search-forward "Revision: \\([0-9]+\\)" nil t)
	(setq revision (string-to-number (match-string 1))))

      (when (re-search-forward "Submitted via: \\(.+\\)$" nil t)
	(setq client (match-string 1)))

      (when (re-search-forward "DISCID=\\(.+\\)$" nil t)
	(setq id (match-string 1)))

      (when (setq title (car (cddb-pget "DTITLE")))
	(if (string-match " */ *" title)
	    (setq artist (substring title 0 (match-beginning 0))
		  title (substring title (match-end 0)))
	  (setq artist title)))

      (setq tracks (cddb-pget "TTITLE"))
      (setq extension (car (cddb-pget "EXTD")))
      (setq track-extensions (cddb-pget "EXTT"))

      (setq year (car (cddb-pget "DYEAR")))

      (setq items
	    (cddb-build-alist 
	     'id id
	     'artist artist
	     'title title
	     'tracks tracks
	     'extension extension
	     'extensions track-extensions
	     'frames (nreverse frames)
	     'length length
	     'client client
	     'revision revision
	     'year year))
      (if type
	  (cdr (assq type items))
	items))))

(defun cddb-build-alist (&rest pairs)
  (let (alist)
    (while pairs
      (push (cons (pop pairs) (pop pairs)) alist))
    alist))

(defun cddb-pget (type)
  "Return a list of elements of TYPE in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (format "^%s\\([0-9]*\\)=\\(.+\\)$" (regexp-quote type)))
	  (prev-string "")
	  prev items)
      (while (re-search-forward regexp nil t)
	(when (and prev
		   (not (equal (match-string 1) prev)))
	  (push prev-string items)
	  (setq prev-string ""))
	(setq prev (match-string 1)
	      prev-string (concat prev-string (match-string 2))))
      (push prev-string items)
      (nreverse items))))

(defun cddb-insert (alist)
  "Insert a CDDB entry based on ALIST."
  (let* ((i 0)
	 (id (cdr (assq 'id alist)))
	 (frames (cdr (assq 'frames alist)))
	 (tracks (cdr (assq 'tracks alist)))
	 (track-extensions (cdr (assq 'track-extensions alist)))
	 (ntracks (length frames))
	 names)
    (insert "# xmcd CD database file\n#\n# Track frame offsets:\n")
    (while frames
      (insert (format "#\t%s\n" (pop frames))))
    (insert (format "#\n# Disc length: %d\n"
		    (cdr (assq 'length alist))))
    (insert (format "# Revision: %s\n"
		    (1+ (or (cdr (assq 'revision alist)) -1)))
	    "# Submitted via: cddb.el 1.0 (Emacs "
	    emacs-version ")\n#\n")
    (insert "DISCID=" id "\n")
    (insert (format "DTITLE=%s / %s\n" (or (cdr (assq 'artist alist)) "")
		    (or (cdr (assq 'title alist)) "")))
    (insert (format "DYEAR=%s\n" (or (cdr (assq 'year alist)) "")))
    (while (< i ntracks)
      (insert (format "TTITLE%d=%s\n" i (or (nth i tracks) "")))
      (incf i))
    (insert (format "EXTD=%s\n" (or (cdr (assq 'extension alist)) "")))
    (setq i 0)
    (while (< i ntracks)
      (insert (format "EXTT%d=%s\n" i (or (nth i track-extensions) "")))
      (incf i))
    (insert "PLAYORDER=\n")))

(defun cddb-insert-new-data (alist &optional genre)
  "Insert editable data based on ALIST."
  (let* ((i 0)
	 (tracks (cdr (assq 'tracks alist)))
	 (ntracks (length tracks))
	 names)
    (insert (format "Genre: %s\n" (or genre "folk"))) 
    (insert (format "Title: %s / %s\n"
		    (or (cdr (assq 'artist alist)) "")
		    (or (cdr (assq 'title alist)) "")))
    (insert (format "Year: %s\n" (or (cdr (assq 'year alist)) "")))
    (insert "Tracks:\n")
    (while (< i ntracks)
      (insert (format "%s\n" (or (nth i tracks) "")))
      (incf i))))

(defun cddb-write-file (file alist)
  "Write ALIST to FILE."
  (save-excursion
    (set-buffer (get-buffer-create " *cddb work*"))
    (erase-buffer)
    (cddb-insert alist)
    (write-region (point-min) (point-max) file nil 'silent)
    (kill-buffer (current-buffer))))

(defvar cddb-data nil)

(defun cddb-edit (alist &optional genre)
  "Edit a CDDB entry based on ALIST."
  (pop-to-buffer (generate-new-buffer (format "*cddb %s*"
					      (cdr (assq 'id alist)))))
  (cddb-insert-new-data alist genre)
  (set (make-local-variable 'cddb-data) alist)
  (cddb-mode)
  (goto-char (point-min))
  (forward-line 2)
  (set-buffer-modified-p nil))

(defun cddb-submit (&optional arg)
  "Send the entry to CDDB."
  (interactive "P")
  (let ((alist cddb-data)
	(modified (buffer-modified-p))
	(genre (message-fetch-field "Genre"))
	(year (message-fetch-field "Year"))
	(ctitle (message-fetch-field "Title"))
	tracks title artist)
    (when (string-match "^\\([^/]+\\) / \\(.*\\)$" ctitle)
      (setq artist (match-string 1 ctitle))
      (setq title (match-string 2 ctitle)))
    (goto-char (point-min))
    (when (re-search-forward "^Tracks:.*\n" nil t)
      (while (not (eobp))
	(push (buffer-substring (point) (progn (end-of-line) (point)))
	      tracks)
	(forward-line 1)))
    (setq tracks (nreverse tracks))
    (setq alist (cddb-merge alist
			    (list (cons 'artist artist)
				  (cons 'title title)
				  (cons 'year year)
				  (cons 'tracks tracks))))
    (erase-buffer)
    (cddb-insert alist)

    (save-excursion
      (goto-char (point-min))
      (search-forward "DTITLE")
      (beginning-of-line)
      (while (not (eobp))
	(if (not (looking-at "\\([^=]+=\\)"))
	    (error "Bogus file")
	  (end-of-line)
	  (if (not (> (current-column) 78))
	      (forward-line 1)
	    (beginning-of-line)
	    (forward-char 78)
	    (insert "\n" (match-string 1))
	    (beginning-of-line))))
      (when (and modified
		 (not arg))
	(cddb-send genre))
      (set-buffer-modified-p nil))
    (cddb-write-file (concat cddb-directory "data/new-cdda/"
			     (cddb-get 'id alist))
		     alist))
  (run-hooks 'cddb-submit-hook)
  (kill-buffer (current-buffer))
  (other-window 1)
  (delete-other-windows))

(defun cddb-postpone ()
  "Don't send the entry to CDDB."
  (interactive)
  (run-hooks 'cddb-postpone-hook))

(defun cddb-send (genre)
  "Send the current buffer to cddb."
  (save-excursion
    (let ((mail-header-separator "")
	  (message-signature nil)
	  (buf (current-buffer))
	  (id (cddb-parse (current-buffer) 'id)))
      (message-mail
       "freedb-submit@freedb.org"
       (concat "cddb " genre " " id))
      (insert-buffer buf)
      (message-send))))

(defvar cddb-mode-map nil)
(unless cddb-mode-map
  (setq cddb-mode-map (copy-keymap text-mode-map))
  (define-key cddb-mode-map "\C-c\C-c" 'cddb-submit)
  (define-key cddb-mode-map "\C-c\C-p" 'cddb-postpone)
  (define-key cddb-mode-map "\C-c\C-r" 'cddb-capitalize))

(defun cddb-mode (&optional arg)
  "Mode for editing CDDB files.

\\{cddb-mode-map}"
  (interactive)
  (let ((table (copy-syntax-table text-mode-syntax-table)))
    (set-syntax-table table)
    (modify-syntax-entry ?' "w"))
  (setq major-mode 'cddb-mode)
  (setq mode-name "CDDB")
  (use-local-map cddb-mode-map)
  (run-hooks 'cddb-mode-hook))

(defun cddb-capitalize ()
  "Capitalize titles."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line 3)
    (while (not (eobp))
      (let* ((line (buffer-substring (point) (point-at-eol)))
	     (new-line
	      (if (string-match " / " line)
		  (mapconcat 'identity
			     (mapcar 'capitalize-title
				     (split-string line " / "))
			     " / ")
		(capitalize-title line))))
	(unless (string= line new-line)
	  (delete-region (point) (point-at-eol))
	  (insert new-line)))
      (forward-line 1))))

(defun cddb-track-length (track entry)
  "Return the length of TRACK (in bytes) in ENTRY."
  (let ((frames (cddb-get 'frames entry))
	(length (cddb-get 'length entry)))
    (setq frames (nconc frames (list (* length 75))))
    (* 1.0 (- (nth track frames) (nth (1- track) frames))
       2048)))

(defun cddb-reverse-tracks ()
  (interactive)
  (replace-regexp "^\\(.*\\) / \\(.*\\)$" "\\2 / \\1"))

(defun cddb-merge (ofr frames)
  (dolist (elem frames)
    (setq ofr (delq (assq (car elem) ofr) ofr)))
  (nconc frames ofr))

(defun cddb-chop-list (list length)
  (setcdr (nthcdr (1- length) list) nil)
  list)

(defun cddb-get-toc-with-discid ()
  "Get the Table Of Contents by using the cd-discid extenal command."
  (let ((output (shell-command-to-string
		 (format "cd-discid %s" cddb-cdrom-device)))
	entry)
    (if (not output)
	(error "No output from cd-discid")
      (setq output (split-string output))
      (cddb-build-alist
       'id (nth 0 output)
       'length (string-to-number (car (last output)))
       'frames (cddb-chop-list (cddr output)
			       (- (length output) 3))))))

(defun cddb-read-cd ()
  "Examing a cd, query the server and enter a buffer to edit the entry."
  (interactive)
  (let* ((frames (cddb-get-toc-with-discid))
	 (entry (cddb-query frames)))
    (if (not entry)
	(cddb-edit (cdr entry) (car entry))
      (save-excursion
	(set-buffer (cdr entry))
	(cddb-edit (cddb-merge (cddb-parse (current-buffer)) frames)
		   (car entry))))))

(defun cddb-compute-cddb-id (entry)
  (let ((frames (cdr (assoc 'frames entry)))
	(n 0))
    (dolist (frame frames)
      (incf n (cddb-id-sum (truncate (/ frame 75)))))
    
    (format "%02x%04x%02x"
	    (% n 255)
	    (- (cdr (assoc 'length entry))
	       (truncate (/ (car frames) 75)))
	    (length frames))))

(defun cddb-id-sum (n)
  (let ((ret 0))
    (while (> n 0)
      (incf ret (% n 10))
      (setq n (truncate (/ n 10))))
    ret))

(provide 'cddb)

;;; cddb.el ends here
