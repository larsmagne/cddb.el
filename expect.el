;;; expect.el --- support for external process communication
;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <lmi@gnus.org>
;; Keywords: extensions, processes

;; This file is soon to be part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'timer)

(defvar expect-message nil
  "*If non-nil, report how much data has arrived in the process buffer.
This variable is buffer-local to all Expect buffers, and should be set
inside @code{with-expect} forms.")

(defvar expect-start nil
  "If a number, start the Expect searches from that point.
If not, start searches from `(point-min)'.
This variable is typically `let' to t before calling `with-expect'
when waiting for output from a process that is already started and may
have output data.")

;;; Internal variables.

(defvar expect-processes nil)
(defvar expect-asynchronous nil)
(defvar expect-is-running nil)
(defvar expect-process nil)		; Dynamic variable
(defvar expect-current-info nil)	; Dynamic variable
(defvar expect-cancel nil)		; Dynamic variable
(defvar expect-main-cancel 'no)		; Dynamic variable

;;; Utility macros.

(defun expect-make-info (process message point)
  (list process message point nil nil))

(defmacro expect-info-process (info)
  `(nth 0 ,info))

(defmacro expect-info-message (info)
  `(nth 1 ,info))

(defmacro expect-info-point (info)
  `(nth 2 ,info))
(defmacro expect-info-set-point (info point)
  `(setcar (nthcdr 2 ,info) ,point))

(defmacro expect-info-sentinels (info)
  `(nth 3 ,info))
(defmacro expect-info-set-sentinels (info sentinels)
  `(setcar (nthcdr 3 ,info) ,sentinels))

(defmacro expect-info-timer (info)
  `(nth 4 ,info))
(defmacro expect-info-set-timer (info timer)
  `(setcar (nthcdr 4 ,info) ,timer))

(defmacro expect-info-queries (info)
  `(nthcdr 5 ,info))
(defmacro expect-info-set-queries (info queries)
  `(setcdr (nthcdr 4 ,info) ,queries))

(defmacro expect-find-info (process)
  `(assoc ,process expect-processes))

;;; Interface macros.

;;;###autoload
(defmacro with-expect (program &rest forms)
  "Set things up for communication with PROGRAM.
FORMS will be evaluated in the normal manner.  To talk to the process,
use `expect' and `expect-send'.  See the manual for full documentation.
This macro returns nil.

If PROGRAM is a string, start that program.  If PROGRAM is a list, use
the first element of that list as the program and the remainder as the
parameters.  If PROGRAM is a process, talk to that process.

PROGRAM will be started up in a new, fresh temporary buffer.  The
buffer will be killed upon completion.  If PROGRAM is a process,
a new buffer won't be created, and the buffer won't be killed upon
completion."
  (let ((point (make-symbol "point"))
	(process-spec (make-symbol "process-spec")))
    `(save-excursion
       (let ((,point (point))
	     ,process-spec expect-process expect-current-info
	     expect-main-cancel)
	 (unless (setq ,process-spec (expect-start-process ,program))
	   (error "Can't start program"))
	 (setq expect-process (car ,process-spec))
	 (expect-setup ,point)
	 ,@forms
	 (expect-cleanup (cdr ,process-spec))
	 nil))))

(defmacro with-expect-asynchronous (program &rest forms)
  "Set things up for asynchronous communication with PROGRAM.
This macro behaves like `with-expect', only that `expect' calls
contained in FORMS will be evaluated asyncronously.

See the documentation of the `with-expect' macro for documentation."
  `(let ((expect-asynchronous t))
     (with-expect ,program ,@forms)))

(defmacro expect (regexp &rest forms)
  "Execute FORMS when REGEXP  has arrived in the buffer."
  `(expect-1 ,regexp #'(lambda () ,@forms)))

(defmacro expect-cond (&rest clauses)
  "Try each clause until one succeeds.
Each clause looks like (CONDITION BODY).  CONDITION should be
a regular expression to wait for, or a process status symbol.
If CONDITION is satisfied (i. e., the data has arrived or
the process has entered the specified status), BODY will be executed."
  (let (result)
    (while clauses
      (push (if (stringp (caar clauses)) (caar clauses)
	      (list 'quote (caar clauses)))
	    result)
      (push (car `(#'(lambda () ,@(cdar clauses)))) result)
      (pop clauses))
    `(expect-1 ,@(nreverse result))))

(defmacro expect-exit (&rest forms)
  "Execute FORMS when the process has exited."
  `(expect-exit-1 #'(lambda () ,@forms)))

;;; User utility functions.

(defun expect-send (string)
  "Send STRING to the current buffer's process.
A newline will be appended to the string before sending."
  (process-send-string expect-process (concat string "\n")))

(defun expect-send-string (string)
  "Send STRING to the current buffer's process."
  (process-send-string expect-process string))

(defun expect-cancel ()
  "Cancel the Expect currently in progress."
  (expect-info-set-queries expect-current-info nil)		       
  (setq expect-cancel t))

;;; Internal functions.

(defun expect-setup (&optional point)
  "Initialize Expect data, filter and sentinel."
  (setq expect-current-info
	(expect-make-info expect-process expect-message 
			  (or expect-start (point-min))))
  (push expect-current-info expect-processes)
  (set-process-filter expect-process 'expect-filter)
  (set-process-sentinel expect-process 'expect-sentinel)
  (set-buffer (process-buffer expect-process)))

(defun expect-shutdown (process)
  "Remove Expect infestation of PROCESS."
  (setq expect-processes (delq (expect-find-info process) expect-processes))
  (set-process-filter process nil)
  (set-process-sentinel process nil))

(defun expect-kill (process)
  "Kill PROCESS and its buffer."
  (let ((buffer (process-buffer process)))
    (when (buffer-name buffer)
      (kill-buffer buffer))
    (expect-shutdown process)
    (delete-process process)))

(defun expect-wait ()
  "Wait until the current outstanding command has been performed."
  (let ((info (expect-find-info expect-process)))
    (expect-setup-timer info)
    (expect-find-event expect-process)
    (while (and (car (expect-info-queries (expect-find-info expect-process)))
		(memq (process-status expect-process) '(open run)))
      (accept-process-output expect-process 1))
    (expect-cancel-timer info))
  ;; We return nil.
  nil)

(defun expect-1 (&rest clauses)
  ;; If a synchronous `cancel' is in effect, then we don't
  ;; want to do anything here.
  (unless (eq expect-main-cancel 'yes)
    (let (entry entries timeout-form timeout-length)
      (unless expect-process
	(error "No expect in this buffer"))
      ;; Add this clause to the list of things to be executed.
      (while clauses
	(if (and (consp (car clauses))
		 (eq (caar clauses) 'timeout))
	    (setq timeout-form (cadr clauses)
		  timeout-length (cadar clauses)
		  clauses (cddr clauses))
	  (push (list (pop clauses) (pop clauses))
		entries)))
      (when timeout-form
	(expect-info-set-timer
	 expect-current-info (list nil timeout-length timeout-form)))
      (nconc expect-current-info (list (nreverse entries)))
      ;; We see whether we have to wait for the command to complete
      ;; or not.
      (if expect-asynchronous
	  nil
	(expect-wait)))))

(defun expect-exit-1 (function)
  (unless expect-process
    (error "No expect in this buffer"))
  (let ((info (expect-find-info expect-process)))
    (expect-info-set-sentinels
     info
     (nconc (expect-info-sentinels info)
	    (list function))))
  ;; We return nil.
  nil)

(defun expect-filter (process string)
  "Controlling Expect function run as a process filter."
  (let ((old-buffer (current-buffer))
	(expect-process process))
    (unwind-protect
	(let (moving)
	  (set-buffer (process-buffer process))
	  (setq moving (= (point) (process-mark process)))
	  (save-excursion
	    ;; Insert the text, moving the process-marker.
	    (goto-char (process-mark process))
	    (insert string)
	    (set-marker (process-mark process) (point))
	    ;; Do Expect things.
	    (unless expect-is-running
	      (expect-find-event process)))
	  (when (memq (process-status process) '(open run))
	    (if moving (goto-char (process-mark process)))))
      (when (buffer-name old-buffer)
	(set-buffer old-buffer)))))

(defun expect-sentinel (process status)
  "Controlling Expect sentinel."
  ;; Perhaps we're waiting for one of the process events?
  (expect-find-event process)
  ;; We do `expect-exit' calls.
  (when (eq 'exit (process-status process))
    (save-excursion
      (let ((expect-process process))
	(when (and (process-buffer process)
		   (buffer-name (process-buffer process)))
	  (set-buffer (process-buffer process))
	  (let ((sentinels (expect-info-sentinels (expect-find-info process))))
	    (while sentinels
	      (save-excursion
		(funcall (pop sentinels))))
	    (expect-shutdown process)))))))
      
(defun expect-find-event (process)
  "Find (and execute) the next event."
  (let* ((info (expect-find-info process))
	 (point (expect-info-point info))
	 (queries (expect-info-queries info))
	 (clause (car queries))
	 (expect-is-running t)
	 cond)
    (expect-setup-timer info)
    (when (expect-info-message info)
      (message "Expect received %d characters" (point-max)))
    (when clause
      (if (eq (caar clause) t)
	  ;; We have handled all queries and want to die.
	  (expect-kill process)
	(when (> (point-max) point)
	  (goto-char point)
	  (while clause
	    (setq cond (caar clause))
	    (when (cond
		   ;; Regexp
		   ((stringp cond)
		    (re-search-forward (caar clause) nil t))
		   ;; Fall-through
		   ((eq t cond)
		    t)
		   ;; Process state
		   ((memq cond '(exit run stop signal open closed))
		    (eq cond (process-status process)))
		   (t
		    (error "Illegal condition: %s" cond)))
	      (expect-cancel-timer info)
	      (expect-info-set-point info (point))
	      (expect-info-set-queries info (cdr queries))
	      (save-excursion
		;; We tippy-toe around here a bit, since the
		;; form we evaluate might contain an `expect' call,
		;; and we want to insert it in the proper place.
		(let* ((expect-current-info info)
		       (old-queries (expect-info-queries info))
		       expect-cancel)
		  (expect-info-set-queries info nil)		       
		  (funcall (cadar clause))
		  (if expect-cancel
		      ;; See whether we're in a synchronous Expect...
		      (when (null expect-main-cancel)
			;; Yes, so we cancel all further `expects'.
			(setq expect-main-cancel 'yes))
		    (expect-info-set-queries
		     info (nconc (expect-info-queries info) old-queries)))))
	      (setq clause nil)
	      ;; More than one event may have arrived, so we try again.
	      (when (memq (process-status process) '(open run))
		(expect-find-event process)))
	    (setq clause (cdr clause))))))))

(defun expect-setup-timer (info)
  (let ((timer (expect-info-timer info)))
    (when timer
      (expect-cancel-timer info)
      (setcar timer (run-at-time (cadr timer) nil (caddr timer))))))

(defun expect-cancel-timer (info)
  (when (car (expect-info-timer info))
    (ignore-errors (cancel-timer (car (expect-info-timer info))))))

(defun expect-start-process (program)
  (cond
   ((stringp program)
    (set-buffer (generate-new-buffer " *expect*"))
    (cons (start-process "expect" (current-buffer) program) t))
   ((consp program)
    (set-buffer (generate-new-buffer " *expect*"))
    (cons (apply 'start-process
		 "expect" (current-buffer) (car program) (cdr program))
	  t))
   ((processp program)
    (cons program nil))
   (t
    (error "Illegal process spec"))))

(defun expect-cleanup (new)
  (if (expect-info-sentinels expect-current-info)
      ;; We started a new process, so we have to kill the
      ;; buffer on exit.
      (if new
	  (expect-exit
	    (expect-kill expect-process))
	(expect-exit
	  (expect-shutdown expect-process)))
    ;; We started a new process, so we have to kill the
    ;; buffer on exit.
    (if new
	(expect ""
	  (expect-kill expect-process))
      (expect ""
	(expect-shutdown expect-process)))))

;;; Indentation and edebug specs.

(put 'expect 'lisp-indent-function 1)
(put 'expect 'edebug-form-spec '(form body))
(put 'expect-exit 'lisp-indent-function 0)
(put 'expect-exit 'edebug-form-spec '(body))
(put 'with-expect 'lisp-indent-function 1)
(put 'with-expect 'edebug-form-spec '(form body))
(put 'with-expect-asynchronous 'lisp-indent-function 1)
(put 'with-expect-asynchronous 'edebug-form-spec '(form body))

(provide 'expect)

;;; expect.el ends here
