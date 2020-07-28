;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               ch08.lisp
;;;
;;;   STARTED:            Tue Jun 11 10:38:11 2002
;;;   MODIFICATIONS:
;;;
;;;   PURPOSE:
;;;
;;;
;;;
;;;   CALLING SEQUENCE:
;;;
;;;
;;;   INPUTS:
;;;
;;;   OUTPUTS:
;;;
;;;   EXAMPLE:
;;;
;;;   NOTES:
;;;
;;;

;;;
;;;    Ex. 8.6
;;;
(defun alloddp (numlist)
  (cond ((null numlist) t)
	((evenp (car numlist)) nil)
	(t (alloddp (cdr numlist)))) )

;;;
;;;    This is slightly different. This insists that an odd element exists.
;;;    By contrast, the above function could be named none-even-p.
;;;
(defun alloddp (numlist)
  (cond ((null numlist) nil)
	((evenp (car numlist)) nil) ;Test should really be
				    ;(not (oddp (car numlist)))
	((null (cdr numlist)) t)
	(t (alloddp (cdr numlist)))) )