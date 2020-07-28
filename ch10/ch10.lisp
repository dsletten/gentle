;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               ch10.lisp
;;;
;;;   STARTED:            Wed Jul 10 04:06:35 2002
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
;;;    Ex. 10.3
;;;
(let ((friends ())
      (acquaintance-count 0))
  (defun meet (person)
    (cond ((eq person (first friends))
	   (incf acquaintance-count)
	   'we-just-met)
	  ((member person friends)
	   (incf acquaintance-count)
	   'we-know-each-other)
	  (t (push person friends)
	     'pleased-to-meet-you)))

  (defun show-friends ()
    friends)

  (defun get-acquaintance-count ()
    acquaintance-count)

  (defun forget (person)
    (cond ((member person friends) (delete person friends))
	  (t 'no-friend-of-mine))))

;;;
;;;    Ex. 10.9
;;;
(defun chop (l)
  (cond ((null l) l)
	((listp l) (setf (cdr l) nil) l)
	(t l)))

;;;
;;;    Touretzky's version (Duh!)
;;;
(defun chop (l)
  (if (consp l) (setf (cdr l) nil))
  l)

;;;
;;;    Ex. 10.10
;;;
(defun ntack (l sym)
  (if (listp l)
      (nconc l (list sym))
      l))
