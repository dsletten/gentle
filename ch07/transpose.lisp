;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               transpose.lisp
;;;
;;;   STARTED:            Sun Jun  2 15:13:50 2002
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

(let ((note-table '((c 1)
		    (c-sharp 2)
		    (d 3)
		    (d-sharp 4)
		    (e 5)
		    (f 6)
		    (f-sharp 7)
		    (g 8)
		    (g-sharp 9)
		    (a 10)
		    (a-sharp 11)
		    (b 12))))

  (defun numbers (note-list)
    (mapcar #'(lambda (note)
		(second (assoc note note-table)))
	    note-list))

  (defun notes (num-list)
    (mapcar #'(lambda (num)
		(first (rassoc num note-table :key #'car)))
	    num-list))

  (defun raise (n num-list)
    (mapcar #'(lambda (x)
		(+ x n))
	    num-list))

  (defun normalize (num-list)
    (mapcar #'(lambda (x)
		(1+ (mod (+ x 11) 12)))
	    num-list))

  (defun transpose (n song)
    (notes (normalize (raise n (numbers song)))) ))

