;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               blocks.lisp
;;;
;;;   STARTED:            Sun Jun  9 18:32:45 2002
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

(let ((database '((b1 shape brick)
		  (b1 color green)
		  (b1 size small)
		  (b1 supported-by b2)
		  (b1 supported-by b3)
		  (b2 shape brick)
		  (b2 color red)
		  (b2 size small)
		  (b2 supports b1)
		  (b2 left-of b3)
		  (b3 shape brick)
		  (b3 color red)
		  (b3 size small)
		  (b3 supports b1)
		  (b3 right-of b2)
		  (b4 shape pyramid)
		  (b4 color blue)
		  (b4 size large)
		  (b4 supported-by b5)
		  (b5 shape cube)
		  (b5 color green)
		  (b5 size large)
		  (b5 supports b4)
		  (b6 shape brick)
		  (b6 color purple)
		  (b6 size large))))

  ;;;
  ;;;    Touretzky beats me on the first one:
  ;;;    (I modified MATCH-ELEMENT to mirror my version.)
  ;;;    His MATCH-TRIPLE is pretty slick, but mine is cute.
  ;;;    (His won't detect lists of unequal lengths!)
  ;;;    
;   (defun match-element (s1 s2)
;     (or (eq s1 s2) (eq s1 '?) (eq s2 '?)))
;   (defun match-triple (assertion pattern)
;     (every #'match-element assertion pattern))
  
  (defun match-element (s1 s2)
    (cond ((eq s1 s2) t)
	  ((or (eq s1 '?)
	       (eq s2 '?))
	   t)
	  (t nil)))

  (defun match-triple (assertion pattern)
    (cond ((null assertion) (null pattern))
	  ((null pattern) nil)
	  ((match-element (car assertion) (car pattern))
	   (match-triple (cdr assertion) (cdr pattern)))
	  (t nil)))

  (defun fetch (query)
    (remove-if-not #'(lambda (assertion)
		       (match-triple assertion query))
		   database))

  (defun block-color-query (block)
    (list block 'color '?))

  (defun supporters (block)
    (mapcar #'first (fetch (list '? 'supports block))))

  (defun supp-cube-p (block)
    (and (find-if #'(lambda (supporter)
		      (fetch (list supporter 'shape 'cube)))
		  (supporters block)) t))

  (defun desc1 (block)
    (fetch (list block '? '?)))

  (defun desc2 (block)
    (mapcar #'cdr (desc1 block)))

  (defun description (block)
    (apply #'append (desc2 block))))

