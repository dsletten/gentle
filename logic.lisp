;#!/usr/local/bin/clisp

;;
;   NAME:               logic.lsp
;
;   STARTED:            001105
;   MODIFICATIONS:
;
;   PURPOSE:
;      Various logic/Boolean functions
;
;
;   CALLING SEQUENCE:
;
;
;   INPUTS:
;
;   OUTPUTS:
;
;   EXAMPLE:
;
;   NOTES:
;
;;

#|Basic definitions using AND and OR primitives|#
(defun logical-and-1 (p q)
  (cond (p (cond (q t)))) )

(defun logical-and-2 (p q)
  (and p q t) )

(defun logical-or-1 (p q)
  (cond (p t)
	(q t)) )

(defun logical-or-2 (p q)
  (or (and p t)
      (and q t)) )

#|   Definitions in terms of NAND   |#
(defun nand (p q)
  (not (and p q)) )

(defun not-nand (p)
  (nand p p) )

(defun logical-and-nand (p q)
  (nand (nand p q)
	(nand p q)) )

(defun logical-or-nand (p q)
  (nand (nand p p)
	(nand q q)) )

#|   Definitions in terms of NOR   |#
(defun nor (p q)
  (not (or p q)) )

(defun not-nor (p)
  (nor p p) )

(defun logical-and-nor (p q)
  (nor (nor p p)
       (nor q q)) )

(defun logical-or-nor (p q)
  (nor (nor p q)
       (nor p q)) )

(defun nand-nor (p q)
  (nor (nor (nor p p)
	    (nor q q))
       (nor (nor p p)
	    (nor q q))) )

#|   Often the following are equivalent:
(if p q r)

and

(or (and p q) r)

However, the equivalence fails when q evaluates to NIL.
In this case, the OR version returns the value of r, which
may or may not be the correct value NIL. For instance:
(if t nil 'foo)

is not equivalent to

(or (and t nil) 'foo)

The correct general equivalence is as follows:
(or (and p q)
    (and (not p) r))

Here if p is not-NIL, then either q evaluates to not-NIL and this 
becomes the value of OR, or q is NIL and the final AND returns NIL
after evaluating (not p).
|#