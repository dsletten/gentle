;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               DNA.lisp
;;;
;;;   STARTED:            Sat Jul 20 01:51:02 2002
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

(let ((base-pairs '((a t)
		    (t a)
		    (g c)
		    (c g))))
  (defun complement-base (base)
    "Return the complementary base of a given base."
    (second (assoc base base-pairs))))

(defun complement-strand (strand)
  "Determine the strand of complementary bases given a strand."
  (mapcar #'complement-base strand))

(defun make-double (strand)
  "Build a double strand of DNA given one strand."
  (mapcar #'list strand (complement-strand strand)))

(defun count-bases (strand)
  (let ((a-count 0)
	(c-count 0)
	(g-count 0)
	(t-count 0))
    (labels ((count-aux (strand)
	       (cond ((null strand) nil)
		     ((atom strand) (case strand
				      (a (incf a-count))
				      (c (incf c-count))
				      (g (incf g-count))
				      ((t) (incf t-count))))
		     (t (count-aux (car strand))
			(count-aux (cdr strand)))) ))
      (count-aux strand))
    (list (list 'a a-count)
	  (list 'c c-count)
	  (list 'g g-count)
	  (list 't t-count))))

(defun count-bases (strand)
  (let ((a-count 0)
	(c-count 0)
	(g-count 0)
	(t-count 0))
    (labels ((count-base (base)
	       (case base
		 (a (incf a-count))
		 (c (incf c-count))
		 (g (incf g-count))
		 ((t) (incf t-count)))) )
      (dolist (elt strand)
	(cond ((atom elt) (count-base elt))
	      (t (count-base (first elt))
		 (count-base (second elt)))) ))
    (list (list 'a a-count)
	  (list 'c c-count)
	  (list 'g g-count)
	  (list 't t-count))))

(defun prefixp (strand1 strand2)
  (cond ((null strand1) t)
	((eql (car strand1) (car strand2))
	 (prefixp (cdr strand1) (cdr strand2)))
	(t nil)))

(defun prefixp (strand1 strand2)
  (do ((s1 strand1 (cdr s1))
       (s2 strand2 (cdr s2)))
      ((null s1) t)
    (unless (eql (car s1) (car s2))
      (return nil))))

(defun appearsp (strand1 strand2)
  (cond ((null strand1) t)
	((null strand2) nil)
	((eql (car strand1) (car strand2))
	 (or (appearsp-aux (cdr strand1) (cdr strand2))
	     (appearsp strand1 (cdr strand2))))
	(t (appearsp strand1 (cdr strand2)))) )

(defun appearsp-aux (strand1 strand2)
  (cond ((null strand1) t)
	((null strand2) nil)
	((eql (car strand1) (car strand2))
	 (appearsp-aux (cdr strand1) (cdr strand2)))
	(t nil)))

;;;
;;;    The following was inspired by Prolog relation sublist/2
;;;    (See Bratko notes.)
;;;
(defun appearsp (strand1 strand2 &optional (alt nil alt-flag))
  (cond ((null strand1) t)
	((null strand2) nil)
	((eql (car strand1) (car strand2))
	 (or (appearsp (cdr strand1) (cdr strand2) nil)
	     (if alt-flag
		 alt
		 (appearsp strand1 (cdr strand2)))) )
	(t (appearsp strand1 (cdr strand2)))) )

(defun appearsp (strand1 strand2)
  (do ((s2 strand2 (cdr s2)))
      ((null s2) nil)
    (when (prefixp strand1 s2)
      (return t))))

;;;
;;;    Assumes neither strand is ()
;;;    
(defun coverp (strand1 strand2)
  (let ((l1 (length strand1)))
    (do ((s2 strand2 (nthcdr l1 s2)))
	((null s2) t)
      (unless (prefixp strand1 s2)
	(return nil)))) )

;;;
;;;    This would be slicker with a new definition of PREFIXP which returns
;;;    the list following the prefix rather than just t. We have to return t
;;;    if the two are EQUAL though, since there's nothing after the prefix in
;;;    that case.
;;;
(defun prefixp (strand1 strand2)
  (do ((s1 strand1 (cdr s1))
       (s2 strand2 (cdr s2)))
      ((null s1) (or s2 t))
    (unless (eql (car s1) (car s2))
      (return nil))))

(defun coverp (strand1 strand2)
    (do ((s2 strand2 (prefixp strand1 s2)))
	((atom s2) s2)))

(defun prefix (n strand)
  (let ((result ()))
    (dotimes (i n (nreverse result))
      (push (pop strand) result))))

(defun prefix (n strand)
  (subseq strand 0 n))

;;;
;;;    This is consistently fastest!
;;;    
(defun kernel (strand)
  (do ((s1 (cdr strand) (cdr s1))
       (kernel (list (car strand)) (append kernel (list (car s1)))) )
      ((null s1) kernel)
    (when (coverp kernel s1)
      (return kernel))))

;;;
;;;    Touretzky's solution
;;;
(defun kernel (strand)
  (do ((i 1 (+ i 1)))
      ((coverp (prefix i strand) strand) (prefix i strand))))

;;;
;;;    My improvement (? Time is about the same...His conses more.)
;;;
(defun kernel (strand)
  (do* ((i 1 (+ i 1))
	(prefix (prefix i strand) (prefix i strand)))
       ((coverp prefix strand) prefix)))

(defun draw-dna (strand)
  (let ((l (length strand)))
    (draw-segment l "-----")
    (draw-segment l "  !  ")
    (draw-strand strand)
    (draw-segment l "  .  ")
    (draw-segment l "  .  ")
    (draw-strand (complement-strand strand))
    (draw-segment l "  !  ")
    (draw-segment l "-----")))

(defun draw-segment (n s)
  (dotimes (i n)
    (format t "~A" s))
  (format t "~%"))

(defun draw-strand (strand)
  (dolist (base strand)
    (format t "  ~A  " base))
  (format t "~%"))