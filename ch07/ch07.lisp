;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               ch07.lisp
;;;
;;;   STARTED:            Sun Jun  2 15:01:37 2002
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
;;;    Ex. 7.8
;;;
(defun roughly-equal (x k)
  (find-if #'(lambda (x) ;Ha! Everybody's X!
	       (<= (- k 10) x (+ k 10)))
	   x))

;;;
;;;    Ex. 7.9
;;;
(defun find-nested (l)
  (find-if #'(lambda (elt)
	       (if (listp elt)
		   elt))
	   l))

;;;
;;;    Ex. 7.10
;;;    (See transpose.lisp)
;;;

;;;
;;;    Ex. 7.11
;;;
(defun filter-1-5 (num-list)
  (remove-if-not #'(lambda (x)
		     (and (numberp x)
			  (< 1 x 5)))
		 num-list))

;;;
;;;    Ex. 7.12
;;;
(defun count-the (sentence)
  (length (remove-if-not #'(lambda (symbol)
			     (eq symbol 'the))
			 sentence)))

(defun count-the1 (sentence)
  (let ((count 0))
    (mapc #'(lambda (symbol)
	      (if (eq symbol 'the)
		  (incf count)))
	  sentence)
    count))

;;!!!!!!!!!!!
(defun count-the2 (sentence)
  (count 'the sentence))

;[4]> (count-the '(the quick brown fox jumps over the lazy dog))

;;;
;;;    Ex. 7.13
;;;    (Oops...should be pick-2!!)
;;;    
(defun pick-3 (lol)
  (remove-if-not #'(lambda (l)
		     (= (length l) 3))
		 lol))

(defun pick-3a (lol)
  (mapcan #'(lambda (l)
	      (if (= (length l) 3)
		  (list l)
		  nil))
	  lol))

; [11]> (time (pick-3 '((a b c) (1 2) (d e f) (4 5 6) (9))))

; Real time: 1.42E-4 sec.
; Run time: 0.0 sec.
; Space: 192 Bytes
; ((A B C) (D E F) (4 5 6))
; [12]> (time (pick-3a '((a b c) (1 2) (d e f) (4 5 6) (9))))

; Real time: 1.26E-4 sec.
; Run time: 0.0 sec.
; Space: 180 Bytes
; ((A B C) (D E F) (4 5 6))

;;;
;;;    Ex. 7.14
;;;
(defun my-intersection (a b)
  (remove-if-not #'(lambda (elt)
		     (member elt b))
		 a))

(defun my-union (a b)
  (append a (remove-if #'(lambda (elt)
			   (member elt a))
		       b)))

;;;
;;;    Ex. 7.15
;;;    (See cards.lisp)
;;;

;;;
;;;    Ex. 7.17
;;;
(defun count-flat-1 (lol)
  (length (apply #'append lol)))

(defun count-flat-2 (lol)
  (let ((count 0))
    (mapc #'(lambda (l)
	      (incf count (length l)))
	  lol)
    count))

(defun count-flat-3 (lol)
  (cond ((null lol) 0)
	(t (+ (length (car lol))
	      (count-flat-3 (cdr lol)))) ))

(defun count-flat-4 (lol)
  (let ((count 0))
    (dolist (l lol count)
      (incf count (length l)))) )

(defun count-flat-5 (lol)
  (reduce #'+ (mapcar #'length lol)))

(defun count-flat-6 (lol)
  (length (reduce #'append lol)))  ;Much CONSing!!

;;;
;;;    How can these byte counts be real?
;;;    (Compiled functions)
;;;    
; [21]> (time (count-flat-1 '((a b c) (d e) (f) (g h i))))

; Real time: 5.6E-5 sec.
; Run time: 0.0 sec.
; Space: 48 Bytes
; 9
; [22]> (time (count-flat-2 '((a b c) (d e) (f) (g h i))))

; Real time: 6.0E-5 sec.
; Run time: 0.0 sec.
; Space: 0 Bytes
; 9
; [23]> (time (count-flat-3 '((a b c) (d e) (f) (g h i))))

; Real time: 6.4E-5 sec.
; Run time: 0.0 sec.
; Space: 0 Bytes
; 9
; [24]> (time (count-flat-4 '((a b c) (d e) (f) (g h i))))

; Real time: 6.4E-5 sec.
; Run time: 0.0 sec.
; Space: 0 Bytes
; 9

; [28]> (time (count-flat-5 '((a b c) (d e) (f) (g h i))))

; Real time: 7.1E-5 sec.
; Run time: 0.0 sec.
; Space: 32 Bytes
; 9
; [29]> (time (count-flat-6 '((a b c) (d e) (f) (g h i))))

; Real time: 6.9E-5 sec.
; Run time: 0.0 sec.
; Space: 112 Bytes
; 9
