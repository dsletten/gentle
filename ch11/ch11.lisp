;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               ch11.lisp
;;;
;;;   STARTED:            Tue Jul 16 11:48:17 2002
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
(load "~/lisp/programs/utils.lisp")
;;;
;;;    Ex. 11.1
;;;
(defun it-member (key l)
  "Determine whether or not KEY is an element of the list L."
  (dolist (elt l nil)
    (when (eql elt key)
      (return t))))

(test 'it-member '(((a (g o r g o n z o l a)) t)
		   ((p (r a b i d)) nil)))
;;;
;;;    Ex. 11.2
;;;
(defun it-assoc (key a-list)
  "Determine whether an entry using KEY exists in association list A-LIST."
  (dolist (l a-list nil)
    (when (eql (car l) key)
      (return l))))

(test 'it-assoc '(((pung ((pung foo) (bar baz) (quord freen))) (pung foo))
		  ((quibble ((norg bleen) (mak trung))) nil)))

;;;
;;;    Ex. 11.3
;;;
(defun check-all-odd (l)
  "Determine whether or not all elements in L are odd."
  (cond ((null l) t)
	(t (format t "Checking ~S...~%" (car l))
	   (if (oddp (car l)) 
	       (check-all-odd (cdr l))
	       nil))))

(test 'check-all-odd '((((1 3 5)) t)
		       (((1 3 4 5)) nil)))
;;;
;;;    Ex. 11.4
;;;
(defun it-length (l)
  "Determine the length of list L iteratively."
  (let ((i 0))
    (dolist (elt l i)
      (incf i))))

(test 'it-length '((((a b c d)) 4)
		   (((a b c d e f g h)) 8)
		   ((()) 0)))

;;;
;;;    Ex. 11.5
;;;
(defun it-nth (n l)
  "Find the Nth element in list L."
  (let ((l1 l))
    (dotimes (i n (car l1))
      (setf l1 (cdr l1)))) )

(test 'it-nth '(((4 (a b c d e f)) e)
		((3 ((0) (1) (2) (3) (4))) (3))
		((5 (a b c d)) ())))

;;;
;;;    Ex. 11.6
;;;
(defun it-union (a b)
  "Determine the union of sets A and B."
  (let ((result b))
    (dolist (elt a result)
      (unless (member elt result)
	(push elt result)))) )

(test 'it-union '((((a b c) (d e a)) (c b d e a))
		  (((a b c) (a b c)) (a b c))
		  ((() (a b c)) (a b c))
		  (((a b c) ()) (c b a))))

;;;
;;;    Ex. 11.8
;;;
(defun it-reverse (l)
  "Reverse a list using iteration."
  (let ((result ()))
    (dolist (elt l result)
      (push elt result))))

(test 'it-reverse '((((a b c d e f)) (f e d c b a))
		    ((((a b) (c d))) ((c d) (a b)))) )

;;;
;;;    Ex. 11.11
;;;
; (defun find-largest (list-of-numbers)
;   (let ((largest (first list-of-numbers)))
;     (dolist (element (rest list-of-numbers) largest)
;       (when (> element largest)
; 	(setf largest element)))) )
(defun find-largest (numlist)
  "Find the largest (max) number in a list of numbers."
  (do* ((l (cdr numlist) (cdr l))
	(elt (car numlist) (car l))
	(largest elt))
       ((null l) largest)
    (when (> elt largest)
      (setf largest elt))))

(test 'find-largest '((((8 0 -9 12 15)) 15)
		      ((()) nil)
		      (((19 1 2 3)) 19)
		      (((7 7 7 7 7)) 7)))

;;;
;;;    Ex. 11.12
;;;
; (defun power-of-2 (n)
;   (let ((result 1))
;     (dotimes (i n result)
;       (incf result result))))
(defun power-of-2 (n)
  "Compute 2 to the Nth power."
  (assert (typep n '(integer 0 *)) (n) "N must be a non-negative integer.")
  (do ((pow 1 (* pow 2))
       (i n (1- i)))
      ((zerop i) pow)))

(test 'power-of-2 '(((3) 8)
		    ((10) 1024)
		    ((0) 1)))
	