;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch03.lisp
;;;;
;;;;   Started:            Sat Aug 24 23:11:36 2019
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes:
;;;;
;;;;
(load "/home/slytobias/lisp/packages/lang.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :ch03 (:use :common-lisp :lang :test) (:shadow :length))

(in-package :ch03)

;;;
;;;    3.5
;;;
(defun half (x)
  (/ x 2))

(deftest test-half ()
  (check
   (= (half 8) 4)
   (= (half 0) 0)
   (= (half 3) 3/2)
   (= (half 3d0) 1.5d0)))

(defun cube (x)
  (* x x x))

(deftest test-cube ()
  (check
   (= (cube 5) 125)
   (= (cube 3.0) 27.0)
   (= (cube -2) -8)
   (zerop (cube 0))
   (= (cube 2) 8)
   (= (cube 0) 0)
   (= (cube -3) -27)
   (approximately= (cube pi) 31.006276680299816D0)))

(defun onemorep (x y)
  (= x (1+ y)))

(deftest test-onemorep ()
  (check
   (onemorep 3 2)
   (onemorep 3.1 2.1)
   (onemorep 3.1d0 2.1d0)
   (not (onemorep 0 5))
   (onemorep 9 8)
   (onemorep 5/2 3/2)
   (not (onemorep 8 9))
   (not (onemorep 0 0))))

;;;
;;;    3.6
;;;
(defun hypotenuse (a b)
  (sqrt (+ (* a a) (* b b))))

(deftest test-hypotenuse ()
  (check
   (= (hypotenuse 3 4) 5)
   (= (hypotenuse 3d0 4d0) 5d0)
   (= (hypotenuse 5 12) 13)
   (= (hypotenuse 5d0 12d0) 13d0)))

(defun num-list-p (l)
  (or (null l)
      (and (numberp (first l))
           (num-list-p (rest l)))) )

(deftype num-list ()
  '(satisfies num-list-p))

(defun pythag (&rest nums)
  (assert (typep nums 'num-list))
  (sqrt (reduce #'+ (mapcar #'(lambda (x) (* x x)) nums))))

(deftest test-pythag ()
  (check
   (zerop (pythag))
   (= (pythag 2) 2)
   (= (pythag 3 4) 5)
   (= (pythag 3 4 12) 13)))

;;;
;;;    3.7
;;;
(defun miles-per-gallon (initial-odometer-reading final-odometer-reading gallons-consumed)
  (/ (- final-odometer-reading initial-odometer-reading) gallons-consumed))

(deftest test-miles-per-gallon ()
  (check
   (= (miles-per-gallon 0 420 18) 70/3)
   (= (miles-per-gallon 0 420.3d0 18) 23.35D0)))

;;;
;;;    3.11
;;;
(defun longer-than (l1 l2)
  (> (cl:length l1) (cl:length l2)))

;;;
;;;    Empty list is not longer than anything.
;;;    Any non-empty list is longer than an empty list.
;;;    Otherwise compare tails.
;;;
(defun longer-than-rec (l1 l2)
  (cond ((endp l1) nil)
        ((endp l2) t)
        (t (longer-than-rec (rest l1) (rest l2)))) )

(deftest test-longer-than ()
  (check
   (longer-than '(a b c) '(a b))
   (longer-than '(a) '())
   (not (longer-than '(a b) '(a b c)))
   (not (longer-than '(a b c) '(a b c)))
   (eq (longer-than #1='(a b c) #2='(a b)) (longer-than-rec #1# #2#))
   (eq (longer-than #2# #1#) (longer-than-rec #2# #1#))
   (eq (longer-than #1# #1#) (longer-than-rec #1# #1#))))

;;;
;;;    3.12
;;;
;;;    Four solutions:
;;;    1. Use built-in LENGTH
;;;    2. Implement LENGTH
;;;    3. Implement LENGTH properly
;;;    4. Call ADD-LENGTH itself recursively. Have to grab length off
;;;       head of recursive result and throw rest away...
;;;       
(defun add-length (l)
  (cons (cl:length l) l))

;;;
;;;    This is dumb--it just implements LENGTH...
;;;    
(defun add-length (l)
  (labels ((length (l)
             (if (endp l)
                 0
                 (1+ (length (rest l)))) ))
    (cons (length l) l)))

;;;
;;;    At least make it tail-recursive!
;;;    
(defun add-length (l)
  (labels ((length (l result)
             (if (endp l)
                 result
                 (length (rest l) (1+ result)))) )
    (cons (length l 0) l)))

;;;
;;;    This is crazy...
;;;    
(defun add-length (l)
  (if (null l)
      (list 0)
      (destructuring-bind (length . tail) (add-length (rest l))
        (declare (ignore tail))
        (cons (1+ length) l))))

(deftest test-add-length ()
  (check
   (equal (add-length '()) '(0))
   (equal (add-length (add-length '())) '(1 0))
   (equal (add-length '(a b c)) '(3 a b c))
   (equal (add-length '(moo goo gai pan)) '(4 MOO GOO GAI PAN))
   (equal (add-length (add-length '(a b c))) '(4 3 A B C))))

;;;
;;;    3.22d
;;;    
(defun firstp (sym list)
  (eq sym (first list)))

(deftest test-firstp ()
  (check
   (firstp 'foo '(foo bar baz))
   (not (firstp 'boing '(foo bar baz)))) )

;;;
;;;    3.22e
;;;    
(defun mid-add1 (l)
  (list* (first l) (1+ (second l)) (rest (rest l))))

(defun mid-add1 (l)
  (destructuring-bind (a b c) l
    (list a (1+ b) c)))

(deftest test-mid-add1 ()
  (check
   (equal (mid-add1 '(take 2 cookies)) '(take 3 cookies))))

;;;
;;;    3.22f
;;;
;;; C = 5/9(F + 40) - 40
;;; F = 9/5(C + 40) - 40
;;; 
(defun convert (scale temp)
  (- (* scale (+ temp 40)) 40))

(defun f->c (f)
  (convert 5/9 f))

(defun c->f (c)
  (convert 9/5 c))

(deftest test-f->c ()
  (check
   (= (f->c 32) 0)
   (= (f->c 98.6d0) 37)
   (= (f->c 212) 100)))

(deftest test-c->f ()
  (check
   (= (c->f 0) 32)
   (= (coerce (c->f 37) 'double-float) 98.6d0) ; ????
   (= (c->f 100) 212)
   (= (f->c (c->f 80)) 80)))

;(loop for i from 0 to 200 do (unless (= i (f->c (c->f i)) (c->f (f->c i))) (print i)))

