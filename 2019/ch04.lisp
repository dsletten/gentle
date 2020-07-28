;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               ch04.lisp
;;;;
;;;;   Started:            Sun Sep  1 02:00:22 2019
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
(load "/home/slytobias/lisp/packages/test.lisp")
;(load "/Users/dsletten/lisp/packages/collections.lisp")

(defpackage :ch04 (:use :common-lisp :test) (:shadow :not))
;(defpackage :ch04 (:use :common-lisp :test :collections))

(in-package :ch04)

;;;
;;;    4.1
;;;    
(defun make-even (n)
  (if (oddp n)
      (1+ n)
      n))

(deftest test-make-even ()
  (check
   (= (make-even 0) 0)
   (= (make-even 1) 2)
   (= (make-even 2) 2)
   (= (make-even -1) 0)
   (= (make-even -2) -2)))

;;;
;;;    4.2
;;;    
;; (defun further (x)
;;   (if (plusp x)
;;       (1+ x)
;;       (1- x)))

(defun further (x)
  (if (plusp x)
      (1+ x)
      (if (minusp x)
          (1- x)
          x)))

(defun further (x)
  (cond ((plusp x) (1+ x))
        ((minusp x) (1- x))
        (t x)))

(defun further (x)
  (if (zerop x)
      x
      (+ x (signum x))))

(defun further (x)
  (if (zerop x)
      x
      (+ x (/ (abs x) x)))) ; Poor man's SIGNUM

(deftest test-further ()
  (check
   (= (further 1) 2)
   (= (further 0.5) 1.5)
   (= (further 3/2) 5/2)
   (= (further -1) -2)
   (= (further -0.5) -1.5)
   (= (further -3/2) -5/2)
   (= (further 0) 0)
   (= (further 0.0) 0.0)))

;;;
;;;    4.3
;;;    
(defun not (p)
  (if p nil t))

(deftest test-not ()
  (check
   (eq (not #1=(= 2 3)) (cl:not #1#))
   (eq (not #2=(< 2 3)) (cl:not #2#))))

;;;
;;;    4.10
;;;
(defun constrain (x min max)
  (cond ((< x min) min)
        ((> x max) max)
        (t x)))

(defun constrain (x min max)
  (if (< x min)
      min
      (if (> x max)
          max
          x)))

(defun constrain (x min max)
  (max (min x max) min))

(deftest test-constrain ()
  (check
   (= (constrain 3 -50 50) 3)
   (= (constrain 92 -50 50) 50)
   (= (constrain -1 0 10) 0)))

;;;
;;;    4.11
;;;
(defun first-zero (nums)
  (cond ((zerop (first nums)) 'first)
        ((zerop (second nums)) 'second)
        ((zerop (third nums)) 'third)
        (t 'none)))

(deftest test-first-zero ()
  (check
   (eq (first-zero '(0 3 4)) 'first)
   (eq (first-zero '(3 0 4)) 'second)
   (eq (first-zero '(3 4 0)) 'third)
   (eq (first-zero '(1 2 3)) 'none)))

;;;
;;;    4.12
;;;
(defconstant limit 99)
(defun cycle (n)
  (cond ((= n limit) 1)
        (t (1+ n))))

(defun cycle (n)
  (1+ (mod n limit)))

;; (let ((ring (make-circular-list (loop for i from 1 limit collect i))))
;;   (defun cycle (n)
;;     (

(deftest test-cycle ()
  (check
   (every #'(lambda (x y) (= (1+ x) y)) (loop for i from 1 to (1- limit) collect i) (loop for i from 1 to (1- limit) collect (cycle i)))
   (= (cycle limit) 1)))

;;;
;;;    4.13
;;;
(defun how-compute (op1 op2 val)
  (cond ((= (+ op1 op2) val) 'sum-of)
        ((= (* op1 op2) val) 'product-of)
        (t '(beats me))))

(deftest test-how-compute ()
  (check
   (eq (how-compute 3 4 7) 'sum-of)
   (eq (how-compute 3 4 12) 'product-of)
   (equal (how-compute 1 2 9) '(beats me))
   (eq (how-compute 2 2 4) 'sum-of) ;????
   (eq (how-compute 0 0 0) 'sum-of))) ;????

;;;
;;;    4.16
;;;
(defun fancy (x)
  (cond ((and (oddp x) (plusp x)) (* x x))
        ((and (oddp x) (minusp x)) (* 2 x))
        (t (/ x 2))))

(defun fancy (x)
  (cond ((typep x '(and (integer 1) (satisfies oddp))) (* x x))
        ((typep x '(and (integer * (0)) (satisfies minusp))) (* 2 x))
        (t (/ x 2))))

(deftest test-fancy ()
  (check
   (= (fancy 3) 9)
   (= (fancy -7) -14)
   (= (fancy pi) (/ pi 2)))) ; First version of FANCY => error

;;;
;;;    4.17
;;;
(defun categorize (sex age)
  (cond ((or (eq sex 'boy) (eq sex 'girl)) (eq age 'child))
        ((or (eq sex 'man) (eq sex 'woman)) (eq age 'adult))
        (t nil)))

(defun categorize (sex age)
  (case sex
    ((boy girl) (eq age 'child))
    ((man woman) (eq age 'adult))
    (otherwise nil)))

(deftest test-categorize ()
  (check
   (categorize 'boy 'child)
   (categorize 'girl 'child)
   (categorize 'man 'adult)
   (categorize 'woman 'adult)
   (cl:not (categorize 'boy 'adult))
   (cl:not (categorize 'girl 'adult))
   (cl:not (categorize 'man 'child))
   (cl:not (categorize 'woman 'child))))

;;;
;;;    4.18
;;;
(defun play (first second)
  (cond ((eq first second) 'tie)
        ((or (and (eq first 'rock) (eq second 'paper))
             (and (eq first 'paper) (eq second 'scissors))
             (and (eq first 'scissors) (eq second 'rock)))
         'second-wins)
        (t 'first-wins)))

(defun play (first second)
  (ecase first
    (rock (ecase second
            (rock 'tie)
            (paper 'second-wins)
            (scissors 'first-wins)))
    (paper (ecase second
             (rock 'first-wins)
             (paper 'tie)
             (scissors 'second-wins)))
    (scissors (ecase second
                (rock 'second-wins)
                (paper 'first-wins)
                (scissors 'tie)))) )

(defvar *jan-ken-pon* '((rock (rock scissors paper))
                        (paper (paper rock scissors))
                        (scissors (scissors paper rock))))
(defun play (first second)
  (destructuring-bind (_ (tie win lose)) (assoc first *jan-ken-pon*)
    (declare (ignore _))
    (cond ((eq second tie) 'tie)
          ((eq second win) 'first-wins)
          ((eq second lose) 'second-wins))))

;(dolist (first #1='(rock paper scissors)) (dolist (second #1#) (print `(eq (play ',first ',second)))))

(deftest test-play ()
  (check
   (EQ (PLAY 'ROCK 'ROCK) 'tie) 
   (EQ (PLAY 'ROCK 'PAPER) 'second-wins) 
   (EQ (PLAY 'ROCK 'SCISSORS) 'first-wins) 
   (EQ (PLAY 'PAPER 'ROCK) 'first-wins) 
   (EQ (PLAY 'PAPER 'PAPER) 'tie) 
   (EQ (PLAY 'PAPER 'SCISSORS) 'second-wins) 
   (EQ (PLAY 'SCISSORS 'ROCK) 'second-wins) 
   (EQ (PLAY 'SCISSORS 'PAPER) 'first-wins) 
   (EQ (PLAY 'SCISSORS 'SCISSORS) 'tie)))

;;;
;;;    4.29
;;;
(defun logical-and (p q)
  (if p
      (if q
          t
          nil)
      nil))

(defun logical-and (p q)
  (cond (p (cond (q t) (t nil)))
        (t nil)))

(deftest test-logical-and ()
  (check
   (eq (logical-and 2 4) t)
   (eq (logical-and "pung" 'foo) t)
   (eq (logical-and t (oddp 3)) t)
   (eq (logical-and nil t) nil)
   (eq (logical-and nil 8) nil)
   (eq (logical-and t nil) nil)
   (eq (logical-and (> 4 3) (< 4 3)) nil)))

;;;
;;;    4.30
;;;
(defun logical-or (p q)
  (or (and p t)
      (and q t)))

(defun logical-or (p q)
  (if p
      t
      (if q
          t
          nil)))

(defun logical-or (p q)
  (cond (p t)
        (q t)
        (t nil)))

(deftest test-logical-or ()
  (check
   (eq (logical-or 2 4) t)
   (eq (logical-or "pung" 'foo) t)
   (eq (logical-or nil 4) t)
   (eq (logical-or 4 nil) t)
   (eq (logical-or (> 4 3) (> 3 4)) t)
   (eq (logical-or nil nil) nil)
   (eq (logical-or (> 2 3) (> 3 4)) nil)))

