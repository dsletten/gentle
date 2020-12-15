;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               unary.lisp
;;;;
;;;;   Started:            Fri Aug 23 00:20:11 2019
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

(defpackage :unary (:use :common-lisp :test) (:shadow :number :1+ :1- :< :> := :+ :- :* :/ :mod :zerop :plusp :evenp :oddp))

(in-package :unary)

(defconstant tally 'x)

(defun unaryp (n)
  (every #'(lambda (elt) (eq elt tally)) n))

(deftype unary ()
  '(satisfies unaryp))

(defun unary (n)
  (loop repeat n collect tally))

(defun number (unary)
  (length unary))

(defun 1+ (n)
  (check-type n unary)
  (cons tally n))

(defconstant zero (unary 0))
(defconstant one (1+ zero))
(defconstant two (1+ one))

(defun 1- (n)
  (check-type n unary)
  (assert (plusp n))
  (rest n))

(deftest test-1- ()
  (check
   (= (1- two) one)
   (= (1- one) zero)
   (not (handler-case
            (progn (1- zero)
                   (warn "Should have signalled error."))
          (simple-error () (format t "No negative numbers.~%")))) ))

(defun zerop (n)
  (check-type n unary)
  (null n))

(deftest test-zerop ()
  (check
   (zerop zero)
   (not (zerop (1+ zero)))
   (not (zerop one))
   (zerop (1- one))
   (zerop (1- (1- two)))) )

(defun = (m n)
  (check-type m unary)
  (check-type n unary)
  (cond ((zerop m) (zerop n))
        ((zerop n) nil)
        (t (= (1- m) (1- n)))) )

(deftest test-= ()
  (check
   (= one one)
   (not (= one two))
   (= two two)
   (not (= one (1+ one)))
   (= two (1+ one))
   (= (1+ zero) (1- two))))

(defun > (m n)
  (check-type m unary)
  (check-type n unary)
  (cond ((zerop m) nil)
        ((zerop n) t)
        (t (> (1- m) (1- n)))) )

(deftest test-> ()
  (check
   (> two one)
   (> one zero)
   (not (> one one))
   (not (> zero two))
   (> (1- two) zero)))

(defun < (m n)
  (check-type m unary)
  (check-type n unary)
  (cond ((zerop n) nil)
        ((zerop m) t)
        (t (< (1- m) (1- n)))) )

(deftest test-< ()
  (check
   (< zero one)
   (< one two)
   (< two (1+ two))
   (not (< zero zero))
   (not (< one zero))
   (not (< two one))))

;;;
;;;    m + n = (m + 1) + (n - 1)
;;;    
(defun + (m n)
  (check-type m unary)
  (check-type n unary)
  (if (zerop n)
      m
      (+ (1+ m) (1- n))))

(deftest test-+ ()
  (check
   (= (+ zero zero) zero)
   (= (+ zero one) one)
   (= (+ one zero) one)
   (= (+ one one) two)
   (= (+ two one) (1+ two))))

;;;
;;;    m - n = (m - 1) - (n - 1)
;;;
(defun - (m n)
  (check-type m unary)
  (check-type n unary)
  (cond ((zerop n) m)
        ((zerop m) (error "Cannot compute negative value."))
        (t (- (1- m) (1- n)))) )

(deftest test-- ()
  (check
   (= (- zero zero) zero)
   (= (- one zero) one)
   (= (- one one) zero)
   (= (- two two) zero)
   (= (- two one) one)
   (= (- two zero) two)))

;;;
;;;    m * n = m * n + m - m = m * (n - 1) + m
;;;
(defun * (m n)
  (check-type m unary)
  (check-type n unary)
  (if (zerop n)
      zero
      (+ m (* m (1- n)))) )

(defun * (m n)
  (check-type m unary)
  (check-type n unary)
  (labels ((** (n acc)
             (if (zerop n)
                 acc
                 (** (1- n) (+ acc m)))) )
    (** n zero)))

(deftest test-* ()
  (check
   (= (* (unary 2) (unary 3)) (unary 6))
   (= (* two two) (+ two two))
   (= (* one (unary 8)) (unary 8))
   (= (* two zero) zero)
   (= (* zero two) zero)))

;;;
;;;    m   m - n + n    m - n
;;;    - = --------- = ------ + 1
;;;    n       n         n
;;;    
(defun / (m n)
  (check-type m unary)
  (check-type n unary)
  (cond ((zerop n) (error "Cannot divide by zero."))
        ((< m n) zero)
        (t (1+ (/ (- m n) n)))) )

;; (defun / (m n)
;;   (check-type m unary)
;;   (check-type n unary)
;;   (labels ((// (m n acc)
;;              (cond ((zerop n) (error "Cannot divide by zero."))
;;                    ((< m n) acc)
;;                    (t (// (- m n) n (1+ acc)))) ))
;;     (// m n zero)))

;;;
;;;    Modified from Oz version.
;;;    
(defun / (m n)
  (check-type m unary)
  (check-type n unary)
  (labels ((// (m acc)
             (if (< m n)
		 acc
                 (// (- m n) (1+ acc)))) )
    (if (zerop n) 
	(error "Cannot divide by zero.")
        (// m zero))))

(deftest test-/ ()
  (check
   (= (/ (unary 4) two) two)
   (= (/ (unary 5) two) two)
   (= (/ (unary 6) two) (unary 3))
   (= (/ (* two two) (* two two)) one)
   (= (/ zero one) zero)))

(defun mod (m n)
  (check-type m unary)
  (check-type n unary)
  (cond ((zerop n) (error "Cannot divide by zero."))
        ((< m n) m)
        (t (mod (- m n) n))))

(deftest test-mod ()
  (check
   (= (mod (unary 5) two) one)
   (= (mod (unary 7) two) one)
   (= (mod (unary 6) two) zero)
   (= (mod (unary 9) (unary 4)) one)
   (= (mod (unary 9) (unary 5)) (unary 4))))

(defun plusp (n)
  (check-type n unary)
  (not (zerop n)))

(deftest test-plusp ()
  (check
   (plusp one)
   (plusp two)
   (not (plusp zero))
   (plusp (1+ zero))
   (not (plusp (1- one)))) )

(defun evenp (n)
  (check-type n unary)
  (zerop (mod n two)))

(deftest test-evenp ()
  (check
   (evenp zero)
   (evenp two)
   (evenp (+ one one))
   (not (evenp one))
   (not (evenp (+ one two)))) )

(defun oddp (n)
  (check-type n unary)
  (not (evenp n)))

(deftest test-oddp ()
  (check
   (oddp one)
   (oddp (- two one))
   (oddp (+ two one))
   (not (oddp zero))
   (not (oddp two))))
