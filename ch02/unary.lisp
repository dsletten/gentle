;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               unary.lisp
;;;;
;;;;   Started:            Tue May  7 01:40:09 2013
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

(defpackage :unary (:use :common-lisp) (:shadow :1+ :1- :+ :- :* :/ :number :zerop :plusp :evenp :oddp :mod := :> :<))

(in-package :unary)

(defconstant tally 'x)

(defun zerop (n)
  (null n))

(defun 1+ (n)
  (cons tally n))

(defun 1- (n)
  (if (zerop n)
      (error "Undefined subtraction.")
      (rest n)))

(defconstant zero '())
(defconstant one (1+ zero))
(defconstant two (1+ one))

(defun plusp (n)
  (> n zero))

;;;
;;;    I.e., (not (zerop n))
;;;    
;; (defun plusp (n)
;;   (car n))

(defun evenp (n)
  (zerop (mod n two)))

(defun oddp (n)
  (not (evenp n)))

;;;
;;;    m + n = (m + 1) + (n - 1)
;;;    
(defun + (m n)
  (if (zerop n)
      m
      (+ (1+ m) (1- n))))

;; (defun + (m n)
;;   (append m n))

;;;
;;;    m - n = (m - 1) - (n - 1), m >= n
;;;    
(defun - (m n)
  (cond ((zerop n) m)
        ((zerop m) (error "Undefined subtraction."))
        (t (- (1- m) (1- n)))) )

;;;
;;;    m * n = m * (n - 1) + m
;;;    
(defun * (m n)
  (cond ((zerop n) zero)
        ((zerop m) zero)
        (t (+ m (* m (1- n)))) ))

;;;
;;;    m   m - n
;;;    - = ----- + 1
;;;    n     n
;;;    
(defun / (m n)
  (cond ((zerop n) (error "Attempt to divide by zero."))
        ((< m n) zero)
        (t (1+ (/ (- m n) n)))) )

(defun mod (m n)
  (cond ((zerop n) (error "Attempt to divide by zero."))
        ((< m n) m)
        (t (mod (- m n) n))))
  
(defun = (m n)
  (cond ((zerop m) (zerop n))
        ((zerop n) nil)
        (t (= (1- m) (1- n)))))

(defun > (m n)
  (cond ((zerop m) nil) ; Zero is not greater than anything.
        ((zerop n) t)   ; Everything (besides zero) is greater than zero.
        (t (> (1- m) (1- n)))) )

(defun < (m n)
  (cond ((zerop n) nil) ; Nothing is less than zero.
        ((zerop m) t)   ; Zero is less than anything (besides zero).
        (t (< (1- m) (1- n)))) )

(defun number (n)
  (length n))

(defun unary (n)
  (make-list n :initial-element tally))
