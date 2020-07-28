;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               craps.lisp
;;;;
;;;;   Started:            Tue Aug 20 13:42:56 2013
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
;;;;   Notes: Touretzky's output is slightly wrong. (SNAKEYES, --)
;;;;   His solution uses MEMBER and APPEND, both introduced in chapter 6!
;;;;
;;;;   See ANNOUNCE function in 2002 version.
;;;;
;;;;
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :craps (:use :common-lisp :test))

(in-package :craps)

(defun throw-die ()
  "Return a number from 1 to 6, inclusive, representing a throw of a 6-sided die."
  (1+ (random 6)))

(deftest test-throw-die ()
  (check
   (every #'(lambda (throw) (<= 1 throw 6)) (loop repeat 10000 collect (throw-die)))) )

(defun throw-dice ()
  "Return a list representing the result of throwing two 6-sided dice."
  (list (throw-die) (throw-die)))

(defun snake-eyes-p (throw)
  "Is the throw a pair of ones?"
  (equal throw '(1 1)))

(deftest test-snake-eyes-p ()
  (check
   (snake-eyes-p '(1 1))
   (not (snake-eyes-p '(2 3)))
   (not (snake-eyes-p '(1)))) )

(defun box-cars-p (throw)
  "Is the throw a pair of sixes?"
  (equal throw '(6 6)))

(deftest test-box-cars-p ()
  (check
   (box-cars-p '(6 6 ))
   (not (box-cars-p '(2 3)))
   (not (box-cars-p '(6)))) )

(defun instant-win-p (throw)
  "Is the throw an instant win?"
  (assert (= (length throw) 2) (throw) "Bad throw: ~A" throw)
  (case (throw-value throw)
    ((7 11) t)
    (otherwise nil)))

(deftest test-instant-win-p ()
  (check
   (instant-win-p '(3 4))
   (instant-win-p '(5 6))
   (instant-win-p '(1 6))
   (not (instant-win-p '(2 2)))
   (not (instant-win-p '(5 5)))) )

(defun instant-loss-p (throw)
  "Is the throw an instant loss?"
  (assert (= (length throw) 2) (throw) "Bad throw: ~A" throw)
  (case (throw-value throw)
    ((2 3 12) t)
    (otherwise nil)))

(deftest test-instant-loss-p ()
  (check
   (instant-loss-p '(1 1))
   (instant-loss-p '(6 6))
   (instant-loss-p '(1 2))
   (not (instant-loss-p '(2 2)))
   (not (instant-loss-p '(5 5)))) )

;;;
;;;    This is Touretzky's name. I like it better.
;;;    
;; (defun sum-throw (throw)
(defun throw-value (throw)
  (+ (first throw) (second throw)))

(defun say-throw (throw)
  "Identify a given throw."
  (assert (= (length throw) 2) (throw) "Bad throw: ~A" throw)
  (let ((value (throw-value throw)))
    (case value
      (2 'snake-eyes)
      (12 'boxcars)
      (otherwise value))))

(deftest test-say-throw ()
  (check
   (eq (say-throw '(1 1)) 'snake-eyes)
   (eq (say-throw '(6 6)) 'boxcars)
   (eq (say-throw '(1 2)) 3)
   (eq (say-throw '(3 4)) 7)))

(defun print-throw (throw)
  (format t "Throw ~D and ~D -- " (first throw) (second throw)))

(defun craps ()
  (let ((throw (throw-dice)))
    (print-throw throw)
    (cond ((instant-win-p throw) (format t "~D -- You win!~%" (say-throw throw)))
          ((instant-loss-p throw) (format t "~D -- You lose!~%" (say-throw throw)))
          (t (format t "Your point is ~D.~%" (say-throw throw)))) ))

(defun try-for-point (point)
  (let* ((throw (throw-dice))
         (value (throw-value throw)))
    (print-throw throw)
    (cond ((= value point) (format t "~D -- You win!~%" (say-throw throw)))
          ((= value 7) (format t "~D -- You lose!~%" (say-throw throw)))
          (t (format t "~D -- Throw again.~%" (say-throw throw)))) ))
