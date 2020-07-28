;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               craps-clos.lisp
;;;;
;;;;   Started:            Tue Aug 20 16:12:03 2013
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
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :craps-clos (:use :common-lisp :test) (:shadow :first :second :throw))

(in-package :craps-clos)

(defclass throw ()
  ((first :reader first :initarg :first)
   (second :reader second :initarg :second)))

(defmethod print-object ((throw throw) stream)
  (format stream "Throw ~D and ~D" (first throw) (second throw)))

(defgeneric value (throw))
(defmethod value ((throw throw))
  (+ (first throw) (second throw)))

(defgeneric say-throw (throw))
(defmethod say-throw ((throw throw))
  "Identify a given throw."
  (let ((value (value throw)))
    (case value
      (2 'snake-eyes)
      (12 'box-cars)
      (otherwise value))))

(deftest test-say-throw ()
  (check
   (eq (say-throw (make-instance 'throw :first 1 :second 1)) 'snake-eyes)
   (eq (say-throw (make-instance 'throw :first 6 :second 6)) 'box-cars)
   (eq (say-throw (make-instance 'throw :first 1 :second 2)) 3)
   (eq (say-throw (make-instance 'throw :first 3 :second 4)) 7)))

(defun throw-die ()
  "Return a number from 1 to 6, inclusive, representing a throw of a 6-sided die."
  (1+ (random 6)))

(deftest test-throw-die ()
  (check
   (every #'(lambda (throw) (<= 1 throw 6)) (loop repeat 10000 collect (throw-die)))) )

(defun throw-dice ()
  "Return an instance of the throw class representing the result of throwing two 6-sided dice."
  (make-instance 'throw :first (throw-die) :second (throw-die)))

(defun snake-eyes-p (throw)
  "Is the throw a pair of ones?"
  (= (first throw) (second throw) 1))

(deftest test-snake-eyes-p ()
  (check
   (snake-eyes-p (make-instance 'throw :first 1 :second 1))
   (not (snake-eyes-p (make-instance 'throw :first 2 :second 3)))) )

(defun box-cars-p (throw)
  "Is the throw a pair of sixes?"
  (= (first throw) (second throw) 6))

(deftest test-box-cars-p ()
  (check
   (box-cars-p (make-instance 'throw :first 6 :second 6 ))
   (not (box-cars-p (make-instance 'throw :first 2 :second 3)))) )

(defun instant-win-p (throw)
  "Is the throw an instant win?"
  (case (value throw)
    ((7 11) t)
    (otherwise nil)))

(deftest test-instant-win-p ()
  (check
   (instant-win-p (make-instance 'throw :first 3 :second 4))
   (instant-win-p (make-instance 'throw :first 5 :second 6))
   (instant-win-p (make-instance 'throw :first 1 :second 6))
   (not (instant-win-p (make-instance 'throw :first 2 :second 2)))
   (not (instant-win-p (make-instance 'throw :first 5 :second 5)))) )

(defun instant-loss-p (throw)
  "Is the throw an instant loss?"
  (case (value throw)
    ((2 3 12) t)
    (otherwise nil)))

(deftest test-instant-loss-p ()
  (check
   (instant-loss-p (make-instance 'throw :first 1 :second 1))
   (instant-loss-p (make-instance 'throw :first 6 :second 6))
   (instant-loss-p (make-instance 'throw :first 1 :second 2))
   (not (instant-loss-p (make-instance 'throw :first 2 :second 2)))
   (not (instant-loss-p (make-instance 'throw :first 5 :second 5)))) )

(defun craps ()
  (let ((throw (throw-dice)))
    (cond ((instant-win-p throw) (announce-win throw))
          ((instant-loss-p throw) (announce-loss throw))
          (t (format t "~A -- Your point is ~D.~%" throw (say-throw throw)))) ))

(defun try-for-point (point)
  (let* ((throw (throw-dice))
         (value (throw-value throw)))
    (cond ((= value point) (announce-win throw))
          ((= value 7) (announce-loss throw))
          (t (format t "~A -- ~D -- Throw again.~%" throw (say-throw throw)))) ))

(defun announce-win (throw)
  (format t "~A -- ~D -- You win!~%" throw (say-throw throw)))

(defun announce-loss (throw)
  (format t "~A -- ~D -- You lose!~%" throw (say-throw throw)))  
