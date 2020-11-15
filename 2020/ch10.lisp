;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               ch10.lisp
;;;;
;;;;   Started:            Mon Nov  9 02:56:13 2020
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

(defpackage :ch10 (:use :common-lisp :test))

(in-package :ch10)

;;;
;;;    10.3/10.4
;;;    
(let ((friends '())
      (close-friends '()))
  (defun meet (person)
    (cond ((member person friends)
           (pushnew person close-friends)
           (if (eq person (first friends))
               'we-just-met
               'we-know-each-other))
          (t (push person friends) 'pleased-to-meet-you)))
  (defun close-friend-count () 
    (length close-friends))
  (defun forget (person)
    (cond ((member person friends)
           (setf friends (remove person friends)
                 close-friends (remove person close-friends))
           'forgotten)
          (t `(do not know ,person))))
  (defun friends ()
    friends)
  (defun close-friends ()
    close-friends))

;;;
;;;    10.5
;;;    
(defun ugly (x y)
  (when (> x y)
    (setf temp y)
    (setf y x)
    (setf x temp))
  (setf avg (/ (+ x y) 2.0))
  (setf pct (* 100 (/ avg y)))
  (list 'average avg 'is pct 'percent 'of 'max y))

(defun beautiful (x y)
  (let* ((max (max x y))
         (avg (/ (+ x y) 2d0))
         (pct (* 100 (/ avg max))))
    `(average ,avg is ,pct percent of max ,max)))

(defun analyze-profit (price commission-rate)
  (let* ((commission (* price commission-rate))
         (result (cond ((> commission 100) 'rich)
                       (t 'poor))))
    (break "Value of RESULT is ~S" result)
    (format t "I predict that you will be: ~S" result)
    result))

;;;
;;;    10.9
;;;    
(defun chop (l)
 (unless (null l)
   (setf (cdr l) nil))
 l)

;;;
;;;    Touretzky's is better
;;;    
(defun chop (l)
  (if (consp l)
      (setf (cdr l) nil)
      l))

;;;
;;;    10.10
;;;
(defun ntack (l obj)
  (nconc l (list obj)))

