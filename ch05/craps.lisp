;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               craps.lisp
;;;
;;;   STARTED:            Sun May 26 03:40:50 2002
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

(defun throw-die ()
  "Generate a random number from 1 to 6 inclusive representing a throw of a 6-sided die."
  (1+ (random 6)))

(defun throw-dice ()
  "Generate a pair of random numbers representing a toss of two dice."
  (list (throw-die) (throw-die)))

(defun snake-eyes-p (throw)
  "Determine whether or not a pair of tossed dice is a pair of ones."
  (equal throw '(1 1)))

(defun box-cars-p (throw)
  "Determine whether or not a pair of tossed dice is a pair of sixes."
  (equal throw '(6 6)))

(defun instant-win-p (throw)
  "Determine if first throw wins."
  (or (= (throw-sum throw) 7)
      (= (throw-sum throw) 11)))

(defun instant-loss-p (throw)
  "Determine if first throw loses."
  (or (snake-eyes-p throw)
      (box-cars-p throw)
      (= (throw-sum throw) 3)))

(defun throw-sum (throw)
  "Add the values of two thrown dice."
  (+ (first throw) (second throw)))

(defun say-throw (throw)
  "Evaluate a given toss of the dice."
  (cond ((snake-eyes-p throw) 'snakeyes)
	((box-cars-p throw) 'boxcars)
	(t (throw-sum throw))))

(defun craps ()
  (let ((throw (throw-dice)))
    (cond ((instant-win-p throw)
	   (winning-throw throw))
	  ((instant-loss-p throw)
	   (losing-throw throw))
	  (t (announce-point throw)))) )

(defun winning-throw (throw)
  (announce throw "you win"))
  
(defun losing-throw (throw)
  (announce throw "you lose"))

(defun announce (throw outcome)
  (format t "~A ~A -- ~A" (announce-outcome throw)
	  (say-throw throw) outcome))
  
(defun announce-point (throw)
  (format t "~A your point is ~A" (announce-outcome throw)
	  (say-throw throw)))

(defun announce-outcome (throw)
  (format nil "throw ~A and ~A --" (first throw) (second throw)))
  
(defun try-for-point (point)
  (let ((throw (throw-dice)))
    (cond ((= (throw-sum throw) point)
	   (winning-throw throw))
	  ((= (throw-sum throw) 7)
	   (losing-throw throw))
	  (t (throw-again throw)))) )

(defun throw-again (throw)
  (announce throw "throw again"))

		