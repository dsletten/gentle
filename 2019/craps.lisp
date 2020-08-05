;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               craps.lisp
;;;;
;;;;   Started:            Sun Nov 10 04:55:46 2019
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

(defpackage :craps (:use :common-lisp :test))

(in-package :craps)

(defconstant snake-eyes '(1 1))
(defconstant boxcars '(6 6))

(defun throw-die ()
  (1+ (random 6)))

(defun throw-dice ()
  (list (throw-die) (throw-die)))

(defun snake-eyes-p (throw)
  (equal throw snake-eyes))

(defun boxcars-p (throw)
  (equal throw boxcars))

;; (defun instant-win-p (throw)
;;   (let ((score (+ (first throw) (second throw))))
;;     (case score
;;       ((7 11) t)
;;       (otherwise nil))))

;; (defun instant-loss-p (throw)
;;   (case (apply #'+ throw)
;;     ((2 3 12) t)
;;     (otherwise nil)))

;; (defun say-throw (throw)
;;   (cond ((snake-eyes-p throw) 'snake-eyes)
;;         ((boxcars-p throw) 'boxcars)
;;         (t (+ (first throw) (second throw)))) )

;; (defun announce (throw)
;;   (cond ((instant-win-p throw) '(you win))
;;         ((instant-loss-p throw) '(you lose))
;;         (t `(your point is ,(say-throw throw)))) )

;; (defun craps ()
;;   (let ((throw (throw-dice)))
;;     `(throw ,(first throw) and ,(second throw) -- ,(say-throw throw) -- ,@(announce throw))))

;; (defun try-for-point (point)
;;   (let ((throw (throw-dice)))
;;     `(throw ,(first throw) and ,(second throw) -- ,(say-throw throw) -- ,@(let ((score (+ (first throw) (second throw))))
;;                                                                             (cond ((= score point) '(you win))
;;                                                                                   ((= score 7) '(you lose))
;;                                                                                   (t '(throw again)))) )))

(defun throw-sum (throw)
  (+ (first throw) (second throw)))

(defun instant-win-p (throw)
  (case (throw-sum throw)
    ((7 11) t)
    (otherwise nil)))

(defun instant-loss-p (throw)
  (case (throw-sum throw)
    ((2 3 12) t)
    (otherwise nil)))

(defun say-throw (throw)
  (cond ((snake-eyes-p throw) 'snake-eyes)
        ((boxcars-p throw) 'boxcars)
        (t (throw-sum throw))))

(defun outcome (throw)
  (cond ((instant-win-p throw) '(you win))
        ((instant-loss-p throw) '(you lose))
        (t `(your point is ,(say-throw throw)))) )

(defun announce (throw outcome)
  `(throw ,(first throw) and ,(second throw) -- ,(say-throw throw) -- ,@outcome))

(defun craps ()
  (let ((throw (throw-dice)))
    (announce throw (outcome throw))))

(defun try-for-point (point)
  (let* ((throw (throw-dice))
         (score (throw-sum throw)))
    (cond ((= score point) (announce throw '(you win)))
          ((= score 7) (announce throw '(you lose)))
          (t (announce throw '(throw again)))) ))

;;;
;;;    2020
;;;
(defun throw-die ()
  "Result of throwing one six-sided die."
  (1+ (random 6)))

(defun throw-dice ()
  "List of results of throwing two six-sided dice."
  (list (throw-die) (throw-die)))

(deftype die-throw ()
  '(cons (integer 1 6) (cons (integer 1 6) null)))

(defun snake-eyes-p (throw)
  "Is the THROW a pair of ones?"
  (equal throw '(1 1)))

(defun box-cars-p (throw)
  "Is the THROW a pair of sixes?"
  (equal throw '(6 6)))

(defun instant-win-p (throw)
  "Is the value of the THROW 7 or 11?"
  (check-type throw die-throw)
  (case (+ (first throw) (second throw))
    ((7 11) t)
    (otherwise nil)))

(defun instant-loss-p (throw)
  "Is the value of the THROW 2, 3, or 12?"
  (check-type throw die-throw)
  (case (+ (first throw) (second throw))
    ((2 3 12) t)
    (otherwise nil)))

(defun say-throw (throw)
  (check-type throw die-throw)
  (cond ((snake-eyes-p throw) 'snake-eyes)
        ((box-cars-p throw) 'box-cars)
        (t (+ (first throw) (second throw)))) )

(defun say (first second result status)
  (format t "~A~%" `(throw ,first and ,second -- ,result -- ,@status)))

(defun craps ()
  "Output analysis of THROW."
  (let ((throw (throw-dice)))
    (destructuring-bind (first second) throw
      (let ((result (say-throw throw)))
        (cond ((instant-win-p throw) (say first second result '(you win)))
              ((instant-loss-p throw) (say first second result '(you lose)))
              (t (say first second result `(your point is ,result))
                 (try-for-point result)))) )))

(defun try-for-point (point)
  (destructuring-bind (first second) (throw-dice)
    (let ((result (+ first second)))
      (cond ((= result point) (say first second result '(you win)))
            ((= result 7) (say first second result '(you lose)))
            (t (say first second result '(throw again))
               (try-for-point point)))) ))
