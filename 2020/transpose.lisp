;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               transpose.lisp
;;;;
;;;;   Started:            Sat Sep 26 20:36:19 2020
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;       Touretzky pg. 208
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

(defpackage :transpose (:use :common-lisp :test))

(in-package :transpose)

(defvar *note-table* '((C 1)
                       (C-SHARP 2)
                       (D 3)
                       (D-SHARP 4)
                       (E 5)
                       (F 6) 
                       (F-SHARP 7)
                       (G 8)
                       (G-SHARP 9)
                       (A 10) 
                       (A-SHARP 11)
                       (B 12)))

(defun numbers (notes)
  (mapcar #'(lambda (note) (second (assoc note *note-table*))) notes))

(deftest test-numbers ()
  (check
   (equal (numbers '(e d c d e e e)) '(5 3 1 3 5 5 5))
   (equal (numbers '(C C-SHARP D D-SHARP E F F-SHARP G G-SHARP A A-SHARP B)) '(1 2 3 4 5 6 7 8 9 10 11 12))))

(defun notes (numbers)
  (mapcar #'(lambda (number) (first (rassoc number *note-table* :key #'car))) numbers))

;;;
;;;    Touretzky says you can't use RASSOC (!)
;;;    
(defun notes (numbers)
  (mapcar #'(lambda (number) (first (find-if #'(lambda (entry) (= (second entry) number)) *note-table*))) numbers))

(deftest test-notes ()
  (check
   (equal (notes '(5 3 1 3 5 5 5)) '(e d c d e e e))
   (equal (notes '(1 2 3 4 5 6 7 8 9 10 11 12)) '(C C-SHARP D D-SHARP E F F-SHARP G G-SHARP A A-SHARP B))
   (equal (numbers (notes #1='(8 9 1 7 6))) #1#)
   (equal (notes (numbers #2='(a b c-sharp d d-sharp f))) #2#)))

(defun raise (n xs)
  (mapcar #'(lambda (x) (+ x n)) xs))

(deftest test-raise ()
  (check
   (equal (raise 5 '(5 3 1 3 5 5 5)) '(10 8 6 8 10 10 10))
   (equal (raise 0 #1='(5 3 1 3 5 5 5)) #1#)))

(defun normalize (xs)
  (mapcar #'(lambda (x) (1+ (mod (+ x 11) 12))) xs))

(deftest test-normalize ()
  (check
   (equal (normalize #1='(1 2 3 4 5 10 11 12)) #1#)
   (equal (normalize '(6 10 13)) '(6 10 1))
   (equal (normalize '(0 -1 -2 -3 -10 -11)) '(12 11 10 9 2 1))))

(defun transpose (n song)
  (notes (normalize (raise n (numbers song)))) )


