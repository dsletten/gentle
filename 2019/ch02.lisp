;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch02.lisp
;;;;
;;;;   Started:            Sat Mar 28 05:40:23 2020
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

(defpackage :ch02 (:use :common-lisp :test))

(in-package :ch02)

;;;
;;;    2.21
;;;    
(defun pack (a b c d)
  (list (list a b) (list c d)))

(deftest test-pack ()
  (check
   (equal (pack 1 2 3 4) '((1 2) (3 4)))
   (equal (pack '(a) 'b 'c '(d)) '(((a) b) (c (d)))) ))

;;;
;;;    2.22
;;;    
(defun duo-cons (x y l)
  (cons x (cons y l)))

(deftest test-duo-cons ()
  (check
   (equal (duo-cons . #1=('patrick 'seymour '(marvin))) (list* . #1#))
   (equal (duo-cons . #2=('(a) '(b) '((c)))) (list* . #2#))))

;;;
;;;    2.23
;;;    
(defun two-deeper (obj)
  (list (list obj)))

(defun two-deeper (obj)
  (cons (cons obj '()) '()))

(deftest test-two-deeper ()
  (check
   (equal (two-deeper #1='moo) `((,#1#)))
   (equal (two-deeper #2='(bow wow)) `((,#2#)))) )


