;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Pascal is for building pyramids—imposing, breathtaking, static structures built by armies pushing heavy blocks into place. Lisp is for building organisms...
;;;;   -- Alan Perlis
;;;;
;;;;   Name:               ch08.lisp
;;;;
;;;;   Started:            Wed Oct 21 03:06:13 2020
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

(defpackage :ch08 (:use :common-lisp :test) (:shadow :member :assoc :nth :+ :1+ :1-))

(in-package :ch08)

;;;
;;;    8.5
;;;    
(defun add-up (ns)
  (if (endp ns) 
      0
      (cl:+ (first ns) (add-up (rest ns)))) )

(defun add-up (ns)
  (labels ((add (ns result)
             (if (endp ns)
                 result
                 (add (rest ns) (cl:+ (first ns) result)))) )
    (add ns 0)))

(deftest test-add-up ()
  (check
   (= (add-up '()) (reduce #'cl:+ '()))
   (= (add-up #1='(2 3 7)) (reduce #'cl:+ #1#))
   (= (add-up #2='(0 0 0 0)) (reduce #'cl:+ #2#))
   (= (add-up #3='(1 -1 2 -2 3 -3 4 -4)) (reduce #'cl:+ #3#))))

;;;
;;;    8.6
;;;    
(defun alloddp (l)
  (cond ((endp l) t)
        ((evenp (first l)) nil)
        (t (alloddp (rest l)))) )

(deftest test-alloddp ()
  (check
   (alloddp '())
   (alloddp '(1))
   (alloddp '(1 3 5 7 9))
   (not (alloddp '(2)))
   (not (alloddp '(1 2 3)))) )

;;;
;;;    8.7
;;;    
(defun member (item list &key key (test #'eql))
  (cond ((endp list) '())
        ((null key) (if (funcall test item (first list))
                        list
                        (member item (rest list) :test test)))
        (t (if (funcall test item (funcall key (first list)))
               list
               (member item (rest list) :key key :test test))) ))

