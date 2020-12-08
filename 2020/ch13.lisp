;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   If you give someone Fortran, he has Fortran. If you give someone Lisp, he has any language he pleases.
;;;;   -- Guy Steele
;;;;
;;;;   Name:               ch13.lisp
;;;;
;;;;   Started:            Sun Dec  6 03:38:35 2020
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

(defpackage :ch13 (:use :common-lisp :test) (:shadow :get))

(in-package :ch13)

;;;
;;;    Weird order of params??
;;;    
(defun addprop (sym value prop)
  (pushnew value (cl:get sym prop)))

(deftest test-addprop ()
  (check
   (let ((sym (make-symbol "FOO")))
     (addprop sym 'foo 'pung)
     (addprop sym 'baz 'bar)
     (addprop sym 'sneep 'bar)
     (equal (symbol-plist sym) '(bar (sneep baz) pung (foo)))) ))

(defun record-meeting (x y)
  (addprop x y 'has-met)
  (addprop y x 'has-met)
  t)

(defun has-met-p (x y)
  (and (member x (cl:get y 'has-met))
       (member y (cl:get x 'has-met))))

(deftest test-record-meeting ()
  (check
   (let ((bob (make-symbol "BOB"))
         (frank (make-symbol "FRANK"))
         (sue (make-symbol "SUE")))
     (record-meeting bob frank)
     (record-meeting frank sue)
     (and (has-met-p bob frank)
          (has-met-p frank bob)
          (has-met-p frank sue)
          (has-met-p sue frank)
          (not (has-met-p bob sue))
          (not (has-met-p sue bob)))) ))

;;;
;;;    13.1
;;;    
(defun subprop (sym value prop)
  (setf (cl:get sym prop) (remove value (cl:get sym prop))))

(deftest test-subprop ()
  (check
   (let ((sym (make-symbol "PUNG")))
     (addprop sym 'a 'foo)
     (addprop sym 'b 'foo)
     (addprop sym 'c 'foo)
     (addprop sym 'd 'foo)
     (addprop sym 'e 'foo)
     (subprop sym 'd 'foo)
     (equal (cl:get sym 'foo) '(e c b a)))) )

;;;
;;;    13.2
;;;    
(defun forget-meeting (x y)
  (subprop x y 'has-met)
  (subprop y x 'has-met)
  t)

(deftest test-forget-meeting ()
  (check
   (let ((bob (make-symbol "BOB"))
         (frank (make-symbol "FRANK"))
         (sue (make-symbol "SUE")))
     (record-meeting bob frank)
     (record-meeting frank sue)
     (forget-meeting bob frank)
     (and (not (has-met-p bob frank))
          (not (has-met-p frank bob))
          (has-met-p frank sue)
          (has-met-p sue frank)
          (not (has-met-p bob sue))
          (not (has-met-p sue bob))))
   (let ((bob (make-symbol "BOB"))
         (frank (make-symbol "FRANK"))
         (sue (make-symbol "SUE")))
     (record-meeting bob frank)
     (record-meeting frank sue)
     (forget-meeting frank bob)
     (and (not (has-met-p bob frank))
          (not (has-met-p frank bob))
          (has-met-p frank sue)
          (has-met-p sue frank)
          (not (has-met-p bob sue))
          (not (has-met-p sue bob)))) ))

;;;
;;;    13.3
;;;
(defun get (sym prop)
  (let ((entry (member prop (symbol-plist sym))))
    (if (null entry)
        nil
        (second entry))))

;;;
;;;    Touretzky (fixed...)
;;;    
(defun get (sym prop)
  (do ((plist (symbol-plist sym) (cddr plist)))
      ((null plist) nil)
    (when (eq prop (first plist))
      (return (second plist)))) )

(deftest test-get ()
  (check
   (let ((pung (make-symbol "PUNG")))
     (setf (cl:get pung 'foo) 'bar
           (cl:get pung 'baz) 'sneep)
     (and (eq (get pung 'foo) 'bar)
          (eq (get pung 'baz) 'sneep)))) )

;;;
;;;    13.4
;;;   
(let ((sentinel (make-symbol "NOPE"))) 
  (defun hasprop (sym prop)
    (let ((result (cl:get sym prop sentinel)))
      (not (eq result sentinel)))) )

;;;
;;;    Touretzky (fixed...)
;;;    
(defun hasprop (sym prop)
  (do ((plist (symbol-plist sym) (cddr plist)))
      ((null plist) nil)
    (when (eq prop (first plist))
      (return t))))

(deftest test-hasprop ()
  (check
   (let ((pung (make-symbol "PUNG")))
     (setf (cl:get pung 'foo) t
           (cl:get pung 'bar) nil
           (cl:get pung 'sneaky) (make-symbol "NOPE"))
     (and (hasprop pung 'foo)
          (hasprop pung 'bar)
          (hasprop pung 'sneaky)
          (not (hasprop pung 'baz)))) ))
