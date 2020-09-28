;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   What I like about Lisp is that you can feel the bits between your toes.
;;;;   -- Drew McDermott
;;;;
;;;;   Name:               ch07.lisp
;;;;
;;;;   Started:            Mon Sep 21 02:03:00 2020
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
(ql:quickload "clsql")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :ch07 (:use :common-lisp :test) (:shadow :set-difference :intersection :union))

(in-package :ch07)

;;;
;;;    7.1
;;;    
(setf (symbol-function 'add1) #'1+)

(deftest test-add1 ()
  (check
   (equal (mapcar #'add1 '(1 3 5 7 9)) '(2 4 6 8 10))))

;;;
;;;    7.2
;;;    
(defvar *daily-planet* '((olsen jimmy 123-76-4535 cub-reporter)
                         (kent clark 089-52-6787 reporter)
                         (lane lois 951-26-1438 reporter)
                         (white perry 355-16-7439 editor)))

(defun ssns (employees)
  (mapcar #'(lambda (employee)
              (destructuring-bind (first last ssn title) employee
                (declare (ignore first last title))
                ssn))
          employees))


(deftest test-ssns ()
  (check
   (equal (ssns *daily-planet*) '(123-76-4535 089-52-6787 951-26-1438 355-16-7439))))

(defclass employee ()
  ((first-name :reader first-name :initarg :first-name)
   (last-name :reader last-name :initarg :last-name)
   (ssn :reader ssn :initarg :ssn)
   (title :accessor title :initarg :title)))

(defmethod print-object ((e employee) stream)
  (print-unreadable-object (e stream :type t)
    (format stream "~A, ~A ~A [~A]" (last-name e) (first-name e) (title e) (ssn e))))

(defun make-employee (last first ssn title)
  (make-instance 'employee :first-name first :last-name last :ssn ssn :title title))

(defvar *employees* (mapcar #'(lambda (employee-data) (apply #'make-employee employee-data)) *daily-planet*))

(defun ssns (employees)
  (mapcar #'ssn employees))

(deftest test-ssns ()
  (check
   (equal (ssns *employees*) '(123-76-4535 089-52-6787 951-26-1438 355-16-7439))))

;(clsql-sys:connect '("localhost" "daily_planet" "cl-user" "pung") :database-type :mysql)
(clsql-sys:reconnect)

(defun db-ssns ()
  (mapcar #'first (clsql-sys:query "select ssn from employees")))

(deftest test-db-ssns ()
  (check
   (equal (db-ssns) '("123-76-4535" "089-52-6787" "951-26-1438" "355-16-7439"))))

;;;
;;;    7.6
;;;    
(defun booleanp (obj)
  (typep obj 'boolean))

(defun booleanp (obj)
  (eq obj (not (not obj))))

(deftest test-booleanp ()
  (check
   (booleanp t)
   (booleanp nil)
   (notany #'booleanp '(4 2.3 "asdf" (a b) #\k #(1 2 3)))) )

;;;
;;;    7.8
;;;    
(defun ballpark (l k &optional (epsilon 10))
  (let ((lower (- k epsilon))
        (upper (+ k epsilon)))
    (find-if #'(lambda (x) (<= lower x upper)) l)))

(deftest test-ballpark ()
  (check
   (= (ballpark '(2 4 6 8 10) 1) 2)
   (= (ballpark '(2 4 6 8 10) 19) 10)
   (= (ballpark '(2 4 6 8 10) 5 1) 4)
   (= (ballpark '(2 4 6 8 10) 5 1.1) 4)
   (= (ballpark '(2 4 6 8 10) 6) 2)
   (= (ballpark '(2 4 6 8 10) 6 0.5) 6)))

;;;
;;;    7.9
;;;    
(defun find-nested (l)
  (find-if #'(lambda (elt) (and (listp elt) (not (null elt)))) l))

;;;    D'oh! Touretzky:
(defun find-nested (l)
  (find-if #'consp l))

(deftest test-find-nested ()
  (check
   (equal (find-nested '(a b #1=(c d) (e f))) '#1#)
   (null (find-nested '(a b c)))
   (null (find-nested '(a b () c)))) )

;;;
;;;    7.11
;;;    
(defun handful (l)
  (remove-if #'(lambda (n) (or (<= n 1) (>= n 5))) l))

(defun handful (l)
  (remove-if-not #'(lambda (n) (< 1 n 5)) l))

(deftest test-handful ()
  (check
   (equal (handful '(1 2 3 4 5)) '(2 3 4))
   (equal (handful '(-4 -3 -2)) '())
   (equal (handful #1='(2 2 2)) #1#)
   (equal (handful '(8 9 10 11 12)) '())))

;;;
;;;    7.12
;;;    
(defun count-the (sentence)
  (length (remove-if-not #'(lambda (word) (eq word 'the)) sentence)))

(defun count-the (sentence)
  (count 'the sentence))

(deftest test-count-the ()
  (check
   (= (count-the '(a fool for your loving no more)) 0)
   (= (count-the '(now is the time for all good men to come to the aid of their country)) 2)
   (= (count-the '(the quick brown fox jumps over the lazy dog)) 2)))

;;;
;;;    7.13
;;;    
(defun find-twofers (l)
  (remove-if-not #'(lambda (elt) (and (listp elt) (= (length elt) 2))) l))

(deftest test-find-twofers ()
  (check
   (equal (find-twofers '(a b c)) '())
   (equal (find-twofers '((a) (b) (c))) '())
   (equal (find-twofers #1='((a 1) (b 2) (c 3))) #1#)
   (equal (find-twofers '((a 1) (b 2 :x) (c 3) (d 4) (e 5 fight like a brave))) '((a 1) (c 3) (d 4)))) )

;;;
;;;    7.14  See ch. 6 2019 recursive definitions
;;;    
(defun set-difference (a b)
  (remove-if #'(lambda (elt) (member elt b)) a))

(defun validate (actual expected)
  (and (cl:subsetp actual expected)
       (cl:subsetp expected actual)))

(deftest test-set-difference ()
  (check
   (validate (set-difference '(a b) '(a b)) '())
   (validate (set-difference '(a b c) '(d e f)) '(a b c))
   (validate (set-difference '(b c a) '(a d e)) '(b c))))

(defun union (a b)
  (append (remove-if #'(lambda (elt) (member elt b)) a) b))

(defun union (a b)
  (append (set-difference a b) b))

(deftest test-union ()
  (check
   (validate (union '(a b) '(a b)) '(a b))
   (validate (union '(a b c) '(d e f)) '(a b c d e f))
   (validate (union '(b c a) '(a d e)) '(a b c d e))))

;;;
;;;    These 2 (mine) are overkill. All of the elements of A are in A!
;;;    Simply need to remove the ones that aren't also in B.
;;;    
(defun intersection (a b)
  (append (remove-if-not #'(lambda (elt) (member elt b)) a)
          (remove-if-not #'(lambda (elt) (member elt a)) b)))

(defun intersection (a b)
  (labels ((filter (a b)
             (remove-if-not #'(lambda (elt) (member elt b)) a)))
    (append (filter a b) (filter b a))))

;;;   Touretzky
(defun intersection (a b)
 (remove-if-not #'(lambda (elt) (member elt b)) a))

(deftest test-intersection ()
  (check
   (validate (intersection '(a b) '(a b)) '(a b))
   (validate (intersection '(a b c) '(d e f)) '())
   (validate (intersection '(b c a) '(a d e)) '(a))))

