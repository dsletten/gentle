;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   In Lisp there is always more than one way to solve a problem.
;;;;   -- David Touretzky
;;;;
;;;;   Name:               ch14.lisp
;;;;
;;;;   Started:            Thu Dec 17 02:38:55 2020
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

(defpackage :ch14 (:use :common-lisp :test) (:shadow :rotatef))

(in-package :ch14)

;;;
;;;    INCF must be a macro not a function.
;;;    
(defun faulty-incf (var)
  (setq var (+ var 1))) ; Simply modifies variable local to function. No effect once function returns.

(faulty-incf 4)
(let ((i 2))
  (faulty-incf i) i)

;;;
;;;    Sort of possible with special variables.
;;;    Parameter VAR is the name of some other variable.
;;;    
(defun dynamic-incf (var &optional (delta 1))
  (setf (symbol-value var) (+ (symbol-value var) delta)))

(let ((i 2)) 
  (declare (special i))
  (dynamic-incf 'i)
  i)

;;;
;;;    14.3
;;;    
(defmacro set-nil (var)
  `(setf ,var nil))

;;;
;;;    14.4
;;;    
(defmacro rotatef (a b)
  `(let ((temp ,a))
     (setf ,a ,b ,b temp)))

(defmacro rotatef (a b)
  (let ((a* (gensym))
        (b* (gensym)))
    `(let ((,a* ,a)
           (,b* ,b))
       (setf ,a ,b* ,b ,a*))))

;; (macroexpand-1 '(rotatef (car l) (cdar l)))
;; (LET ((#:G436 (CAR L)) (#:G437 (CDAR L)))
;;   (SETF (CAR L) #:G437
;;         (CDAR L) #:G436))
;; T
;; * (macroexpand-1 '(cl:rotatef (car l) (cdar l)))
;; (LET* ((#:L438 L) (#:LIST (CAR L)))
;;   (MULTIPLE-VALUE-BIND (#:NEW1)
;;       (CDR #:LIST)
;;     (MULTIPLE-VALUE-BIND (#:NEW)
;;         (CAR #:L438)
;;       (SB-KERNEL:%RPLACA #:L438 #:NEW1)
;;       (SB-KERNEL:%RPLACD #:LIST #:NEW)
;;       NIL)))
;; T
;; * (macroexpand-1 '(SETF (CAR L) #:G437
;;         (CDAR L) #:G436))
;; (PROGN (SETF (CAR L) #:G437) (SETF (CDAR L) #:G436))
;; T
;; * 

;;;
;;;    14.5
;;;    
(defmacro set-mutual (a b)
  `(setf ,a ',b ,b ',a))

;; (let (a b) (set-mutual a b) (list a b))
;; (B A)

(defmacro showvar (x)
  `(format t "The value of ~S is ~A~%" ',x ,x))

(defmacro set-zero (&rest vars)
  `(progn (setf ,@(loop for var in vars collect var collect 0))
     '(zeroed ,@vars)))

;;;
;;;    Touretzky's version
;;;
(defmacro set-zero (&rest variables)
  `(progn ,@(mapcar #'(lambda (var) (list 'setf var 0)) variables)
     '(zeroed ,@variables)))

(defmacro set-zero (&rest variables)
  `(progn ,@(mapcar #'(lambda (var) `(setf ,var 0)) variables)
     '(zeroed ,@variables)))

;;;
;;;    14.6
;;;    
(defmacro variable-chain (&rest vars)
  `(setf ,@(loop for var in vars 
                 for val in (rest vars)
                 collect var
                 collect `',val)))

;(macroexpand-1 '(variable-chain a b c d)) => (SETF A 'B B 'C C 'D)

;; (defmacro variable-chain (&rest vars)
;;   `(setf ,@(cons (first vars)
;;                  (loop for val in vars
;;                        for var in (rest vars)
;;                        collect `',val
;;                        collect var))))


