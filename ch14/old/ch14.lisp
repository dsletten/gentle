;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               ch14.lisp
;;;
;;;   STARTED:            Mon Dec 31 01:09:49 2001
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

;;;
;;;    Try both MACROEXPAND and MACROEXPAND-1 on this one.
;;;    
(defmacro set-nil (x)
  (list 'setf x nil) )

(defmacro set-nil (x)
  `(setf ,x nil) )

(defmacro simple-rotatef (x y)
  (list 'let
	(list (list 'a x)
	      (list 'b y))
	(list 'setf x 'b y 'a)) )

;;;
;;;    Oops: (simple-rotatef b a) =>
;;;    (LET ((A B) (B A))
;;;      (SETF B B A A))
;;;    
(defmacro simple-rotatef (x y)
  `(let ((a ,x)
	 (b ,y))
    (setf ,x b ,y a)) )

(defmacro set-mutual (x y)
  (list 'setf
	x (list 'quote y)
	y (list 'quote x)) )

(defmacro set-mutual (x y)
  `(setf ,x ',y ,y ',x) )
