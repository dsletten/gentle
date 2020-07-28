;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               ch09.lisp
;;;
;;;   STARTED:            Sun Jul  7 13:05:03 2002
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
;;;    Ex. 9.1
;;;
(defun flying ()
  (mapc #'(lambda (s)
	    (format t "~A~%" s))
	'("There are old pilots,"
	  "and there are bold pilot,"
	  "but there are no old bold pilots."))
  nil)

;;;
;;;    Ex. 9.2
;;;
(defun draw-line (n)
  (cond ((zerop n) (format t "~%"))
	(t (format t "*")
	   (draw-line (1- n)))) )

;;;
;;;    Ex. 9.3
;;;
(defun draw-box (n m)
  (cond ((zerop m) nil)
	(t (draw-line n)
	   (draw-box n (1- m)))) )

;;;
;;;    Ex. 9.4
;;;
(defun ninety-nine-bottles (bottles)
  (cond ((zerop bottles) (format t "...We're out of beer, dude.~%"))
	(t (format t "~D bottle~:P of beer on the wall, ~@
                      ~:*~D bottle~:P of beer!~@
                      Take one down,~@
                      Pass it around,~@
                      ~D bottle~:P of beer on the wall.~%~%" bottles
		      (1- bottles))
	   (ninety-nine-bottles (1- bottles)))) )

(defun ninety-nine-bottles (bottles)
  (cond ((zerop bottles) (format t "...We're out of beer, dude.~%"))
	(t (format t "~@(~R~) bottle~:P of beer on the wall, ~@
                      ~@(~:*~R~) bottle~:P of beer!~@
                      Take one down,~@
                      Pass it around,~@
                      ~@(~R~) bottle~:P of beer on the wall.~%~%" bottles
		      (1- bottles))
	   (ninety-nine-bottles (1- bottles)))) )

;;;
;;;    Ex. 9.5
;;;
(defun print-board (position-list)
  (labels ((left-cell (l)
	     (format t " ~A |" (or (car l) " "))
	     (middle-cell (cdr l)))
	   (middle-cell (l)
	     (format t " ~A |" (or (car l) " "))
	     (right-cell (cdr l)))
	   (right-cell (l)
	     (format t " ~A~%" (or (car l) " "))
	     (when (cdr l)
	       (format t "------------~%")
	       (left-cell (cdr l)))) )
    (left-cell position-list)))

;;;
;;;    Ex. 9.6
;;;
(defun wages ()
  (format t "Please enter pay rate: ")
  (let ((pay-rate (read)))
    (format t "Please enter hours worked: ")
    (let ((hours (read)))
      (format t "Gross pay for ~A hours at $~$: $~$~%" hours pay-rate
	      (* hours pay-rate)))) )

;;;
;;;    Ex. 9.7
;;;
(defun cookie-monster ()
  (labels ((get-cookie (cookie)
	     (cond ((eq cookie 'cookie)
		    (format t "Thank you!...Munch munch munch...BURP~%") t)
		   (t (format t "No want ~S...~%~%" cookie)))) )
  (format t "Give me cookie!!!~%")
  (format t "Cookie? ")
  (unless (get-cookie (read))
    (cookie-monster))))

;;;
;;;    Ex. 9.10
;;;    (Uses Graham's PROMPT and MAPA-B utilities) Ha!!!!
;;;    
(defun make-graph ()
  (labels ((plot-one-point (plotting-string y-val)
	     (format t "~V,0T~A~%" y-val plotting-string))
	   (plot-points (plotting-string y-vals)
	     (dolist (y y-vals)
	       (plot-one-point plotting-string y))))
    (let ((func (prompt "Function to graph? "))
	  (x0 (prompt "Starting x value? "))
	  (x1 (prompt "Ending x value? "))
	  (plotting-string (prompt "Plotting string? ")))
      (plot-points plotting-string (mapa-b func x0 x1)))) )

(defun square (x) (* x x))
(defun cube+ (x) (+ 30 (* x x x)))

;;;
;;;    Ex. 9.11
;;;
(defun dot-prin1 (obj)
  (cond ((atom obj) (prin1 obj))
	(t (format t "(")
	   (dot-prin1 (car obj))
	   (format t " . ")
	   (dot-prin1 (cdr obj))
	   (format t ")"))))

;;;
;;;    Ex. 9.15
;;;
(defun hybrid-prin1 (obj)
  (labels ((print-car (obj)
	     (cond ((atom obj) (format t "~S" obj))
		   (t (format t "(")
		      (print-car (car obj))
		      (print-cdr (cdr obj)))) )
	   (print-cdr (obj)
	     (cond ((null obj) (format t ")"))
		   ((atom obj) (format t " . ~S)" obj))
		   (t (format t " ")
		      (print-car (car obj))
		      (print-cdr (cdr obj)))) ))
    (print-car obj)))
		    
