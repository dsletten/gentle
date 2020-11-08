;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               ch09.lisp
;;;;
;;;;   Started:            Thu Nov  5 00:48:43 2020
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
(load "/home/slytobias/lisp/packages/lang.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :ch09 (:use :common-lisp :lang :test))

(in-package :ch09)

;;;
;;;    9.1
;;;    
(defun pilots ()
  (format t "There are old pilots,~%")
  (format t "and there are bold pilots,~%")
  (format t "but there are no old bold pilots."))

;;;
;;;    !
;;;    
(deftest test-pilots ()
  (check
   (let* ((s (make-string-output-stream))
          (*standard-output*  s))
     (pilots)
     (string= (get-output-stream-string s)
"There are old pilots,
and there are bold pilots,
but there are no old bold pilots."))))

;;;
;;;    9.2
;;;    
(defun draw-line (n)
  (unless (zerop n)
    (format t "*")
    (draw-line (1- n))))

(defun touretzky-draw-line (n)
  (cond ((zerop n) (format t "~%"))
        (t (format t "*")
           (touretzky-draw-line (1- n)))) )

(defun capture-stdout (f)
  (let* ((s (make-string-output-stream))
         (*standard-output*  s))
     (funcall f)
     (get-output-stream-string s)))

(deftest test-draw-line ()
  (check
   (string= (capture-stdout #'(lambda () (draw-line 1))) "*")
   (string= (capture-stdout #'(lambda () (draw-line 5))) "*****")
   (string= (capture-stdout #'(lambda () (draw-line 10))) "**********")))

;;;
;;;    9.3
;;;    
(defun draw-box (cols rows)
  (unless (zerop rows)
    (draw-line cols)
    (format t "~%")
    (draw-box cols (1- rows))))

;;;
;;;    9.4
;;;    
(defun ninety-nine-bottles (n)
  (cond ((zerop n) (format t "Time for a beer run...~%"))
        (t (format t "~D bottle~:P of beer on the wall,~%" n)
           (format t "~D bottle~:P of beer!~%" n)
           (format t "Take one down,~%")
           (format t "Pass it around,~%")
           (format t "~D bottle~:P of beer on the wall.~2%" n)
           (ninety-nine-bottles (1- n)))) )

;;;
;;;    9.5
;;;    Way over-engineered!
;;;    
(defun print-board (elements)
  (labels ((print-cell (element)
             (format t " ~A " (if (null element) " " element)))
           (left (row elements)
             (print-cell (first elements))
             (format t "|")
             (middle row (rest elements)))
           (middle (row elements)
             (print-cell (first elements))
             (format t "|")
             (right row (rest elements)))
           (right (row elements)
             (print-cell (first elements))
             (format t "~%")
             (unless (zerop row)
               (format t "-----------~%")
               (left (1- row) (rest elements)))) )
    (left 2 elements)))

(defun print-board (elements)
  (labels ((do-print-board (row elements)
             (destructuring-bind (left middle right . more) elements
               (print-line left middle right)
               (unless (zerop row)
                 (format t "-----------~%")
                 (do-print-board (1- row) more))))
           (print-line (&rest cells)
             (apply #'format t " ~A | ~A | ~A~%" (substitute " " nil cells))))
    (do-print-board 2 elements)))

(defun print-board (elements)
  (labels ((print-line (&rest cells)
             (apply #'format t " ~A | ~A | ~A~%" (substitute " " nil cells))))
    (loop for (left middle right) on elements by #'cdddr
          for row from 2 downto 0
          do (print-line left middle right)
          unless (zerop row)
          do (format t "-----------~%"))))

;(print-board '(x o o nil x nil o nil x))

;;;
;;;    Interesting...
;;;    
(defun touretzky-print-board (b)
  (let ((b2 (sublis '((x . "X") (o . "O") (nil . " ")) b))) ; Only necessary for NIL...
    (format t "~&")
    (print-line b2)
    (format t "-----------~%")
    (print-line (nthcdr 3 b2))
    (format t "-----------~%")
    (print-line (nthcdr 6 b2))))

(defun print-line (line)
  (format t " ~A | ~A | ~A~%" (first line) (second line) (third line)))

;;;
;;;    9.6
;;;    
(defun gross-pay (hours pay-rate)
  (* hours pay-rate))

(defun compute-gross-pay ()
  (format t "Enter pay rate: ")
  (force-output)
  (let ((rate (read)))
    (format t "Enter hours worked: ")
    (force-output)
    (let ((hours (read)))
      (format t "Gross pay is: ~F~%" (gross-pay hours rate)))) )

(defun compute-gross-pay ()
  (let ((rate (get-num "Enter pay rate: " :test #'(lambda (x) (> x 0))))
        (hours (get-num "Enter hours worked: " :test #'(lambda (x) (> x 0)))) )
    (format t "Gross pay is: ~F~%" (gross-pay hours rate))))

;;;
;;;    9.7
;;;    
(defun cookie-monster ()
  (format t "Give me cookie!!!~%")
  (format t "Cookie? ")
  (force-output)
  (let ((obj (read)))
    (cond ((eq obj 'cookie) (format t "Thank you!...Munch munch munch...BURP~%"))
          (t (format t "No want ~A...~2%" obj)
             (cookie-monster)))) )

;;;
;;;    9.11
;;;    
(defun dot-print (obj)
  (cond ((atom obj) (format t "~S" obj))
        (t (format t "(")
           (dot-print (car obj))
           (format t " . ")
           (dot-print (cdr obj))
           (format t ")"))))

(defun read-dot-print (obj)
  (read-print #'dot-print obj))

(defun read-print (f obj)
  (let* ((s (make-string-output-stream))
         (*standard-output*  s))
     (funcall f obj)
     (read-from-string (get-output-stream-string s))))

(deftest test-dot-print ()
  (check
   (equal #1='() (read-dot-print #1#))
   (equal #2='a (read-dot-print #2#))
   (equal #3='(a) (read-dot-print #3#))
   (equal #4='(a b) (read-dot-print #4#))
   (equal #5='(a b c) (read-dot-print #5#))
   (equal #6='(a (b) c) (read-dot-print #6#))
   (equal #7='((((a)))) (read-dot-print #7#))
   (equal #8='(a . (b. c)) (read-dot-print #8#))))

;;;
;;;    9.15
;;;    
;; (defun hybrid-print (obj)
;;   (labels ((hybrid (obj)
;;              (cond ((atom obj) (format t "~S" obj))
;;                    (t (format t "(")
;;                       (hybrid (car obj))
;;                       (cond ((null (cdr obj)) (format t ")"))
;;                             ((atom (cdr obj)) (format t " . ") (hybrid (cdr obj)) (format t ")"))
;;                             (t (hybrid-cdr (cdr obj)))) )))
;;            (hybrid-cdr (obj)
;;              (format t " ~A" (car obj))
;;              (cond ((null (cdr obj)) (format t ")"))
;;                    ((atom (cdr obj)) (format t " . ") (hybrid (cdr obj)) (format t ")"))
;;                    (t (hybrid-cdr (cdr obj)))) ))
;;     (hybrid obj)))

(defun hybrid-print (obj)
  (labels ((hybrid (obj)
             (cond ((atom obj) (format t "~S" obj))
                   (t (format t "(")
                      (hybrid (car obj))
                      (hybrid-cdr (cdr obj))
                      (format t ")"))))
           (hybrid-cdr (obj)
             (cond ((null obj) nil)
                   ((atom obj) (format t " . ") (hybrid obj))
                   (t (format t " ")
                      (hybrid (car obj))
                      (hybrid-cdr (cdr obj)))) ))
    (hybrid obj)))

(defun read-hybrid-print (obj)
  (read-print #'hybrid-print obj))

(deftest test-hybrid-print ()
  (check
   (equal #1='(a) (read-hybrid-print #1#))
   (equal #2='(a . b) (read-hybrid-print #2#))
   (equal #3='(a b) (read-hybrid-print #3#))
   (equal #4='(a b . c) (read-hybrid-print #4#))
   (equal #5='(a b c . d) (read-hybrid-print #5#))
   (equal #6='((a) b c . d) (read-hybrid-print #6#))))
