;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   What I like about Lisp is that you can feel the bits between your toes.
;;;;   -- Drew McDermott
;;;;
;;;;   Name:               plot.lisp
;;;;
;;;;   Started:            Sat Nov  7 02:31:59 2020
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;       Keyboard Exercise Touretzky Chapter 9
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

(defpackage :plot (:use :common-lisp :test))

(in-package :plot)

(defun space-over (n)
  (assert (not (minusp n)) () "N must be non-negative: ~D" n)
  (unless (zerop n)
    (write-char #\Space)
    (space-over (1- n))))

(deftest test-space-over (n)
  (format t ">>>")
  (space-over n)
  (format t "<<<~%"))

(defun plot-point (sym y)
  (space-over y)
  (format t "~A~%" sym))

(defun plot-points (sym points)
  (unless (endp points)
    (plot-point sym (first points))
    (plot-points sym (rest points))))

(defun generate (m n)
  (labels ((collect (i result)
             (cond ((> i n) (nreverse result))
                   (t (collect (1+ i) (cons i result)))) ))
    (collect m '())))

(defun generate (m n)
  (loop for i from m to n collect i))

;;;    See lang.lisp!!
;;;    #[m n]

(defun make-graph ()
  (format t "Function to graph? ")
  (force-output)
  (let ((f (read)))
    (format t "Starting x value? ")
    (force-output)
    (let ((start (read)))
      (format t "Ending x value? ")
      (force-output)
      (let ((end (read)))
        (format t "Plotting string? ")
        (force-output)
        (let ((sym (read)))
          (plot-points sym (mapcar f #[start end])))) )))

(defun square (x)
  (* x x))
