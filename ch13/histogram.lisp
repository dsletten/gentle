;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               histogram.lisp
;;;
;;;   STARTED:            Sat Jul 27 01:51:47 2002
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

(let ((hist-array)
      (total-points 0))

  (defun new-histogram (n)
    (setf total-points 0)
    (setf hist-array (make-array n :initial-element 0)))

  (defun record-value (x)
    (assert (typep x `(integer 0 (,(length hist-array)))) )
    (incf (aref hist-array x))
    (incf total-points))

  (defun make-histogram (n-trials)
    (do ((l (length hist-array))
	 (i 0 (1+ i)))
	((>= i n-trials))
      (record-value (random l))))

  (defun print-hist-line (i)
    (format t "~2D [~3D] " i (aref hist-array i))
    (dotimes (j (aref hist-array i))
      (format t "*"))
    (format t "~%"))

  (defun print-histogram ()
    (dotimes (i (length hist-array))
      (print-hist-line i))
    (format t "~4T~3D total~%" total-points)))
