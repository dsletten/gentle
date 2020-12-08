;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Pascal is for building pyramids—imposing, breathtaking, static structures built by armies pushing heavy blocks into place. Lisp is for building organisms...
;;;;   -- Alan Perlis
;;;;
;;;;   Name:               histogram.lisp
;;;;
;;;;   Started:            Sun Dec  6 21:43:54 2020
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

(defpackage :histogram (:use :common-lisp :test))

(in-package :histogram)

(defvar *hist-array* nil "The array of bins.")
(defvar *total-points* 0 "The number of trials for current histogram.")

(defun new-histogram (bins)
  (setf *hist-array* (make-array bins :initial-element 0)
        *total-points* 0))

(defun valid-index-p (i)
;  (<= 0 i (1- (length *hist-array*))))
  (typep i `(integer 0 (,(length *hist-array*)))) )

(defun record-value (i)
  (cond ((valid-index-p i)
         (incf (aref *hist-array* i))
         (incf *total-points*))
        (t (warn "Invalid bin: ~D" i))))

(defun print-hist-line (i)
  (cond ((valid-index-p i)
         (format t "~2D [~3D] ~A~%" i (aref *hist-array* i) (make-string (aref *hist-array* i) :initial-element #\*)))
        (t (warn "Invalid bin: ~D" i))))

(defun print-histogram ()
  (dotimes (i (length *hist-array*))
    (print-hist-line i))
  (format t "~7D total~%" *total-points*))

(defun run-trials (n)
  (let ((range (length *hist-array*)))
    (dotimes (i n)
      (record-value (random range)))) )

(defpackage :histogram-clos (:use :common-lisp :test))

(in-package :histogram-clos)

(defclass histogram ()
  ((bins :reader bins)
   (trials :reader trials :initform 0)))

(defun make-histogram (n)
  (let ((histogram (make-instance 'histogram)))
    (with-slots (bins) histogram
      (setf bins (make-array n :initial-element 0)))
    histogram))

(defmethod print-object ((h histogram) stream)
  (print-unreadable-object (h stream :type t)
    (format stream "[~D] trials: ~D" (length (bins h)) (trials h))))

(defgeneric record-trial (histogram i)
  (:documentation "Record a random trial in the appropriate bin."))
(defmethod record-trial :around ((h histogram) (i integer))
  (if (valid-index-p h i)
      (call-next-method)
      (warn "Invalid bin: ~D" i)))
(defmethod record-trial ((h histogram) (i integer))
  (with-slots (bins) h
    (incf (aref bins i))))
(defmethod record-trial :after ((h histogram) (i integer))
  (with-slots (trials) h
    (incf trials)))
           
(defun valid-index-p (histogram i)
  (typep i `(integer 0 (,(length (bins histogram)))) ))

(defgeneric print-histogram-bin (histogram i)
  (:documentation "Print a line representing the given bin in the HISTOGRAM."))
(defmethod print-histogram-bin :around ((h histogram) (i integer))
  (if (valid-index-p h i)
      (call-next-method)
      (warn "Invalid bin: ~D" i)))
(defmethod print-histogram-bin ((h histogram) (i integer))
  (let ((count (aref (bins h) i)))
    (format t "~2D [~3D] ~A~%" i count (make-string count :initial-element #\*))))

(defun print-histogram (histogram)
  (dotimes (i (length (bins histogram)))
    (print-histogram-bin histogram i))
  (format t "~7D total~%" (trials histogram)))

(defun run-trials (histogram n)
  (with-slots (bins) histogram
    (let ((range (length bins)))
      (dotimes (i n)
        (record-trial histogram (random range)))) ))

