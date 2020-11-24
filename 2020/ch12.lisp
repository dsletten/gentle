;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is the medium of choice for people who enjoy free style and flexibility.
;;;;   -- Gerald Jay Sussman
;;;;
;;;;   Name:               ch12.lisp
;;;;
;;;;   Started:            Mon Nov 23 00:56:30 2020
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

(defpackage :ch12 (:use :common-lisp :test) (:shadow :condition :speed))

(in-package :ch12)

(deftype condition ()
  '(member :green :yellow :orange :red))

(defstruct starship
  (name nil)
  (speed 0)
  (condition :green)
  (shields :down))

(defpackage :ch12-clos (:use :common-lisp :test) (:shadow :condition :speed))

(in-package :ch12-clos)

(deftype condition ()
  '(member :green :yellow :orange :red))

(defclass starship ()
  ((name :reader name :initarg :name)
   (speed :reader speed :initform 0)
   (condition :reader condition :initform :green)
   (shields :reader shields :initform :down)))

(defun make-starship (name)
  (make-instance 'starship :name name))

(defmethod print-object ((ship starship) stream)
  (print-unreadable-object (ship stream :type t)
    (format stream "~A [~:[☟~;☝~]] ~F ~S" (name ship) (shields-raised-p ship) (speed ship) (condition ship))))

(defun accelerate (starship speed)
  (assert (not (minusp speed)) (speed) "Speed must be non-negative")
  (incf (slot-value starship 'speed) speed))

(defconstant c 299792458)
(defun warp-speed (starship warp)
  (assert (< 0 warp 10) (warp) "Warp speed must be reasonable...")
  (setf (slot-value starship 'speed) (* warp c)))

(defun stop (starship)
  (setf (slot-value starship 'speed) 0))

(defun shields-raised-p (starship)
  (eq (shields starship) :up))

(defgeneric raise-shields (starship)
  (:documentation "Raise shields. Returns T if shields were raised or NIL if already raised."))
(defmethod raise-shields :around ((ship starship))
  (cond ((shields-raised-p ship) nil)
        (t (call-next-method) t)))
(defmethod raise-shields ((ship starship))
  (with-slots (shields) ship
    (setf shields :up)))

(defgeneric lower-shields (starship)
  (:documentation "Lower shields. Returns T if shields were lowered or NIL if already lowered."))
(defmethod lower-shields :around ((ship starship))
  (cond ((shields-raised-p ship) (call-next-method) t)
        (t nil)))
(defmethod lower-shields ((ship starship))
  (with-slots (shields) ship
    (setf shields :down)))

(defun on-alert-p (starship)
  (eq (condition starship) :red))
(defun at-ease-p (starship)
  (eq (condition starship) :green))

(defgeneric on-alert (starship)
  (:documentation "Raise alert condition to maximum and raise shields. Returns T if condition was raised or NIL if already on alert."))
(defmethod on-alert :around ((ship starship))
  (cond ((on-alert-p ship) nil)
        (t (call-next-method) t)))
(defmethod on-alert ((ship starship))
  (with-slots (condition) ship
    (setf condition :red))
  (raise-shields ship))

(defgeneric at-ease (starship)
  (:documentation "Lower alert condition to minimum and lower shields. Returns T if condition was lowered or NIL if already at ease."))
(defmethod at-ease :around ((ship starship))
  (cond ((at-ease-p ship) nil)
        (t (call-next-method) t)))
(defmethod at-ease ((ship starship))
  (with-slots (condition) ship
    (setf condition :green))
  (lower-shields ship))

(defgeneric raise-condition (starship)
  (:documentation "Raise alert condition. Returns T if condition was raised or NIL if already on alert."))
(defmethod raise-condition :around ((ship starship))
  (cond ((on-alert-p ship) nil)
        (t (call-next-method) t)))
(defmethod raise-condition ((ship starship))
  (with-slots (condition) ship
    (setf condition (ecase condition
                      (:green :yellow)
                      (:yellow :orange)
                      (:orange :red)))) )

(defgeneric lower-condition (starship)
  (:documentation "Lower alert condition. Returns T if condition was lowered or NIL if already at ease."))
(defmethod lower-condition :around ((ship starship))
  (cond ((at-ease-p ship) nil)
        (t (call-next-method) t)))
(defmethod lower-condition ((ship starship))
  (with-slots (condition) ship
    (setf condition (ecase condition
                      (:red :orange)
                      (:orange :yellow)
                      (:yellow :green)))) )

