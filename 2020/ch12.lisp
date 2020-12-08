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
  (captain nil) ; Adding new slot redefines the structure. CMUCL invalidates old instances:    TYPEP on obsolete object (was class STARSHIP).
  (name nil :read-only t)
  (speed 0)
  (condition :green)
  (shields :down))

(defmethod print-object ((ship starship) stream)
  (print-unreadable-object (ship stream :type t)
    (format stream "~A - ~A [~:[☟~;☝~]] ~F ~S" (starship-name ship) 
            (or (starship-captain ship) "...") (shields-raised-p ship) (starship-speed ship) (starship-condition ship))))

(defstruct captain
  (name nil :read-only t)
  (age)
  (ship nil))

(defmethod print-object ((capt captain) stream)
  (print-unreadable-object (capt stream :type t)
    (format stream "~A" (captain-name capt))))

(defun take-command (captain ship)
  (setf (starship-captain ship) captain
        (captain-ship captain) ship))

(defun accelerate (starship speed)
  (unless (decommissionedp starship)
    (assert (not (minusp speed)) (speed) "Speed must be non-negative")
    (incf (starship-speed starship) speed)))

(defconstant c 299792458)
(defun warp-speed (starship warp)
  (unless (decommissionedp starship)
    (assert (< 0 warp 10) (warp) "Warp speed must be reasonable...")
    (setf (starship-speed starship) (* warp c))))

(defun stop (starship)
  (unless (decommissionedp starship)
    (setf (starship-speed starship) 0)))

(defun shields-raised-p (starship)
  (eq (starship-shields starship) :up))

(defgeneric raise-shields (starship)
  (:documentation "Raise shields. Returns T if shields were raised or NIL if already raised."))
(defmethod raise-shields :around ((ship starship))
  (unless (decommissionedp ship)
    (cond ((shields-raised-p ship) nil)
          (t (call-next-method) t))))
(defmethod raise-shields ((ship starship))
  (setf (starship-shields ship) :up)) ; Either use struct functions

(defgeneric lower-shields (starship)
  (:documentation "Lower shields. Returns T if shields were lowered or NIL if already lowered."))
(defmethod lower-shields :around ((ship starship))
  (unless (decommissionedp ship)
    (cond ((shields-raised-p ship) (call-next-method) t)
          (t nil))))
(defmethod lower-shields ((ship starship))
  (with-slots (shields) ship ; Or regular CLOS features...
    (setf shields :down)))

(defun on-alert-p (starship)
  (eq (starship-condition starship) :red))
(defun at-ease-p (starship)
  (eq (starship-condition starship) :green))

(defgeneric on-alert (starship)
  (:documentation "Raise alert condition to maximum and raise shields. Returns T if condition was raised or NIL if already on alert."))
(defmethod on-alert :around ((ship starship))
  (unless (decommissionedp ship)
    (cond ((on-alert-p ship) nil)
          (t (call-next-method) t))))
(defmethod on-alert ((ship starship))
  (setf (starship-condition ship) :red)
  (raise-shields ship))

(defgeneric at-ease (starship)
  (:documentation "Lower alert condition to minimum and lower shields. Returns T if condition was lowered or NIL if already at ease."))
(defmethod at-ease :around ((ship starship))
  (unless (decommissionedp ship)
    (cond ((at-ease-p ship) nil)
        (t (call-next-method) t))))
(defmethod at-ease ((ship starship))
  (setf (starship-condition ship) :green)
  (lower-shields ship))

(defgeneric raise-condition (starship)
  (:documentation "Raise alert condition. Returns T if condition was raised or NIL if already on alert."))
(defmethod raise-condition :around ((ship starship))
  (unless (decommissionedp ship)
    (cond ((on-alert-p ship) nil)
          (t (call-next-method) t))))
(defmethod raise-condition ((ship starship))
  (with-slots (condition) ship
    (setf condition (ecase condition
                      (:green :yellow)
                      (:yellow :orange)
                      (:orange :red)))) )

(defgeneric lower-condition (starship)
  (:documentation "Lower alert condition. Returns T if condition was lowered or NIL if already at ease."))
(defmethod lower-condition :around ((ship starship))
  (unless (decommissionedp ship)
    (cond ((at-ease-p ship) nil)
          (t (call-next-method) t))))
(defmethod lower-condition ((ship starship))
  (with-slots (condition) ship
    (setf condition (ecase condition
                      (:red :orange)
                      (:orange :yellow)
                      (:yellow :green)))) )

(defun decommission (starship)
  (with-slots (name speed condition shields) starship
    (setf name nil
          speed 0
          condition :green
          shields :down)))

(defun decommissionedp (starship)
  (null (starship-name starship)))




(defpackage :ch12-clos (:use :common-lisp :test) (:shadow :condition :speed))

(in-package :ch12-clos)

(deftype condition ()
  '(member :green :yellow :orange :red))

(defclass starship ()
  ((captain :accessor captain :initform nil :initarg :captain) ; Adding slot has minimal impact on existing instances!! Automatically shows up.
   (name :reader name :initarg :name)
   (speed :reader speed :initform 0)
   (condition :reader condition :initform :green)
   (shields :reader shields :initform :down)))

(defun make-starship (&key name captain)
  (make-instance 'starship :name name :captain captain))

(defmethod print-object ((ship starship) stream)
  (print-unreadable-object (ship stream :type t)
    (format stream "~A - ~A [~:[☟~;☝~]] ~F ~S" (name ship) (or (captain ship) "...") (shields-raised-p ship) (speed ship) (condition ship))))

(defclass captain ()
  ((name :reader name :initarg :name)
   (age :accessor age :initarg :age)
   (ship :accessor ship :initarg :ship :initform nil)))

(defun make-captain (&key name age ship)
  (make-instance 'captain :name name :age age :ship ship))

(defmethod print-object ((capt captain) stream)
  (print-unreadable-object (capt stream :type t)
    (format stream "~A" (name capt))))

(defun take-command (captain ship)
  (setf (captain ship) captain
        (ship captain) ship))

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

(defun decommission (starship)
  (with-slots (name speed condition shields) starship
    (setf name nil
          speed 0
          condition :green
          shields :down)))

(defun decommissionedp (starship)
  (null (name starship)))
