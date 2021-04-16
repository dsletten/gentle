;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   What I like about Lisp is that you can feel the bits between your toes.
;;;;   -- Drew McDermott
;;;;
;;;;   Name:               robbie-clos.lisp
;;;;
;;;;   Started:            Fri Feb 19 19:02:39 2021
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
;;;;       Models the world in terms of locations, which are aware of connections to other locations.
;;;;
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :robbie-clos (:use :common-lisp :test))

(in-package :robbie-clos)

(defclass location ()
  ((name :reader name :initarg :name :type string)
   (connections :reader connections :initarg :connections))
  (:documentation "A location has a name and connections to other locations."))

(defgeneric destination (location direction)
  (:documentation "What is the destination after leaving LOCATION in the given DIRECTION?"))
(defmethod destination ((l location) direction)
  (with-slots (connections) l
    (let ((destination (assoc direction connections)))
      (if (null destination)
          nil
          (second destination)))) )

(defmethod print-object ((l location) stream)
  (print-unreadable-object (l stream :type t)
    (format stream "~A" (name l))
    (when (connections l)
      (format stream ":")
      (loop for ((direction destination) . more) on (connections l)
            do (format stream " ~A -> ~A" direction destination)
            when more do (format stream " |")))) )

(defmacro defworld (name locations)
  (let ((world (make-hash-table :test #'equal)))
    (dolist (location locations)
      (destructuring-bind (name -> . connections) location
        (declare (ignore ->))
        (setf (gethash name world) (make-instance 'location :name name :connections connections))))
    `(defparameter ,name ,world)))

(defworld *house* ((library -> (south back-stairs) (east upstairs-bedroom))
                   (back-stairs -> (north library) (south downstairs-bedroom))
                   (downstairs-bedroom -> (north back-stairs) (east dining-room))
                   (upstairs-bedroom -> (west library) (south front-stairs))
                   (front-stairs -> (north upstairs-bedroom) (south living-room))
                   (living-room -> (north front-stairs) (east kitchen) (south dining-room))
                   (dining-room -> (north living-room) (east pantry) (west downstairs-bedroom))
                   (kitchen -> (west living-room) (south pantry))
                   (pantry -> (north kitchen) (west dining-room))))

(defun move-robbie (location world moves)
  "Move Robbie from LOCATION according to MOVES along a path defined by WORLD."
  (cond ((endp moves) location)
        (t (destructuring-bind (move . more) moves
             (let ((options (connections (gethash location world))))
               (if (null options)
                   (warn "Where are you Robbie?!")
                   (let ((destination (destination (gethash location world) move)))
                     (if (null destination)
                         (warn "Robbie cannot move ~A from ~A." move location)
                         (move-robbie destination world more)))) )))) )

(deftest test-move-robbie ()
  (check
   (equal (move-robbie 'pantry *house* '(north west)) 'living-room)
   (equal (move-robbie 'pantry *house* '(north west north north west)) 'library)
   (equal (move-robbie 'pantry *house* '(north west north north west south south east north)) 'living-room)
   (equal (move-robbie 'upstairs-bedroom *house* '(south south south west north)) 'back-stairs)
   (equal (move-robbie 'upstairs-bedroom *house* '(west south)) 'back-stairs)))


