;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   LISP has been jokingly described as "the most intelligent way to misuse a computer".
;;;;   -- Edsger W. Dijkstra
;;;;
;;;;   Name:               robbie-clos2.lisp
;;;;
;;;;   Started:            Mon Feb 22 14:10:33 2021
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
;;;;       Similar to family-clos.lisp in the way locations are created first and then wired together. <-- Uh...sort of. Location is just a symbol key in hash table.
;;;;       Models the world in terms of the connections between locations.
;;;;       Allows for the idea of one-way connections!
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :robbie-clos2 (:use :common-lisp :test))

(in-package :robbie-clos2)

(defclass connection ()
  ((source :reader source :initarg :source)
   (direction :reader direction :initarg :direction)
   (destination :reader destination :initarg :destination))
  (:documentation "Connects two locations."))

(defmethod print-object ((c connection) stream)
  (print-unreadable-object (c stream :type t)
    (format stream "~A: ~A -> ~A" (source c) (direction c) (destination c))))

(defmacro defworld (name links)
  (let ((world (make-hash-table :test #'equal)))
    (dolist (link links)
      (destructuring-bind (source -> . connections) link
        (declare (ignore ->))
        (setf (gethash source world)
              (mapcar #'(lambda (connection)
                          (destructuring-bind (direction destination) connection
                            (cons direction (make-instance 'connection :source source :direction direction :destination destination))))
                      connections))))
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
             (let ((options (gethash location world)))
               (if (null options)
                   (warn "Where are you Robbie?!")
                   (let ((connection (cdr (assoc move options))))
                     (if (null connection)
                         (warn "Robbie cannot move ~A from ~A." move location)
                         (move-robbie (destination connection) world more)))) )))) )

(deftest test-move-robbie ()
  (check
   (equal (move-robbie 'pantry *house* '(north west)) 'living-room)
   (equal (move-robbie 'pantry *house* '(north west north north west)) 'library)
   (equal (move-robbie 'pantry *house* '(north west north north west south south east north)) 'living-room)
   (equal (move-robbie 'upstairs-bedroom *house* '(south south south west north)) 'back-stairs)
   (equal (move-robbie 'upstairs-bedroom *house* '(west south)) 'back-stairs)))


