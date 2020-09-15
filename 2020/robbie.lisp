;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   LISP has been jokingly described as "the most intelligent way to misuse a computer".
;;;;   -- Edsger W. Dijkstra
;;;;
;;;;   Name:               robbie.lisp
;;;;
;;;;   Started:            Tue Sep 15 01:50:49 2020
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;       Touretzky ch. 6 (pg. 188) Robbie the Robot
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

(defpackage :robbie (:use :common-lisp :test))

(in-package :robbie)

(defun robbie-fsm (current direction)
  (labels ((library ()
             (case direction
               (:south :back-stairs)
               (:east :upstairs-bedroom)
               (otherwise (illegal-move))))
           (back-stairs ()
             (case direction
               (:north :library)
               (:south :downstairs-bedroom)
               (otherwise (illegal-move))))
           (downstairs-bedroom ()
             (case direction
               (:north :back-stairs)
               (:east :dining-room)
               (otherwise (illegal-move))))
           (upstairs-bedroom ()
             (case direction
               (:west :library)
               (:south :front-stairs)
               (otherwise (illegal-move))))
           (front-stairs ()
             (case direction
               (:north :upstairs-bedroom)
               (:south :living-room)
               (otherwise (illegal-move))))
           (living-room ()
             (case direction
               (:north :front-stairs)
               (:east :kitchen)
               (:south :dining-room)
               (otherwise (illegal-move))))
           (dining-room ()
             (case direction
               (:north :living-room)
               (:east :pantry)
               (:west :downstairs-bedroom)
               (otherwise (illegal-move))))
           (kitchen ()
             (case direction
               (:west :living-room)
               (:south :pantry)
               (otherwise (illegal-move))))
           (pantry ()
             (case direction
               (:north :kitcen)
               (:west :dining-room)
               (otherwise (illegal-move))))
           (illegal-move ()
             (warn "Robbie cannot move ~A from ~A." direction current)))
    (case current
      (:library (library))
      (:back-stairs (back-stairs))
      (:upstairs-bedroom (upstairs-bedroom))
      (:front-stairs (front-stairs))
      (:living-room (living-room))
      (:kitchen (kitchen))
      (:dining-room (dining-room))
      (:downstairs-bedroom (downstairs-bedroom))
      (:pantry (pantry))
      (otherwise (warn "Where are you Robbie?!")))) )

(defun move-robbie (start moves)
  (cond ((endp moves) start)
        (t (move-robbie (robbie-fsm start (first moves)) (rest moves)))) )
