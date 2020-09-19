;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               test-robbie.lisp
;;;;
;;;;   Started:            Tue Sep 15 02:04:23 2020
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
(load "/home/slytobias/lisp/books/Touretzky/2020/robbie.lisp")

(in-package :robbie)

(deftest test-circuit ()
  (check
   (eq (move-robbie :library '(:east :south :south :east :south :west :west :north :north)) :LIBRARY)
   (eq (move-robbie :living-room '(:east :south :west :north)) :LIVING-ROOM)))

;; (move-robbie 'library *house-moves* '(east))
;; UPSTAIRS-BEDROOM
;; ROBBIE(37): (move-robbie 'library *house-moves* '(east south south east south west west north north))
;; LIBRARY
;; ROBBIE(38): (move-robbie 'living-room *house-moves* '(east south west north))
;; LIVING-ROOM
;; ROBBIE(39): (move-robbie 'living-room *house-moves* '(north east south west north))
;; WARNING: Robbie cannot move EAST from FRONT-STAIRS.
;; NIL
;; ROBBIE(40): (move-robbie 'dungeon *house-moves* '(north east south west north))
;; WARNING: Where are you Robbie?!
;; NIL


(deftest test-choices ()
  (check
   (equal (choices 'pantry) '((north kitchen) (west dining-room)))
   (equal (choices 'library) '((south back-stairs) (east upstairs-bedroom)))) )

(deftest test-look ()
  (check
   (eq (look 'north 'pantry) 'kitchen)
   (eq (look 'west 'pantry) 'dining-room)
   (null (look 'south 'pantry))))

(deftest test-how-many-choices ()
  (check
   (let ((*location* 'living-room)) (= (how-many-choices) 3))
   (let ((*location* 'pantry)) (= (how-many-choices) 2))))

