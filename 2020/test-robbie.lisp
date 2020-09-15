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
