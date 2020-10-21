;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               blocks-world.lisp
;;;;
;;;;   Started:            Sat Oct 17 02:50:44 2020
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

(defpackage :blocks-world (:use :common-lisp :test))

(in-package :blocks-world)

(defvar *database* '((b1 shape brick)
                     (b1 color green)
                     (b1 size small)
                     (b1 supported-by b2)
                     (b1 supported-by b3)
                     (b1 material wood)
                     (b2 shape brick)
                     (b2 color red)
                     (b2 size small)
                     (b2 supports b1)
                     (b2 left-of b3)
                     (b2 material plastic)
                     (b3 shape brick)
                     (b3 color red)
                     (b3 size small)
                     (b3 supports b1)
                     (b3 right-of b2)
                     (b4 shape pyramid)
                     (b4 color blue)
                     (b4 size large)
                     (b4 supported-by b5)
                     (b5 shape cube)
                     (b5 color green)
                     (b5 size large)
                     (b5 supports b4)
                     (b6 shape brick)
                     (b6 color purple)
                     (b6 size large)))

(defun wildcardp (sym)
  (eq sym '?))

(defun subject (assertion)
  (first assertion))

(defun property (assertion)
  (second assertion))

(defun value (assertion)
  (third assertion))

(defun match-element (target pattern)
  (assert (not (wildcardp target)))
  (or (eq target pattern)
      (wildcardp pattern)))

(deftest test-match-element ()
  (check
   (match-element 'red 'red)
   (match-element 'red '?)
   (not (match-element 'red 'blue))))

(defun match-triple (assertion pattern)
  (match-tuple assertion pattern 3))

(defun match-tuple (assertion pattern count)
  (cond ((null assertion) (and (null pattern) (zerop count)))
        ((or (null pattern) (zerop count)) nil)
        ((match-element (first assertion) (first pattern)) (match-tuple (rest assertion) (rest pattern) (1- count)))
        (t nil)))

(deftest test-match-triple ()
  (check
   (match-triple '(b2 color red) '(b2 color red))
   (match-triple '(b2 color red) '(? color red))
   (match-triple '(b2 color red) '(b2 ? red))
   (match-triple '(b2 color red) '(b2 color ?))
   (not (match-triple '(b2 color) '(b2 color)))
   (not (match-triple '(b2 color red pung) '(b2 color red foo)))
   (not (match-triple '(b2 color red) '(b2 color red foo)))
   (not (match-triple '(b2 color red pung) '(b2 color red)))
   (not (match-triple '(b2 color red) '(b2 color)))
   (not (match-triple '(b2 color) '(b2 color red)))
   (not (match-triple '(b2 color red) '(b1 color red)))
   (not (match-triple '(b2 color red) '(b2 color green)))) )

(defun fetch (pattern)
  (remove-if-not #'(lambda (assertion) (match-triple assertion pattern)) *database*))

(deftest test-fetch ()
  (check
   (equal (fetch '(b2 color ?)) '((B2 COLOR RED)))
   (equal (fetch '(? supports b1)) '((B2 SUPPORTS B1) (B3 SUPPORTS B1)))
   (equal (fetch '(? supports ?)) '((B2 SUPPORTS B1) (B3 SUPPORTS B1) (B5 SUPPORTS B4)))))

(defun block-color-pattern (block)
  `(,block color ?))

(defun supporters (block)
  (mapcar #'subject (fetch `(? supports ,block))))

(defun supported-by-cube-p (block)
;  (some #'(lambda (supporter) (fetch `(,supporter shape cube))) (supporters block)))
  (find-if #'(lambda (supporter) (fetch `(,supporter shape cube))) (supporters block)))

(defun description (block)
  (labels ((desc1 ()
             (fetch `(,block ? ?)))
           (desc2 (assertions)
             (mapcar #'rest assertions)))
    (apply #'append (desc2 (desc1)))) )

(defun description (block)
  (let* ((assertions (fetch `(,block ? ?)))
         (properties (mapcar #'rest assertions)))
    (apply #'append properties)))



