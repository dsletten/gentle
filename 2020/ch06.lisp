;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is the medium of choice for people who enjoy free style and flexibility.
;;;;   -- Gerald Jay Sussman
;;;;
;;;;   Name:               ch06.lisp
;;;;
;;;;   Started:            Mon Jul 20 01:58:21 2020
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
;(load "/home/slytobias/lisp/packages/test.lisp")
(load "/Users/dsletten/lisp/packages/collections.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :ch06 (:use :common-lisp :test))

(in-package :ch06)

;;;
;;;    6.26
;;;
(defconstant marker '-vs-)

(defun right-side (description)
  (rest (member marker description)))

(deftest test-right-side ()
  (check
   (equal (right-side '(large red shiny cube -vs- . #1=(small shiny red four-sided pyramid))) '#1#)))

(defun left-side (description)
  (ldiff description (member marker description)))

(defun left-side (description)
  (loop for elt in description
        until (eq elt marker)
        collect elt))

(defun left-side (description)
  (reverse (right-side (reverse description))))

(deftest test-left-side ()
  (check
   (equal (left-side '(large red shiny cube -vs- small shiny red four-sided pyramid)) '(large red shiny cube))))

(defun count-common (description)
  (let ((lhs (left-side description))
        (rhs (right-side description)))
    (assert (not (null lhs)))
    (assert (not (null rhs)))
    (length (intersection lhs rhs))))

(deftest test-count-common ()
  (= (count-common '(large red shiny cube -vs- small shiny red four-sided pyramid)) 2))

(defun compare (description)
  (let ((n (count-common description)))
    (if (= n 1)
        (cons n '(common feature))
        (cons n '(common features)))) )

(deftest test-compare ()
  (check 
   (equal (compare '(large red shiny cube -vs- small blue four-sided pyramid)) '(0 common features))
   ;; (equal (compare '(large red shiny cube -vs-)) '(0 common features))
   ;; (equal (compare '(-vs- small shiny red four-sided pyramid)) '(0 common features))
   (equal (compare '(large red shiny cube -vs- small shiny four-sided pyramid)) '(1 common feature))
   (equal (compare '(large red shiny cube -vs- small shiny red four-sided pyramid)) '(2 common features))
   (equal (compare '(small red metal cube -vs- red plastic small cube)) '(3 common features))))


;;;
;;;    Section 6.9
;;;    
(defvar *things* '((object1 large green shiny cube)
                   (object2 small red dull metal cube)
                   (object3 red small dull plastic cube)
                   (object4 small dull blue metal cube)
                   (object5 small shiny red four-sided pyramid)
                   (object6 large shiny green sphere)))

(defvar *same-things* '((object1 . (large green shiny cube))
                        (object2 . (small red dull metal cube))
                        (object3 . (red small dull plastic cube))
                        (object4 . (small dull blue metal cube))
                        (object5 . (small shiny red four-sided pyramid))
                        (object6 . (large shiny green sphere))))

;;;
;;;    Description consists of a list of properties/features.
;;;    
(defun description (obj)
  (rest (assoc obj *things*)))

(deftest test-description ()
  (check
   (equal (description 'object1) '(large green shiny cube))
   (null (description 'foo))))

(defun differences (obj1 obj2)
  "What properties are not shared by both objects?"
  (set-exclusive-or (description obj1)
                    (description obj2)))

(defun set-equal (s1 s2)
  (and (subsetp s1 s2)
       (subsetp s2 s1)))

(deftest test-differences ()
  (check
   (set-equal (differences 'object3 'object4) '(red blue plastic metal))
   (set-equal (differences 'object1 'object6) (differences 'object6 'object1))))

(defvar *quality-table* '((large . size)
                          (small . size)
                          (red . color)
                          (green . color)
                          (blue . color)
                          (shiny . luster)
                          (dull . luster)
                          (metal . material)
                          (plastic . material)
                          (cube . shape)
                          (sphere . shape)
                          (pyramid . shape)
                          (four-sided . shape)))

(defun quality (property)
  (cdr (assoc property *quality-table*)))

(deftest test-quality ()
  (check
   (equal (quality 'red) 'color)
   (equal (quality 'sphere) 'shape)))

(defun quality-difference (obj1 obj2)
  (quality (first (differences obj1 obj2))))

(defun contrast (obj1 obj2)
  (remove-duplicates (sublis *quality-table* (differences obj1 obj2))))

;;;
;;;    MAPCAR is in ch. 7!!
;;;    
(defun contrast (obj1 obj2)
  (remove-duplicates (mapcar #'quality (differences obj1 obj2))))

(deftest test-contrast ()
  (check
   (set-equal (contrast 'object3 'object4) '(color material))
   (null (contrast 'object5 'object5))))

;;;
;;;    6.30-33
;;;    
(defvar *books* '((before-the-frost henning-mankell)
                  (daemon daniel-suarez)
                  (cats-cradle kurt-vonnegut)
                  (prince-caspian c-s-lewis)
                  (paip peter-norvig)))

(defun who-wrote (book)
  (second (assoc book *books*)))

(defun wrote-what (author)
  (first (rassoc author *books* :key #'car)))

;;;
;;;    6.34
;;;
(defvar *atlas* '((pennsylvania pittsburgh)
                  (new-jersey newark)
                  (pennsylvania johnstown)
                  (ohio columbus)
                  (new-jersey princeton)
                  (new-jersey trenton)))

(defvar *atlas2* '((pennsylvania pittsburgh johnstown)
                   (ohio columbus)
                   (new-jersey newark princeton trenton)))

(defun find-cities (state)
  (rest (assoc state *atlas2*)))

(deftest test-find-cities ()
  (check
   (equal (find-cities 'pennsylvania) '(pittsburgh johnstown))
   (equal (find-cities 'new-jersey) '(newark princeton trenton))
   (null (find-cities 'pennsyltucky))))

;;;
;;;    6.35
;;;    
(defvar *nerd-states* '((sleeping eating)
                        (eating waiting-for-a-computer)
                        (waiting-for-a-computer programming)
                        (programming debugging)
                        (debugging sleeping)))

(defun nerdus (state)
  (second (assoc state *nerd-states*)))

(deftest test-nerdus ()
  (check
   (eq (nerdus 'programming) 'debugging)
   (eq (nerdus 'sleeping) 'eating)
   (null (nerdus 'playing-guitar))))

(defun sleepless-nerd (state)
  (let ((next-state (nerdus state)))
    (if (eq next-state 'sleeping)
        (sleepless-nerd next-state)
        next-state)))

(deftest test-sleepless-nerd ()
  (check
   (eq (sleepless-nerd 'eating) 'waiting-for-a-computer)
   (eq (sleepless-nerd 'debugging) 'eating)
   (eq (sleepless-nerd 'sleeping) 'eating))) ; ???

(defun nerd-on-caffeine (state)
  (nerdus (nerdus state)))

(deftest test-nerd-on-caffeine ()
  (check
   (eq (nerd-on-caffeine 'sleeping) 'waiting-for-a-computer)
   (eq (nerd-on-caffeine 'eating) 'programming)
   (eq (nerd-on-caffeine 'waiting-for-a-computer) 'debugging)
   (eq (nerd-on-caffeine 'programming) 'sleeping)
   (eq (nerd-on-caffeine 'debugging) 'eating)))

(defvar *nerd-cycle* (collections:make-circular-list '(sleeping eating waiting-for-a-computer programming debugging)))

;; (defun nerdus (state)
;;   (do ((terminal (first *nerd-cycle*))
;;        (states *nerd-cycle* (rest states)))
;;       ((eq state (first states)) (second states))
;;     (when (

