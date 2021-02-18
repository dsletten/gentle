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
;(load "/Users/dsletten/lisp/packages/lang.lisp")
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
(defvar *nerd-table* '((sleeping eating)
                       (eating waiting-for-a-computer)
                       (waiting-for-a-computer programming)
                       (programming debugging)
                       (debugging sleeping)))

(defun nerdus (state)
  (second (assoc state *nerd-table*)))

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

(defconstant nerd-states '(sleeping eating waiting-for-a-computer programming debugging))
(defvar *nerd-cycle* (collections:make-circular-list nerd-states))

;;;
;;;    Have to detect cycle for STATE that doesn't match!
;;;    
(defun nerdus (state)
  (do ((terminal (first *nerd-cycle*))
       (states (rest *nerd-cycle*) (rest states)))
      ((eq state (first states)) (second states))
    (when (eq terminal (first states))
      (return))))

(defmacro defchain (var vals)
  (let ((chain (mapcar #'(lambda (key val) `(,key ',val)) vals (append (rest vals) (list (first vals)))) ))
    `(case ,var ,@chain)))

(defun nerdus (state)
  (defchain state (sleeping eating waiting-for-a-computer programming debugging)))

;;;
;;;    6.36
;;;    
(defun swap-first-last (l)
  (labels ((swap (first reversed)
             (cons (first reversed) (reverse (cons first (rest reversed)))) ))
    (destructuring-bind (first . rest) l
      (cond ((null rest) l)
            (t (swap first (reverse rest)))) )))

(defun swap-first-last (l)
  (labels ((swap (first rest result)
             (cond ((null rest) (cons (first result) (nreverse (cons first (rest result)))) )
                   (t (swap first (rest rest) (cons (first rest) result)))) ))
    (destructuring-bind (first . rest) l
      (cond ((null rest) l)
            (t (swap first rest '()))) )))

(deftest test-swap-first-last ()
  (check
   (equal (swap-first-last '(a)) '(a))
   (equal (swap-first-last '(a b)) '(b a))
   (equal (swap-first-last '(you cant buy love)) '(love cant buy you))))

;;;
;;;    6.37
;;;    
(defun rotate-left (l)
  (cond ((null l) '())
        ((null (rest l)) l)
        (t (append (rest l) (list (first l))))) )

(defun rotate-left (l)
  (labels ((rotate (first l result)
             (cond ((null l) (nreverse (cons first result)))
                   (t (rotate first (rest l) (cons (first l) result)))) ))
    (cond ((null l) '())
          ((null (rest l)) l)
          (t (rotate (first l) (rest l) '()))) ))

(defun rotate-left (l)
  (labels ((rotate (first l)
             (cond ((null l) (list first))
                   (t (cons (first l) (rotate first (rest l)))) )))
    (cond ((null l) '())
          ((null (rest l)) l)
          (t (rotate (first l) (rest l)))) ))

(defun rotate-left (l)
  (cond ((null l) '())
        ((null (rest l)) l)
        (t (loop for cons on l
                 collect (if (null (rest cons)) (first l) (second cons)))) ))

(deftest test-rotate-left ()
  (check
   (equal (rotate-left '()) '())
   (equal (rotate-left '(a)) '(a))
   (equal (rotate-left '(a b)) '(b a))
   (equal (rotate-left (rotate-left (rotate-left #1='(a b c)))) #1#)
   (equal (rotate-left '(a b c d e)) '(b c d e a))))

(defun rotate-right (l)
  (cond ((null l) '())
        ((null (rest l)) l)
        (t (append (last l) (butlast l)))) )

;;;
;;;    Touretzky (Fails for empty list)
;;;    
;; (defun rotate-right (l)
;;   (let ((reversed (reverse l)))
;;     (cons (first reversed) (reverse (rest reversed)))) )

(defun rotate-right (l)
  (cond ((null l) '())
        ((null (rest l)) l)
        (t (loop for cons on l
                 until (null (rest cons))
                 collect (first cons) into elts
                 finally (return (cons (first cons) elts)))) ))

;;;
;;;    Interesting juggling!
;;;    
(defun rotate-right (l)
  (labels ((rotate (previous l result)
             (cond ((null l) (cons previous (nreverse result)))
                   (t (rotate (first l) (rest l) (cons previous result)))) ))
    (cond ((null l) '())
          ((null (rest l)) l)
          (t (rotate (first l) (rest l) '()))) ))

(deftest test-rotate-right ()
  (check
   (equal (rotate-right '()) '())
   (equal (rotate-right '(a)) '(a))
   (equal (rotate-right '(a b)) '(b a))
   (equal (rotate-right (rotate-right (rotate-right #1='(a b c)))) #1#)
   (equal (rotate-right '(a b c d e)) '(e a b c d))))

;;;
;;;    6.40
;;;    
;; ((a (a b c d))
;;  (b (b c d))
;;  (c (c d))
;;  (d (d)))
;;;
;;;    Touretzky
;;;    
;;;    His solution is odd. Evidently he intends for ASSOC to be used solely. This is
;;;    not how ASSOC is normally employed--the function returns an entry related to
;;;    the given key and then CDR or SECOND is used to extract the value from the entry.
;;;    
(defun touretzky-assoc (key)
  (assoc key '((a b c d)
               (b c d)
               (c d)
               (d))))

(defun my-assoc (key)
  (cdr (assoc key '((a . (a b c d))
                    (b . (b c d))
                    (c . (c d))
                    (d . (d)))) ))

(defun member->assoc (l)
  (loop for elt in l
        for cons on l
        collect (list elt cons)))

;(deftest test-member->assoc ()
  ;; (check
  ;;  (equal (member->assoc '(a b c d)) '((A (A B C D)) (B (B C D)) (C (C D)) (D (D)))) ))

(defun test-member->assoc ()
  (let* ((l '(a b c d))
         (table (member->assoc l)))
    (dolist (elt l)
      (assert (equal (member elt l) (second (assoc elt table)))) )))
