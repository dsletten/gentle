;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               family.lisp
;;;;
;;;;   Started:            Sun Nov  1 02:36:18 2020
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

(defpackage :family (:use :common-lisp :test))

(in-package :family)

(defvar *family* '((colin nil nil)
                   (deirdre nil nil)
                   (arthur nil nil)
                   (kate nil nil)
                   (frank nil nil)
                   (linda nil nil)
                   (suzanne colin deirdre)
                   (bruce arthur kate)
                   (charles arthur kate)
                   (david arthur kate)
                   (ellen arthur kate)
                   (george frank linda)
                   (hillary frank linda)
                   (andre nil nil)
                   (tamara bruce suzanne)
                   (vincent bruce suzanne)
                   (wanda nil nil)
                   (ivan george ellen)
                   (julie george ellen)
                   (marie george ellen)
                   (nigel andre hillary)
                   (frederick nil tamara)
                   (zelda vincent wanda)
                   (joshua ivan wanda)
                   (quentin nil nil)
                   (robert quentin julie)
                   (olivia nigel marie)
                   (peter nigel marie)
                   (erica nil nil)
                   (yvette robert zelda)
                   (diane peter erica)))

(defun father (person)
  (let ((entry (assoc person *family*)))
    (if (null entry)
        nil
        (second entry))))

(deftest test-father ()
  (check
   (null (father nil))
   (eq (father 'yvette) 'robert)
   (eq (father 'diane) 'peter)
   (eq (father 'suzanne) 'colin)
   (null (father 'frederick))))

(defun mother (person)
  (let ((entry (assoc person *family*)))
    (if (null entry)
        nil
        (third entry))))

(deftest test-mother ()
  (check
   (null (mother nil))
   (eq (mother 'yvette) 'zelda)
   (eq (mother 'diane) 'erica)
   (eq (mother 'suzanne) 'deirdre)
   (null (mother 'colin))))

(defun parents (person)
  (let ((entry (assoc person *family*)))
    (if (null entry)
        nil
        (remove nil (rest entry)))) )

(deftest test-parents ()
  (check
   (null (parents nil))
   (equal (parents 'yvette) '(robert zelda))
   (equal (parents 'diane) '(peter erica))
   (equal (parents 'suzanne) '(colin deirdre))
   (equal (parents 'frederick) '(tamara))
   (null (parents 'colin))))

(defun children (person)
  (if (null person)
      '()
      (children-aux person *family*)))

(defun children-aux (person family)
  (cond ((null family) '())
        ((member person (rest (first family))) 
         (cons (first (first family)) (children-aux person (rest family))))
        (t (children-aux person (rest family)))) )

(defun children (person)
  (if (null person)
      '()
      (loop for entry in *family*
            when (member person (rest entry))
            collect (first entry))))

(defun children (person)
  (if (null person)
      '()
      (mapcar #'first (remove-if-not #'(lambda (entry) (member person (rest entry))) *family*))))

(defun children (person)
  (labels ((find-children (family children)
             (if (null family)
                 children
                 (destructuring-bind ((child . parents) . more) family
                   (if (member person parents)
                       (find-children more (cons child children))
                       (find-children more children)))) ))
  (if (null person)
      '()
      (find-children *family* '()))) )

(defun set-equal (a b)
  (and (subsetp a b) (subsetp b a)))

(deftest test-children ()
  (check
   (null (children nil))
   (null (children 'diane))
   (set-equal (children 'arthur) '(bruce charles david ellen))
   (set-equal (children 'ellen) '(ivan julie marie))))

(defun siblings (person)
  (remove person (union (children (father person)) (children (mother person)))) )

(deftest test-siblings ()
  (check
   (set-equal (siblings 'bruce) '(ELLEN DAVID CHARLES))
   (set-equal (siblings 'zelda) '(JOSHUA))
   (null (siblings nil))
   (null (siblings 'colin))))

;;;
;;;    Fails for empty L.
;;;    
(defun mapunion (f l)
  (reduce #'union (mapcar f l)))

;;;
;;;    Touretzky
;;;    
(defun mapunion (f l)
  (and l (reduce #'union (mapcar f l))))

;;;
;;;    Me
;;;    
(defun mapunion (f l)
  (reduce #'union (mapcar f l) :initial-value '()))

(deftest test-mapunion ()
  (check
   (set-equal (mapunion #'rest '((1 a b c) (2 e c j) (3 f a b c d))) '(a b c e j f d))))

(defun grandparents (person)
  (mapunion #'parents (parents person)))

(deftest test-grandparents ()
  (check
   (set-equal (grandparents 'julie) '(FRANK LINDA ARTHUR KATE))
   (set-equal (grandparents 'nigel) '(FRANK LINDA))
   (set-equal (grandparents 'yvette) '(QUENTIN JULIE VINCENT WANDA))))

;;;
;;;    Sweet!
;;;    
(defun cousins (person)
  (mapunion #'children (mapunion #'siblings (parents person))))

(deftest test-cousins ()
  (check
   (set-equal (cousins 'julie) '(NIGEL VINCENT TAMARA))
   (set-equal (cousins 'robert) '(PETER OLIVIA JOSHUA))
   (and (set-equal (cousins 'ivan) (cousins 'marie))
        (set-equal (cousins 'ivan) (cousins 'julie)))
   (set-equal (cousins 'olivia) (cousins 'peter))))

(defun descended-from (descendant ancestor)
  (cond ((null descendant) nil)
        ((eq ancestor (father descendant)) t)
        ((eq ancestor (mother descendant)) t)
        (t (or (descended-from (father descendant) ancestor)
               (descended-from (mother descendant) ancestor)))) )

;; (defun descended-from (descendant ancestor)
;;   (cond ((null ancestor) nil)
;;         ((eq ancestor (father descendant)) t)
;;         ((eq ancestor (mother descendant)) t)
;;         (t (or (descended-from descendant (father ancestor))
;;                (descended-from descendant (mother ancestor)))) ))

;;;
;;;    Bottom up is better than top down?
;;;    Parents of DESCENDANT are limited. Children of ANCESTOR can lead to dead ends...
;;;    

;;;
;;;    This is beautiful from 2002 genealogy.lisp!
;;;    
;; (defun descended-from (person ancestor)
;;   (and person ancestor
;;        (or (parentp ancestor person)
;; 	   (descended-from (father person) ancestor)
;; 	   (descended-from (mother person) ancestor))))

(defun descended-from (descendant ancestor)
  (cond ((null descendant) nil)
        ((or (eq ancestor (father descendant))
             (eq ancestor (mother descendant)) 
             (descended-from (father descendant) ancestor)
             (descended-from (mother descendant) ancestor)))) )

(deftest test-descended-from ()
  (check
   (descended-from 'yvette 'arthur)
   (descended-from 'yvette 'colin)
   (descended-from 'yvette 'deirdre)
   (descended-from 'yvette 'kate)
   (descended-from 'yvette 'frank)
   (descended-from 'yvette 'linda)
   (not (descended-from 'yvette 'andre))
   (descended-from 'yvette 'suzanne)
   (descended-from 'yvette 'bruce)
   (descended-from 'yvette 'ellen)
   (descended-from 'yvette 'george)
   (descended-from 'tamara 'arthur)
   (every #'(lambda (ancestor) (descended-from 'yvette ancestor)) (ancestors 'yvette))))

(defun ancestors (person)
  (cond ((null person) '())
        (t (union (parents person)
                  (union (ancestors (father person))
                         (ancestors (mother person)))) )))

(deftest test-ancestors ()
  (check
   (set-equal (ancestors 'marie) '(GEORGE ELLEN FRANK LINDA ARTHUR KATE))
   (set-equal (ancestors 'yvette) '(ROBERT ZELDA QUENTIN JULIE GEORGE ELLEN FRANK LINDA VINCENT WANDA BRUCE SUZANNE ARTHUR KATE COLIN DEIRDRE))
   (set-equal (ancestors 'diane) '(PETER ERICA NIGEL MARIE ANDRE HILLARY GEORGE ELLEN FRANK LINDA ARTHUR KATE))))

(defun generation-gap (descendant ancestor)
  (cond ((null descendant) nil)
        ((eq ancestor (father descendant)) 1)
        ((eq ancestor (mother descendant)) 1)
        (t (let ((gap (generation-gap (father descendant) ancestor)))
             (if (null gap)
                 (let ((gap (generation-gap (mother descendant) ancestor)))
                   (if (null gap)
                       gap
                       (1+ gap)))
                 (1+ gap)))) ))

(defun generation-gap (descendant ancestor)
  (labels ((find-gap (descendant gap)
             (cond ((null descendant) nil)
                   ((eq ancestor (father descendant)) (1+ gap))
                   ((eq ancestor (mother descendant)) (1+ gap))
                   (t (or (find-gap (father descendant) (1+ gap))
                          (find-gap (mother descendant) (1+ gap)))) )))
    (find-gap descendant 0)))

;;;
;;;    Touretzky has an interesting simplification.
;;;    (My modification.)
;;;
(defun generation-gap (descendant ancestor)
  (labels ((find-gap (descendant gap)
             (cond ((null descendant) nil)
                   ((eq descendant ancestor) gap)
                   (t (or (find-gap (father descendant) (1+ gap))
                          (find-gap (mother descendant) (1+ gap)))) )))
    (find-gap descendant 0)))

(deftest test-generation-gap ()
  (check
   (= (generation-gap 'suzanne 'colin) 1)
   (= (generation-gap 'frederick 'colin) 3)
   (null (generation-gap 'frederick 'linda))
   (= (generation-gap 'olivia 'frank) 3)))
