;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               family-clos.lisp
;;;;
;;;;   Started:            Wed Nov  4 00:47:20 2020
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

(defpackage :family-clos (:use :common-lisp :test))

(in-package :family-clos)

(defclass person ()
  ((name :reader name :initarg :name :type string)
   (father :reader father :initarg :father :type person)
   (mother :reader mother :initarg :mother :type person)
   (children :reader children :initform '() :type (list person))))

(defmethod name ((o null)) nil)
(defmethod father ((o null)) nil)
(defmethod mother ((o null)) nil)
(defmethod children ((o null)) nil)
  
(defmethod print-object ((p person) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~A F: ~A M: ~A" (name p) (name (father p)) (name (mother p)))
    (when (children p)
      (format stream " Children: ~A" (mapcar #'name (children p)))) ))

(defmacro defamily (name members)
  (let ((family (make-hash-table :test #'equal)))
    (dolist (member members)
      (destructuring-bind (person-name father-name mother-name) member
        (let ((father (find-person family father-name))
              (mother (find-person family mother-name)))
          (setf (gethash person-name family) (make-instance 'person :name person-name :father father :mother mother)))) )
    (set-children family)
    `(defparameter ,name ,family)))

(defun set-children (family)
  (loop for person being the hash-values in family
        for father = (father person)
        for mother = (mother person)
        unless (null father)
        do (add-child father person)
        unless (null mother)
        do (add-child mother person)))

(defun add-child (parent child)
  (with-slots (children) parent
    (pushnew child children)))

(defun find-person (family name)
  (gethash name family))

;;;
;;;    Must be ordered from ancestors -> descendants
;;;    
(defamily *family* (("Colin" nil nil)
                    ("Deirdre" nil nil)
                    ("Arthur" nil nil)
                    ("Kate" nil nil)
                    ("Frank" nil nil)
                    ("Linda" nil nil)
                    ("Suzanne" "Colin" "Deirdre")
                    ("Bruce" "Arthur" "Kate")
                    ("Charles" "Arthur" "Kate")
                    ("David" "Arthur" "Kate")
                    ("Ellen" "Arthur" "Kate")
                    ("George" "Frank" "Linda")
                    ("Hillary" "Frank" "Linda")
                    ("Andre" nil nil)
                    ("Tamara" "Bruce" "Suzanne")
                    ("Vincent" "Bruce" "Suzanne")
                    ("Wanda" nil nil)
                    ("Ivan" "George" "Ellen")
                    ("Julie" "George" "Ellen")
                    ("Marie" "George" "Ellen")
                    ("Nigel" "Andre" "Hillary")
                    ("Frederick" nil "Tamara")
                    ("Zelda" "Vincent" "Wanda")
                    ("Joshua" "Ivan" "Wanda")
                    ("Quentin" nil nil)
                    ("Robert" "Quentin" "Julie")
                    ("Olivia" "Nigel" "Marie")
                    ("Peter" "Nigel" "Marie")
                    ("Erica" nil nil)
                    ("Yvette" "Robert" "Zelda")
                    ("Diane" "Peter" "Erica")))

;;;
;;;    These are all the same as in family.lisp !!
;;;    
(defun parents (person)
  (remove nil (list (father person) (mother person))))

(defun siblings (person)
  (remove person (union (children (father person)) (children (mother person)))) )

(defun mapunion (f l)
  (reduce #'union (mapcar f l) :initial-value '()))

(defun grandparents (person)
  (mapunion #'parents (parents person)))

(defun cousins (person)
  (mapunion #'children (mapunion #'siblings (parents person))))

(defun descended-from (descendant ancestor)
  (cond ((null descendant) nil)
        ((eq ancestor (father descendant)) t)
        ((eq ancestor (mother descendant)) t)
        (t (or (descended-from (father descendant) ancestor)
               (descended-from (mother descendant) ancestor)))) )

(defun ancestors (person)
  (cond ((null person) '())
        (t (union (parents person)
                  (union (ancestors (father person))
                         (ancestors (mother person)))) )))

(defun generation-gap (descendant ancestor)
  (labels ((find-gap (descendant gap)
             (cond ((null descendant) nil)
                   ((eq descendant ancestor) gap)
                   (t (or (find-gap (father descendant) (1+ gap))
                          (find-gap (mother descendant) (1+ gap)))) )))
    (find-gap descendant 0)))

;; (parents (find-person *family* "Frederick"))
;; (parents (find-person *family* "Tamara"))

;; (siblings (find-person *family* "Tamara"))
;; (siblings (find-person *family* "Bruce"))
;; (siblings (find-person *family* "Zelda"))
;; (siblings (find-person *family* "Colin"))

;; (grandparents (find-person *family* "Frederick"))
;; (grandparents (find-person *family* "Julie"))
;; (grandparents (find-person *family* "Nigel"))
;; (grandparents (find-person *family* "Yvette"))

;; (cousins (find-person *family* "Julie"))
;; (cousins (find-person *family* "Robert"))
;; (cousins (find-person *family* "Ivan"))
;; (cousins (find-person *family* "Marie"))
;; (cousins (find-person *family* "Olivia"))
;; (cousins (find-person *family* "Peter"))

;; (apply #'descended-from (mapcar #'(lambda (name) (find-person *family* name)) '("Yvette" "Arthur")))
;; (apply #'descended-from (mapcar #'(lambda (name) (find-person *family* name)) '("Yvette" "Colin")))
;; (apply #'descended-from (mapcar #'(lambda (name) (find-person *family* name)) '("Yvette" "Deirdre")))
;; (apply #'descended-from (mapcar #'(lambda (name) (find-person *family* name)) '("Yvette" "Andre")))

;; (ancestors (find-person *family* "Marie"))
;; (ancestors (find-person *family* "Yvette"))
;; (ancestors (find-person *family* "Diane"))

;; (apply #'generation-gap (mapcar #'(lambda (name) (find-person *family* name)) '("Suzanne" "Colin")))
;; (apply #'generation-gap (mapcar #'(lambda (name) (find-person *family* name)) '("Frederick" "Colin")))
;; (apply #'generation-gap (mapcar #'(lambda (name) (find-person *family* name)) '("Frederick" "Linda")))
;; (apply #'generation-gap (mapcar #'(lambda (name) (find-person *family* name)) '("Olivia" "Frank")))
