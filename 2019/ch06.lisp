;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               ch06.lisp
;;;;
;;;;   Started:            Wed Nov 20 23:32:19 2019
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
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :ch06 (:use :common-lisp :test) (:shadow :butlast))

(in-package :ch06)

;;;
;;;    6.6
;;;    
(defun last-element (l)
  (if (null l)
      nil
      (destructuring-bind (a . b) (last l)
        (if (null b)
            a
            b))))

(defun last-element-reverse (l)
  (first (reverse l)))

(defun last-element-nth (l)
  (if (null l)
      nil
      (nth (1- (length l)) l)))

(deftest test-last-element ()
  (check
   (eq (last-element '(a b c)) 'c)
   (eq (last-element '(a b c . d)) 'd)
   (eq (last-element '(a b #1=(c d))) '#1#)
   (eq (last-element '()) nil)))

(deftest test-last-element-reverse ()
  (check
   (eq (last-element-reverse '(a b c)) 'c)
   (eq (last-element-reverse '(a b #1=(c d))) '#1#)
   (eq (last-element-reverse '()) nil)))

(deftest test-last-element-nth ()
  (check
   (eq (last-element-nth '(a b c)) 'c)
   (eq (last-element-nth '(a b #1=(c d))) '#1#)
   (eq (last-element-nth '()) nil)))

;;;
;;;    6.7
;;;    
(defun next-to-last (l)
  (second (reverse l)))

(defun next-to-last (l)
  (labels ((next-to-last-aux (l1 l2 l3)
             (if (endp l3)
                 (first l1)
                 (next-to-last-aux (rest l1) (rest l2) (rest l3)))) )
    (next-to-last-aux l (rest l) (rest (rest l)))) )

(defun next-to-last-nth (l)
  (nth (- (length l) 2) l))

(deftest test-next-to-last ()
  (check
   (eq (next-to-last '(a b c d e)) 'd)
   (eq (next-to-last '(a b)) 'a)
   (eq (next-to-last '(a b #1=(c d) e)) '#1#)))

(deftest test-next-to-last-nth ()
  (check
   (eq (next-to-last-nth '(a b c d e)) 'd)
   (eq (next-to-last-nth '(a b)) 'a)
   (eq (next-to-last-nth '(a b #1=(c d) e)) '#1#)))

;;;
;;;    6.8
;;;    
(defun butlast (l)
  (cond ((endp (rest l)) '())
        (t (cons (first l) (butlast (rest l)))) ))

(defun butlast (l)
  (mapcar #'(lambda (a b)
              (declare (ignore b))
              a) l (rest l)))

(defun butlast (l)
  (loop for tail on l
        unless (endp (rest tail))
        collect (first tail)))

(defun butlast (l)
  (labels ((butlast-aux (l result)
             (if (endp (rest l))
                 (nreverse result)
                 (butlast-aux (rest l) (cons (first l) result)))) )
    (butlast-aux l '())))

(deftest test-butlast ()
  (check
   (equal (butlast '(roses are red)) '(roses are))
   (equal (butlast '(g a g a)) '(g a g))
   (equal (butlast '(a)) '())))

;;;
;;;    6.9
;;;    
(defun mystery (l)
  (first (last (reverse l))))

(deftest test-mystery ()
  (check
   (eq (mystery '#1=(a)) (first '#1#))
   (eq (mystery '#2=(a b)) (first '#2#))
   (eq (mystery '#3=(a b c)) (first '#3#))
   (eq (mystery '#4=((a b c))) (first '#4#))))

;;;
;;;    6.10
;;;    
(defun palindromep (l)
  (equal l (reverse l)))

(deftest test-palindromep ()
  (check
   (palindromep '(a b c d c b a))
   (not (palindromep '(a b c a b c)))) )

;;;
;;;    6.11
;;;    
(defun make-palindrome (l)
  (append l (reverse l)))

(deftest test-make-palindrome ()
  (check
   (equal (make-palindrome '(you and me)) '(you and me me and you))
   (palindromep (make-palindrome '(a b c)))))

;;;
;;;    6.15
;;;    
(let ((articles (list 'a 'an 'the)))
  (defun contains-article-p (sentence)
    (intersection articles sentence)))

(deftest test-contains-article-p ()
  (check
   (contains-article-p '(sometimes a lonely way))
   (contains-article-p '(and a new home for the trumpeter swan))
   (not (contains-article-p '(we can see it now)))) )

(defun contains-article-p (sentence)
  (or (member 'a sentence)
      (member 'an sentence)
      (member 'the sentence)))

(defun contains-article-p (sentence)
  (not (and (not (member 'a sentence))
            (not (member 'an sentence))
            (not (member 'the sentence)))) )

;;;
;;;    6.18
;;;    
(let ((vowels (list 'a 'e 'i 'o 'u)))
  (defun add-vowels (letters)
    (union vowels letters)))

(defun validate (expected actual)
  (and (subsetp expected actual)
       (subsetp actual expected)))

(deftest test-add-vowels ()
  (check
   (validate (add-vowels '(x a e z)) '(x a e z i o u))
   (validate (add-vowels '()) '(a e i o u))
   (validate (add-vowels '(a e i o u)) '(a e i o u))))


