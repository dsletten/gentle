;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch06.lisp
;;;;
;;;;   Started:            Tue Aug 27 12:31:02 2013
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
  (let ((last (last l)))
    (if (null (rest last))
        (first last)
        (rest last))))

;; (deftest test-last-element ()
;;   (check
;;    (eq (last-element '(a b c d)) 'd)
;;    (eq (last-element '(a b c . d)) 'd)))

;;;
;;;    Arg to REVERSE must be _proper_ sequence.
;;;    
(defun last-element (l)
  (first (reverse l)))

;;;
;;;    Arg to LENGTH must be _proper_ sequence.
;;;    
(defun last-element (l)
  (if (null l)
      nil
      (nth (1- (length l)) l)))

(defun last-element (l)
  (cond ((endp l) nil)
        ((endp (rest l)) (first l))
        (t (last-element (rest l)))) )

(defun last-element (l)
  (labels ((find-last (elt l)
             (if (endp l)
                 elt
                 (find-last (first l) (rest l)))) )
    (if (endp l)
        nil
        (find-last (first l) (rest l)))) )

(deftest test-last-element ()
  (check
   (eq (last-element '(a b c d)) 'd)
   (eq (last-element '(a)) 'a)
   (eq (last-element '()) '())))

;;;
;;;    6.7
;;;
(defun next-to-last (l)
  (second (reverse l)))

;; (defun next-to-last (l)
;;   (nth (max (- (length l) 2) 0) l))

(defun next-to-last (l)
  (let ((index (- (length l) 2)))
    (if (minusp index)
        nil
        (nth index l))))

;;;
;;;    This is wrong.
;;;    
;; (defun next-to-last (l)
;;   (let ((index (- (length l) 2)))
;;     (if (plusp index)
;;         (nth index l)
;;         nil)))

(defun next-to-last (l)
  (first (last (cl:butlast l))))

(defun next-to-last (l)
  (labels ((find-next-to-last (first second l)
             (if (endp l)
                 first
                 (find-next-to-last second (first l) (rest l)))) )
    (cond ((null l) nil)
          ((null (rest l)) nil)
          (t (destructuring-bind (first second . rest) l
               (find-next-to-last first second rest)))) ))

(deftest test-next-to-last ()
  (check
   (equal (next-to-last '(a b c d)) 'c)
   (equal (next-to-last '(a b)) 'a)
   (equal (next-to-last '(a)) '())
   (equal (next-to-last '()) '())))

;;;
;;;    6.8
;;;
(defun butlast (l)
  (nreverse (rest (reverse l))))

(defun butlast (l)
  (cond ((endp l) '())
        ((endp (rest l)) '())
        (t (cons (first l) (butlast (rest l)))) ))

(defun butlast (l)
  (labels ((collect (elt l)
             (if (endp l)
                 '()
                 (cons elt (collect (first l) (rest l)))) ))
    (if (endp l)
        '()
        (collect (first l) (rest l)))) )

(defun butlast (l)
  (loop for elt in l
        for _ in (rest l)
        collect elt into result
        finally (return result)))

(defun butlast (l)
  (mapcar #'(lambda (a b) (declare (ignore b)) a) l (rest l)))

(deftest test-butlast ()
  (check
   (equal (butlast '(roses are red)) '(roses are))
   (equal (butlast '(g a g a)) '(g a g))))

;;;
;;;    6.9
;;;
(defun palindromep (l)
  (equal l (reverse l)))

(deftest test-palindromep ()
  (check
   (palindromep '(a b c d c b a))
   (palindromep "able was i ere i saw elba")
   (not (palindromep "race car"))))

;;;
;;;    6.10
;;;    Can only make even palindromes...
;;;
(defun make-palindrome (l)
  (append l (reverse l)))

(deftest test-make-palindrome ()
  (check
   (palindromep (make-palindrome '(you and me)))
   (palindromep (make-palindrome '(a b c d)))) )

;;;
;;;    6.15
;;;
(defun contains-the-p (sentence)
  (member 'the sentence))

(defun contains-article-p (sentence)
  (intersection '(a an the) sentence))

(defun contains-article-p (sentence)
  (or (member 'the sentence)
      (member 'a sentence)
      (member 'an sentence)))

(deftest test-contains-article-p ()
  (check
   (contains-article-p '(the quick brown fox jumps over the lazy dog))
   (contains-article-p '(the quick brown fox jumps over a lazy dog))
   (contains-article-p '(a quick brown fox jumps over an arthritic dog))))

;;;
;;;    6.18, 6.24, 6.25
;;;
(defun add-vowels (set)
  (union set '(a e i o u)))

(defun set-equal (a b)
  (and (subsetp a b)
       (subsetp b a)))

(defun proper-subset-p (a b)
  (and (subsetp a b)
       (not (subsetp b a))))

(deftest test-set-equal ()
  (check
   (set-equal '(a b c) '(c b a))
   (not (set-equal '(a b) '(a b c)))) )

(deftest test-proper-subset-p ()
  (check
   (not (proper-subset-p '(a b c) '(c b a)))
   (proper-subset-p '(a b) '(a b c))))

(deftest test-add-vowels ()
  (check
   (set-equal (add-vowels '(x a e z)) '(x a e z i o u))))