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
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :ch06 (:use :common-lisp :test) (:shadow :butlast :member :union :intersection :set-difference :subsetp                  :delete))

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
;;;
;;;    ABCL/CLISP/CMUCL/SBCL all return an error for (last 'a)
;;;    Clozure returns A, which is inconsistent with CLHS: list---a list, which might be a dotted list but must not be a circular list.
;;;    Although CLHS gives this possible implementation, which behaves like Clozure!
;;;
(defun last-clhs (list &optional (n 1))
  (check-type n (integer 0))
  (do ((l list (cdr l))
       (r list)
       (i 0 (+ i 1)))
      ((atom l) r)
    (if (>= i n) (pop r))))

(defun last-element (l)
  (cond ((null l) nil)
        ((atom l) (error "The value ~A is not of type list" l))
        ((atom (cdr l)) (or (cdr l) (car l)))
        (t (last-element (cdr l)))) )

(defun last-element (l)
  (cond ((null l) nil)
        ((atom l) (error "The value ~A is not of type list" l))
        (t (destructuring-bind (head . tail) l
             (if (atom tail)
                 (or tail head)
                 (last-element tail)))) ))

(defun last-element (l)
  (labels ((last-element-cons (cons)
             (destructuring-bind (head . tail) cons
               (cond ((null tail) head)
                     ((atom tail) tail)
                     (t (last-element-cons tail)))) ))
    (cond ((null l) nil)
          ((atom l) (error "The value ~A is not of type list" l))
          (t (last-element-cons l)))) )

(defun last-element-reverse (l)
  (first (reverse l)))

(defun last-element-nth (l)
  (if (null l)
      nil
      (nth (1- (length l)) l)))

(deftest test-last-element ()
  (check
   (eq (last-element '(a b c)) 'c)
   (eq (last-element '(a . b)) 'b)
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

;; ? (next-to-last '(a))
;; A
;; ? (next-to-last '())
;; NIL
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

;; Graham/ch03/ch03.lisp:(defun stable-set-difference (a b)    ??????
;; Little/Lisper/ch08/ch08.lisp:(defun set-difference (set1 set2)      Identical definitions!
;; Little/Lisper/2012/little.lisp:(defun set-difference (set1 set2)    Essentiallly identical
;; Shapiro/set.lisp:(defun set-difference (A B) Slightly different: expensive type checks, attempt at tail recursion...
;; Touretzky/ch08/tour8.lsp:(defun my-set-difference (a b)             Identical
;;~/lisp/programs/set.lisp       Some weird stuff...My solutions below are identical to Winston's!

(defun member (obj set &key (test #'eql))
  (cond ((null set) nil)
        ((funcall test obj (first set)) set)
        (t (member obj (rest set) :test test))))

(deftest test-member ()
  (check
   (member 'c '(a b c d e))
   (not (member '(pung foo) '(is this not (pung foo))))
   (member '(pung foo) '(is this not (pung foo)) :test #'equal)))

(defun union (a b)
  (cond ((null a) b)
        ((member (first a) b) (union (rest a) b))
        (t (cons (first a) (union (rest a) b)))) )

(deftest test-union ()
  (check
   (validate (union '(a b) '(a b)) '(a b))
   (validate (union '(a b c) '(d e f)) '(a b c d e f))
   (validate (union '(b c a) '(a d e)) '(a b c d e))))

;;;
;;;    Touretzky has a weird defintion of UNION in his Lisp implementation (pg. 173).
;;;    This is how it behaves...
;;;    
(defun touretzky-union (a b)
  (append a (set-difference b a)))

;; (touretzky-union '(finger hand arm) '(toe finger foot leg))
;; (FINGER HAND ARM LEG FOOT TOE)
;; (touretzky-union '(fred john mary) '(sue mary fred))
;; (FRED JOHN MARY SUE)

(defun intersection (a b)
  (cond ((null a) '())
        ((member (first a) b) (cons (first a) (intersection (rest a) b)))
        (t (intersection (rest a) b))))

(deftest test-intersection ()
  (check
   (validate (intersection '(a b) '(a b)) '(a b))
   (validate (intersection '(a b c) '(d e f)) '())
   (validate (intersection '(b c a) '(a d e)) '(a))))

(defun set-difference (a b)
  (cond ((null a) '())
        ((member (first a) b) (set-difference (rest a) b))
        (t (cons (first a) (set-difference (rest a) b)))) )

(deftest test-set-difference ()
  (check
   (validate (set-difference '(a b) '(a b)) '())
   (validate (set-difference '(a b c) '(d e f)) '(a b c))
   (validate (set-difference '(b c a) '(a d e)) '(b c))))

(defun subsetp (a b)
  "Is A a subset of B?"
  (cond ((null a) t)
        ((member (first a) b) (subsetp (rest a) b))
        (t nil)))

(deftest test-subsetp ()
  (check
   (subsetp '() '(a b c))
   (subsetp '(a) '(a b c))
   (subsetp '(c b a) '(a b c))
   (not (subsetp '(a d) '(a b c)))) )


;;;
;;;    6.15
;;;    
(let ((articles (list 'a 'an 'the)))
  (defun contains-article-p (sentence)
    (cl:intersection articles sentence)))

(defun contains-article-p (sentence)
  (not (null (cl:intersection sentence '(the a an)))) )

(deftest test-contains-article-p ()
  (check
   (contains-article-p '(sometimes a lonely way))
   (contains-article-p '(and a new home for the trumpeter swan))
   (not (contains-article-p '(we can see it now)))) )

(defun contains-article-p (sentence)
  (or (cl:member 'a sentence)
      (cl:member 'an sentence)
      (cl:member 'the sentence)))

(defun contains-article-p (sentence)
  (not (and (not (cl:member 'a sentence))
            (not (cl:member 'an sentence))
            (not (cl:member 'the sentence)))) )

(defun contains-article-p (sentence)
  (dolist (article '(a an the) nil)
    (when (cl:member article sentence)
      (return t))))

(defun contains-article-p (sentence)
  (labels ((containsp (articles)
             (cond ((endp articles) nil)
                   ((cl:member (first articles) sentence) t)
                   (t (containsp (rest articles)))) ))
    (containsp '(a an the))))

;;;
;;;    6.18
;;;    
(let ((vowels (list 'a 'e 'i 'o 'u)))
  (defun add-vowels (letters)
    (cl:union vowels letters)))

;;;
;;;    I.e., set equality...
;;;    
(defun validate (actual expected)
  (and (cl:subsetp actual expected)
       (cl:subsetp expected actual)))

(deftest test-add-vowels ()
  (check
   (validate (add-vowels '(x a e z)) '(x a e z i o u))
   (validate (add-vowels '()) '(a e i o u))
   (validate (add-vowels '(a e i o u)) '(a e i o u))))

;;;
;;;    6.21
;;;    
(defun subsetp (a b)
  (null (set-difference a b)))

;;;
;;;    6.25
;;;    
(defun proper-subset-p (a b)
  (and (subsetp a b)
       (not (subsetp b a))))

(deftest test-proper-subset-p ()
  (check
   (proper-subset-p '() '(a b c))
   (proper-subset-p '(a b) '(a b c))
   (not (proper-subset-p '(a b c) '(a b c)))
   (not (proper-subset-p '(a d) '(a b c)))) )

;;;
;;;    Section 6.7
;;;    
(defun titledp (name)
  (member (first name) '(mr ms miss mrs)))

(deftest test-titledp ()
  (check
   (titledp '(ms jane doe))
   (not (titledp '(jane doe)))) )

(let ((male-first-names '(john kim richard fred george))
      (female-first-names '(jane mary wanda barbara kim)))
  (defun malep (name)
    (and (member name male-first-names)
         (not (member name female-first-names))))
  (defun femalep (name)
    (and (member name female-first-names)
         (not (member name male-first-names))))
  (defun gender-ambiguous-names ()
    (intersection male-first-names female-first-names))
  (defun uniquely-male-names ()
    (set-difference male-first-names female-first-names))
  (defun uniquely-female-names ()
    (set-difference female-first-names male-first-names)))

(deftest test-gender ()
  (malep 'richard)
  (not (malep 'barbara))
  (femalep 'barbara)
  (not (malep 'kim))
  (not (femalep 'kim)))

(defun give-title (name)
  (cond ((titledp name) name)
        ((malep (first name)) (cons 'mr name))
        ((femalep (first name)) (cons 'ms name))
        (t (list* 'mr 'or 'ms name))))

(deftest test-give-title ()
  (check
   (equal (give-title #1='(miss jane adams)) #1#)
   (equal (give-title #2='(john q public)) (cons 'mr #2#))
   (equal (give-title #3='(barbara smith)) (cons 'ms #3#))
   (equal (give-title #4='(kim johnson)) (list* 'mr 'or 'ms #4#))))


