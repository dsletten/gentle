;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Pascal is for building pyramids—imposing, breathtaking, static structures built by armies pushing heavy blocks into place. Lisp is for building organisms...
;;;;   -- Alan Perlis
;;;;
;;;;   Name:               ch08.lisp
;;;;
;;;;   Started:            Wed Oct 21 03:06:13 2020
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

(defpackage :ch08 (:use :common-lisp :test) (:shadow :member :assoc :nth :+ :1+ :1-))

(in-package :ch08)

;;;
;;;    8.5
;;;    
(defun add-up (ns)
  (if (endp ns) 
      0
      (cl:+ (first ns) (add-up (rest ns)))) )

(defun add-up (ns)
  (labels ((add (ns result)
             (if (endp ns)
                 result
                 (add (rest ns) (cl:+ (first ns) result)))) )
    (add ns 0)))

(deftest test-add-up ()
  (check
   (= (add-up '()) (reduce #'cl:+ '()))
   (= (add-up #1='(2 3 7)) (reduce #'cl:+ #1#))
   (= (add-up #2='(0 0 0 0)) (reduce #'cl:+ #2#))
   (= (add-up #3='(1 -1 2 -2 3 -3 4 -4)) (reduce #'cl:+ #3#))))

;;;
;;;    8.6
;;;    
(defun alloddp (l)
  (cond ((endp l) t)
        ((evenp (first l)) nil)
        (t (alloddp (rest l)))) )

(deftest test-alloddp ()
  (check
   (alloddp '())
   (alloddp '(1))
   (alloddp '(1 3 5 7 9))
   (not (alloddp '(2)))
   (not (alloddp '(1 2 3)))) )

;;;
;;;    8.7
;;;    
(defun member (item list &key key (test #'eql))
  (cond ((endp list) '())
        ((null key) (if (funcall test item (first list))
                        list
                        (member item (rest list) :test test)))
        (t (if (funcall test item (funcall key (first list)))
               list
               (member item (rest list) :key key :test test))) ))

(deftest test-member ()
  (check
   (equal (member 'd '(a b c d)) (cl:member 'd '(a b c d)))
   (equal (member 'e '(a b c d)) (cl:member 'e '(a b c d)))
   (equal (member 3 '(2 4 5 6 7) :test #'<) (cl:member 3 '(2 4 5 6 7) :test #'<))
   (equal (member 3 '((a 2) (b 4) (c 5) (d 6) (e 7)) :test #'< :key #'second) (cl:member 3 '((a 2) (b 4) (c 5) (d 6) (e 7)) :test #'< :key #'second))))

;;;
;;;    8.8
;;;    
(defun assoc (item list &key key (test #'eql))
  (cond ((endp list) nil)
        ((null key) (if (funcall test item (first (first list)))
                        (first list)
                        (assoc item (rest list) :test test)))
        (t (if (funcall test item (funcall key (first (first list))))
               (first list)
               (assoc item (rest list) :key key :test test))) ))

(deftest test-assoc ()
  (check
   (equal (assoc 'c '((a 1) (b 2) (c 3))) (cl:assoc 'c '((a 1) (b 2) (c 3))))
   (equal (assoc 'd '((a 1) (b 2) (c 3))) (cl:assoc 'd '((a 1) (b 2) (c 3))))
   (equal (assoc 'c '(("a" 1) ("b" 2) ("c" 3)) :test #'string=) (cl:assoc 'c '(("a" 1) ("b" 2) ("c" 3)) :test #'string=))
   (equal (assoc 5 '((0 a) (1 b) (2 c) (3 d) (4 e) (5 f)) :test #'(lambda (item elt) (= (mod item 3) (mod elt 3))))
          (cl:assoc 5 '((0 a) (1 b) (2 c) (3 d) (4 e) (5 f)) :test #'(lambda (item elt) (= (mod item 3) (mod elt 3)))) )
   (equal (assoc 'c '(((:f a) 0) ((:g b) 1) ((:k c) 2)) :key #'second) (cl:assoc 'c '(((:f a) 0) ((:g b) 1) ((:k c) 2)) :key #'second))))

;;;
;;;    8.9
;;;    
(defun nth (n list)
  (cond ((zerop n) (first list))
        (t (nth (cl:1- n) (rest list)))) )

(deftest test-nth ()
  (check
   (equal (nth 0 '(a b c)) (cl:nth 0 '(a b c)))
   (equal (nth 1 '(a b c)) (cl:nth 1 '(a b c)))
   (equal (nth 2 '(a b c)) (cl:nth 2 '(a b c)))
   (equal (nth 3 '(a b c)) (cl:nth 3 '(a b c)))
   (equal (nth 0 '()) (cl:nth 0 '()))) )

;;;
;;;    8.10
;;;    
(defun 1+ (x) (cl:+ x 1))
(defun 1- (x) (cl:- x 1))

(defun + (x y)
  (assert (typep y '(integer 0)))
  (cond ((zerop y) x)
        (t (1+ (+ x (1- y)))) ))

;;;
;;;    Touretzky's version: x + y => (x + 1) + (y -1)
;;;    
(defun + (x y)
  (assert (typep y '(integer 0)))
  (cond ((zerop y) x)
        (t (+ (1+ x) (1- y)))) )

(deftest test-+ ()
  (check
   (every #'(lambda (pair)
              (destructuring-bind (x y) pair
                (= (+ x y) (cl:+ x y))))
          (loop for i from -20 to 20
                nconc (loop for j from 0 to 20
                            collect (list i j)))) ))
          ;; (loop for i from -10 to 10
          ;;       nconc (loop for j from -10 to 10 ; D'oh!! Y can't be negative => infinite loop!!
          ;;                   collect (list i j)))) ))

;;;
;;;    Collatz's Conjecture
;;;
(defun collatz (n)
  (cond ((= n 1) t)
        ((evenp n) (collatz (/ n 2)))
        (t (collatz (+ (* 3 n) 1)))) )

;;;
;;;    8.21
;;;    
(defun add-nums (n)
  (cond ((zerop n) 0)
        (t (cl:+ n (add-nums (cl:1- n)))) ))

(defun sum (n)
  (/ (* n (cl:1+ n)) 2))

(deftest test-add-nums ()
  (check
   (= (add-nums 0) (sum 0))
   (= (add-nums 10) (sum 10))
   (= (add-nums 1000) (sum 1000))))

;;;
;;;    8.22
;;;    
(defun all-equal (l)
  (if (endp l) 
      t
      (destructuring-bind (elt . rest) l
        (if (endp rest)
            t
            (and (equal elt (first rest))
                 (all-equal rest)))) ))

(defun all-equal (l)
  (cond ((endp l) t)
        ((endp (rest l)) t)
        ((equal (first l) (second l)) (all-equal (rest l)))
        (t nil)))

(defun all-equal (l)
  (labels ((check (elt l)
             (cond ((endp l) t)
                   ((equal elt (first l)) (check (first l) (rest l)))
                   (t nil))))
    (if (endp l)
        t
        (check (first l) (rest l)))) )

(deftest test-all-equal ()
  (check
   (all-equal '())
   (all-equal '(i))
   (all-equal '(i i))
   (all-equal '(i i i i))
   (not (all-equal '(i i e i)))))

;;;
;;;    8.24
;;;    
(defun count-down (n)
  (if (zerop n)
      '()
      (cons n (count-down (cl:1- n)))) )

(defun loop-count-down (n)
  (loop for i from n downto 1 collect i))

(deftest test-count-down ()
  (check
   (equal (count-down 0) (loop-count-down 0))
   (equal (count-down 1) (loop-count-down 1))
   (equal (count-down 10) (loop-count-down 10))
   (equal (count-down 200) (loop-count-down 200))))

;;;
;;;    8.25
;;;    
(defun factorial (n)
  (reduce #'* (count-down n)))
;  (reduce #'* (count-down n) :initial-value 1)) ; Not actually needed...

(defun loop-factorial (n)
  (apply #'* (loop for i from 1 to n collect i)))

(deftest test-factorial ()
  (check
   (= (factorial 0) (loop-factorial 0))
   (= (factorial 1) (loop-factorial 1))
   (= (factorial 5) (loop-factorial 5))
   (= (factorial 20) (loop-factorial 20))))

;;;
;;;    8.26
;;;    
(defun count-down-0a (n)
  (if (zerop n)
      (list 0)
      (cons n (count-down-0a (cl:1- n)))) )

(defun loop-count-down-0 (n)
  (loop for i from n downto 0 collect i))

(defun count-down-0b (n)
  (if (minusp n)
      '()
      (cons n (count-down-0b (cl:1- n)))) )

(defun count-down-0c (n)
  (labels ((count-down (i j result)
             (cond ((zerop j) result)
                   (t (count-down (cl:1+ i) (cl:1- j) (cons i result)))) ))
    (count-down 1 n (list 0))))

(deftest test-count-down-0 ()
  (check
   (all-equal (list (count-down-0a #1=0) (loop-count-down-0 #1#) (count-down-0b #1#) (count-down-0c #1#)))
   (all-equal (list (count-down-0a #2=1) (loop-count-down-0 #2#) (count-down-0b #2#) (count-down-0c #2#)))
   (all-equal (list (count-down-0a #3=5) (loop-count-down-0 #3#) (count-down-0b #3#) (count-down-0c #3#)))
   (all-equal (list (count-down-0a #4=10) (loop-count-down-0 #4#) (count-down-0b #4#) (count-down-0c #4#)))) )

;;;
;;;    8.27
;;;    
(defun square-list (l)
  (if (endp l)
      '()
      (cons (expt (first l) 2) (square-list (rest l)))) )

(defun square-mapcar (l)
  (mapcar #'(lambda (x) (* x x)) l))

(defun square-loop (l)
  (loop for elt in l collect (* elt elt)))

(deftest test-square-list ()
  (check
   (all-equal (list (square-list #1='()) (square-mapcar #1#) (square-loop #1#)))
   (all-equal (list (square-list #2='(2)) (square-mapcar #2#) (square-loop #2#)))
   (all-equal (list (square-list #3='(3 4 5 6)) (square-mapcar #3#) (square-loop #3#)))) )

   
