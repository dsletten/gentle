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

(defpackage :ch08 (:use :common-lisp :test) (:shadow :member :assoc :nth :+ :1+ :1- :remove :intersection :set-difference :union :subst :reverse))

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
;;;    8.9 (See ex. 8.29 below)
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

(defun count-down (n)
  (labels ((count-it-down (i result)
             (cond ((> i n) result)
                   (t (count-it-down (cl:1+ i) (cons i result)))) ))
    (count-it-down 1 '())))

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

(defun count-down-0c (n)
  (labels ((count-down (i result)
             (cond ((> i n) result)
                   (t (count-down (cl:1+ i) (cons i result)))) ))
    (count-down 0 '())))

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

;;;
;;;    8.28 (See ex. 8.9 above)
;;;    The point of this one is to short circuit when the list is empty...
;;;    
(defun nth (n list)
  (cond ((null list) nil)
        ((zerop n) (first list))
        (t (nth (cl:1- n) (rest list)))) )

;;;
;;;    8.31
;;;    
(defun compare-lengths (l1 l2)
  (cond ((endp l1) (if (endp l2)
                       'same-length
                       'second-is-longer))
        ((endp l2) 'first-is-longer)
        (t (compare-lengths (rest l1) (rest l2)))) )

(deftest test-compare-lengths ()
  (check
   (eq (compare-lengths '() '()) 'same-length)
   (eq (compare-lengths '(a b c) '(1 2 3)) 'same-length)
   (eq (compare-lengths #1=(loop repeat 200 collect 'foo) #1#) 'same-length)
   (eq (compare-lengths '() '(1)) 'second-is-longer)
   (eq (compare-lengths '(a b c) '(1 2 3 4)) 'second-is-longer)
   (eq (compare-lengths (rest #2=(loop repeat 200 collect 'foo)) #2#) 'second-is-longer)
   (eq (compare-lengths '(a) '()) 'first-is-longer)
   (eq (compare-lengths '(a b c d) '(1 2 3)) 'first-is-longer)
   (eq (compare-lengths #3=(loop repeat 200 collect 'foo) (rest #3#)) 'first-is-longer)))

;;;
;;;    8.32
;;;    
(defun sum-numeric-elements (l)
  (cond ((null l) 0)
        ((numberp (first l)) (+ (first l) (sum-numeric-elements (rest l))))
        (t (sum-numeric-elements (rest l)))) )

(defun sum-numeric-elements-loop (l)
  (loop for elt in l when (numberp elt) sum elt))

;;;
;;;    Inspired by tour8.lsp
;;;
(defun sum-numeric-elements (l)
  (cond ((null l) 0)
        (t (typecase (first l)
             (number (+ (first l) (sum-numeric-elements (rest l))))
             (t (sum-numeric-elements (rest l)))) )))

(deftest test-sum-numeric-elements ()
  (check
   (= (sum-numeric-elements #1='(3 bears 3 bowls and 1 girl)) (sum-numeric-elements-loop #1#))
   (= (sum-numeric-elements #2='()) (sum-numeric-elements-loop #2#))
   (= (sum-numeric-elements #3='(a 1 b 2 c 3 4 5 6 x)) (sum-numeric-elements-loop #3#))))

;;;
;;;    8.33
;;;    
(defun remove (item list &key key (test #'eql))
  (cond ((endp list) '())
        ((null key) (if (funcall test item (first list))
                        (remove item (rest list) :test test)
                        (cons (first list) (remove item (rest list) :test test))))
        (t (if (funcall test item (funcall key (first list)))
               (remove item (rest list) :key key :test test)
               (cons (first list) (remove item (rest list) :key key :test test)))) ))

(deftest test-remove ()
  (check
   (equal (remove 'a '()) '())
   (equal (remove 'a #1='(b c d)) #1#)
   (equal (remove 'a '(a b c a d a b)) '(b c d b))
   (equal (remove 1d0 '(1 2 1d0 3 1)) '(1 2 3 1))
   (equal (remove 1d0 '(1 2 1d0 3 1) :test #'=) '(2 3))
   (equal (remove 1d0 '(1 2 1d0 3 1) :key #'(lambda (x) (coerce x 'double-float))) '(2 3))
   (equal (remove :z '((a 1) (b 2) (:z 3)) :key #'first) '((a 1) (b 2)))) )

;;;
;;;    8.34
;;;    
(defun intersection (a b)
  (cond ((null a) '())
        ((member (first a) b) (cons (first a) (intersection (rest a) b)))
        (t (intersection (rest a) b))))

(defun set-equal (a b)
  (and (subsetp a b) (subsetp b a)))

(deftest test-intersection ()
  (check
   (set-equal (intersection '(a b) '(a b)) '(a b))
   (set-equal (intersection '(a b c) '(d e f)) '())
   (set-equal (intersection '(b c a) '(a d e)) '(a))
   (set-equal (intersection '() #1='(a b c d)) (cl:intersection '() #1#))
   (set-equal (intersection #1# '()) (cl:intersection #1# '()))
   (set-equal (intersection #1# #2='(1 2 3 4)) (cl:intersection #1# #2#))
   (set-equal (intersection #1# #1#) (cl:intersection #1# #1#))
   (set-equal (intersection #1# #3='(d c b a)) (cl:intersection #1# #3#))
   (set-equal (intersection #1# #4='(b d)) (cl:intersection #1# #4#))))

;;;
;;;    8.35
;;;    
(defun set-difference (a b)
  (cond ((null a) '())
        ((member (first a) b) (set-difference (rest a) b))
        (t (cons (first a) (set-difference (rest a) b)))) )

(deftest test-set-difference ()
  (check
   (set-equal (set-difference '(a b) '(a b)) '())
   (set-equal (set-difference '(a b c) '(d e f)) '(a b c))
   (set-equal (set-difference '(b c a) '(a d e)) '(b c))
   (set-equal (set-difference '() #1='(a b c d)) (cl:set-difference '() #1#))
   (set-equal (set-difference #1# '()) (cl:set-difference #1# '()))
   (set-equal (set-difference #1# #2='(1 2 3 4)) (cl:set-difference #1# #2#))
   (set-equal (set-difference #1# #1#) (cl:set-difference #1# #1#))
   (set-equal (set-difference #1# #3='(d c b a)) (cl:set-difference #1# #3#))
   (set-equal (set-difference #1# #4='(b d)) (cl:set-difference #1# #4#))))

;;;
;;;    8.36
;;;    
(defun count-odd (l)
  (cond ((endp l) 0)
        ((oddp (first l)) (cl:1+ (count-odd (rest l))))
        (t (count-odd (rest l)))) )

(defun count-odd (l)
  (cond ((endp l) 0)
        (t (+ (if (oddp (first l)) 1 0) (count-odd (rest l)))) ))

(deftest test-count-odd ()
  (check
   (= (count-odd '()) 0)
   (= (count-odd '(2 4 6 8)) 0)
   (= (count-odd '(4 5 6 7 8)) 2)))

;;;
;;;    8.37
;;;    Binary tree with k terminal nodes has k-1 nonterminal nodes.
;;;
;;;    Proof:
;;;    1. A tree consisting solely of 1 terminal node has 1-1 = 0 nonterminal nodes.
;;;    2. Assume that a tree has n terminal nodes and n-1 nonterminal nodes. Extending
;;;       the tree with 2 terminal nodes as children of each original terminal node results
;;;       in 2n new terminal nodes. But this new tree has n-1 nonterminal nodes plus the new
;;;       n former terminal nodes: n-1 + n = 2n - 1.
;;;    
(defun combine (x y) (cl:+ x y))

(defun fibonacci-touretzky (n)
  (case n
    ((0 1) 1)
    (otherwise (combine (fibonacci-touretzky (- n 1))
                        (fibonacci-touretzky (- n 2)))) ))

(defun fibonacci (n)
  (case n
    (0 0)
    (1 1)
    (otherwise (combine (fibonacci (- n 1))
                        (fibonacci (- n 2)))) ))

;;;
;;;    8.39
;;;    
(defun count-atoms (tree)
  (cond ((atom tree) 1)
        (t (cl:+ (count-atoms (car tree))
                 (count-atoms (cdr tree)))) ))

(deftest test-count-atoms ()
  (check
   (= (count-atoms '()) 1)
   (= (count-atoms '(a b c)) 4)
   (= (count-atoms '(a . b)) 2)
   (= (count-atoms '(a (b) c)) 5)
   (= (count-atoms '((a b) c)) 5)))

;;;
;;;    8.40
;;;    
(defun count-cons (tree)
  (cond ((atom tree) 0)
        (t (cl:+ 1
                 (count-cons (car tree))
                 (count-cons (cdr tree)))) ))

(deftest test-count-cons ()
  (check
   (= (count-cons '()) 0)
   (= (count-cons 'a) 0)
   (= (count-cons '(a)) 1)
   (= (count-cons '(a b)) 2)
   (= (count-cons '(a b c)) 3)
   (= (count-cons '(a . b)) 1)
   (= (count-cons '(a b . c)) 2)
   (= (count-cons '((a . b) . (c . d))) 3)))

;;;
;;;    8.41
;;;    
(defun sum-tree (tree)
  (cond ((numberp tree) tree)
        ((atom tree) 0)
        (t (+ (sum-tree (car tree))
              (sum-tree (cdr tree)))) ))

(deftest test-sum-tree ()
  (check
   (= (sum-tree 5) 5)
   (= (sum-tree 'a) 0)
   (= (sum-tree '()) 0)
   (= (sum-tree '((a 1) b 2 (c 3))) 6)
   (= (sum-tree '((3 bears) (3 bowls) (1 girl))) 7)))

;;;
;;;    8.42
;;;
;;;    See implementation/subst.lisp
;;;    
(defun subst (new old tree &key key (test #'eql))
  (if (null key)
      (cond ((funcall test old tree) new)
            ((atom tree) tree)
            (t (cons (subst new old (car tree) :test test)
                     (subst new old (cdr tree) :test test))))
      (cond ((funcall test old (funcall key tree)) new)
            ((atom tree) tree)
            (t (cons (subst new old (car tree) :key key :test test)
                     (subst new old (cdr tree) :key key :test test)))) ))

;; (cond ((satisfies-the-test old tree :test test
;;                               :test-not test-not :key key)
;;           new)
(defun subst (new old tree &rest keys &key key (test #'eql))
  (cond ((and key (funcall test old (funcall key tree))) new)
        ((and (null key) (funcall test old tree)) new)
        ((atom tree) tree)
        (t (cons (apply #'subst new old (car tree) keys)
                 (apply #'subst new old (cdr tree) keys)))) )

;;;    CLHS example
;(subst 'x 3 '(1 (1 2) (1 2 3) (1 2 3 4)) :key #'(lambda (y) (and (listp y) (third y)))) 

(deftest test-subst ()
  (check
   (equal (subst 'topping 'pickle #1='(ice cream (with (fudge)) for dessert)) #1#)
   (equal (subst 'topping 'fudge #1#) '(ice cream (with (topping)) for dessert))
   (equal (subst 'jalapeno 'and '(tacos (tamales (and (salsa))))) '(tacos (tamales (jalapeno (salsa)))) )
   (equal (subst 'x pi `(2 3 ,pi (,pi (,pi)) 4) :test #'(lambda (old elt) (and (numberp elt) (= old elt))))
          '(2 3 X (X (X)) 4))
   (equal (subst 'x 3 '(1 (1 2) (1 2 3) (1 2 3 4)) :key #'(lambda (y) (and (listp y) (third y))))
          '(1 (1 2) X X))))

;;;
;;;    8.43
;;;    
(defun flatten (tree)
  (cond ((atom tree) tree) ; Return initial atom as is. This also supports dotted lists.
        ((null (car tree)) (flatten (cdr tree)))
        ((atom (car tree)) (cons (car tree) (flatten (cdr tree))))
        (t (flatten (cons (car (car tree)) (cons (cdr (car tree)) (cdr tree)))) )))

(defun make-random-tree (generator)
  (if (< (random 1d0) 0.5)
      (list (funcall generator))
      (cons (make-random-tree generator)
            (make-random-tree generator))))

;; (let ((i 0)) (make-random-tree #'(lambda () (prog1 i (incf i)))))
;; (let ((i 0)) (make-random-tree #'(lambda () (prog1 (code-char (+ i (char-code #\A))) (incf i)))))  

(deftest test-flatten ()
  (check
   (equal (flatten '(a . b)) '(A . B))
   (equal (flatten '(a b . c)) '(A B . C))
   (equal (flatten '(a (b) . c)) '(A B . C))
   (equal (flatten '((a) b . c)) '(A B . C))
   (equal (flatten '((a b) . c)) '(A B . C))
   (equal (flatten '((a b (r)) a c (a d ((a (b)) r) a))) '(A B R A C A D A B R A))
   (equal (flatten '((((((((0) (((1) (2) (3) (4) (((((5) 6) (7) (8) 9) 10) 11) (12) (13) 14) 15) ((16) ((((((17) 18) (19) (((20) 21) ((((22) (23) 24) 25) 26) (27) 28) 29) (30) 31) (32) (33) (34) 35) 36) 37)
                           (38) 39) ((40) 41) ((((((42) ((((43) ((((44) (((45) (46) 47) ((48) (49) (50) ((51) 52) (53) (54) 55) 56) (57) 58) 59) (60) 61) 62) 63) (64) 65) ((66) 67) 68) 69) 70) 71) 72) (73) 74)
                         (75) 76) 77) (78) (79) 80) (((81) (82) 83) 84) 85) 86))
          '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54
            55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86))))

;;;
;;;    8.44
;;;    
(defun tree-depth (tree)
  (cond ((atom tree) 0)
        (t (cl:1+ (max (tree-depth (car tree)) (tree-depth (cdr tree)))) )))

(deftest test-tree-depth ()
  (check
   (= (tree-depth '(a . b)) 1)
   (= (tree-depth '((a b c d))) 5)
   (= (tree-depth '((a . b) . (c . d))) 2)))

;;;
;;;    8.45
;;;    
(defun paren-depth (l)
  (cond ((atom l) 0)
        (t (max (cl:1+ (paren-depth (first l)))
                (paren-depth (rest l)))) ))

(deftest test-paren-depth ()
  (check
   (= (paren-depth '(a b c)) 1)
   (= (paren-depth '(a b ((c) d) e)) 3)))

;;;
;;;    8.46
;;;    
(defun count-up (n)
  (cond ((zerop n) '())
        (t (append (count-up (cl:1- n)) (list n)))) )

(defun loop-count-up (n)
  (loop for i from 1 to n collect i))

(deftest test-count-up ()
  (check
   (equal (count-up 0) (loop-count-up 0))
   (equal (count-up 1) (loop-count-up 1))
   (equal (count-up 10) (loop-count-up 10))))

;;;
;;;    8.47
;;;    
(defun make-loaf (n)
  (if (zerop n)
      '()
      (cons 'x (make-loaf (cl:1- n)))) )

(deftest test-make-loaf ()
  (check
   (equal (make-loaf 0) '())
   (equal (make-loaf 1) '(x))
   (equal (make-loaf 5) '(x x x x x))))

;;;
;;;    8.48
;;;    
(defun bury (obj n)
  (cond ((zerop n) obj)
        (t (list (bury obj (cl:1- n)))) ))

;;;
;;;    From tour8.lsp (tail recursive)
;;;    
(defun bury (obj n)
  (cond ((zerop n) obj)
	(t (bury (list obj) (cl:1- n)))) )

(deftest test-bury ()
  (check
   (equal (bury 'fred 0) 'fred)
   (equal (bury 'fred 1) '(fred))
   (equal (bury 'fred 5) '(((((fred)))) ))))

;;;
;;;    8.49
;;;    
(defun pairings (xs ys)
  (cond ((endp xs) '())
        ((endp ys) '())
        (t (cons (list (first xs) (first ys)) (pairings (rest xs) (rest ys)))) ))

(deftest test-pairings ()
  (check
   (equal (pairings '() '()) '())
   (equal (pairings '(a) '()) '())
   (equal (pairings '() '(1)) '())
   (equal (pairings '(a b c) '(1 2 3)) '((a 1) (b 2) (c 3)))
   (equal (pairings '(a b) '(1 2 3)) '((a 1) (b 2)))
   (equal (pairings '(a b c) '(1 2)) '((a 1) (b 2)))) )

;;;
;;;    8.50
;;;    
(defun sublists (l)
  (cond ((endp l) '())
        (t (cons l (sublists (rest l)))) ))

(deftest test-sublists ()
  (check
   (equal (sublists '()) '())
   (equal (sublists '(a)) '((a)))
   (equal (sublists '(fee fie foe)) '((fee fie foe) (fie foe) (foe)))) )

;;;
;;;    8.51
;;;    
(defun reverse (l)
  (reverse-aux l '()))

(defun reverse-aux (l result)
  (cond ((endp l) result)
        (t (reverse-aux (rest l) (cons (first l) result)))) )

(deftest test-reverse ()
  (check
   (equal (reverse '()) (cl:reverse '()))
   (equal (reverse '(a)) (cl:reverse '(a)))
   (equal (reverse #1='(a b c d)) (cl:reverse #1#))
   (equal (reverse #2='(a b (c (d)) (e))) (cl:reverse #2#))))

;;;
;;;    8.52
;;;    
(defun union (a b)
  (cond ((null a) b)
        ((member (first a) b) (union (rest a) b))
        (t (union (rest a) (cons (first a) b)))) )

(deftest test-union ()
  (check
   (set-equal (union '(a b) '(a b)) '(a b))
   (set-equal (union '(a b c) '(d e f)) '(a b c d e f))
   (set-equal (union '(b c a) '(a d e)) '(a b c d e))
   (set-equal (union '() #1='(a b c d)) (cl:union '() #1#))
   (set-equal (union #1# '()) (cl:union #1# '()))
   (set-equal (union #1# #2='(1 2 3 4)) (cl:union #1# #2#))
   (set-equal (union #1# #1#) (cl:union #1# #1#))
   (set-equal (union #1# #3='(d c b a)) (cl:union #1# #3#))
   (set-equal (union #1# #4='(b d)) (cl:union #1# #4#))))

;;;
;;;    8.53
;;;    
(defun largest-even (l)
  (cond ((endp l) nil)
        ((evenp (first l)) (largest-even-aux (first l) (rest l)))
        (t (largest-even (rest l)))) )

(defun largest-even-aux (n l)
  (cond ((endp l) n)
        ((evenp (first l)) (largest-even-aux (max n (first l)) (rest l)))
        (t (largest-even-aux n (rest l)))) )

(deftest test-largest-even ()
  (check
   (null (largest-even '()))
   (null (largest-even '(1 3 5 7)))
   (= (largest-even '(2)) 2)
   (= (largest-even '(1 2 3 4 5)) 4)
   (= (largest-even '(2 6 16 6 0 2 2 12 8 0)) 16)))

;;;
;;;    I misread his spec. No even number => 0
;;;    
(defun largest-even-touretzky (l)
  (cond ((endp l) 0)
        ((evenp (first l)) (max (first l) (largest-even-touretzky (rest l))))
        (t (largest-even-touretzky (rest l)))) )

(deftest test-largest-even-touretzky ()
  (check
   (zerop (largest-even-touretzky '()))
   (zerop (largest-even-touretzky '(1 3 5 7)))
   (= (largest-even-touretzky '(2)) 2)
   (= (largest-even-touretzky '(1 2 3 4 5)) 4)
   (= (largest-even-touretzky '(2 6 16 6 0 2 2 12 8 0)) 16)))
;;;
;;;    8.54
;;;    
(defun huge (n)
  (huge-aux n n))

(defun huge-aux (base power)
  (cond ((zerop power) 1)
        (t (* base (huge-aux base (cl:1- power)))) ))

(deftest test-huge-aux ()
  (check
   (= (huge-aux 1 0) 1)
   (= (huge-aux 1 1) 1)
   (= (huge-aux 2 0) 1)
   (= (huge-aux 2 1) 2)
   (= (huge-aux 2 2) 4)
   (= (huge-aux 5 2) 25)))

(deftest test-huge ()
  (check
   (= (huge 1) 1)
   (= (huge 2) 4)
   (= (huge 3) 27)
   (= (huge 4) 256)))

;;;
;;;    8.56
;;;    
(defun every-other (l)
  (labels ((ping (l)
             (cond ((null l) '())
                   (t (cons (first l) (pong (rest l)))) ))
           (pong (l)
             (cond ((null l) '())
                   (t (ping (rest l)))) ))
    (ping l)))

(defun every-other (l)
  (loop for elt in l
        for flag = t then (not flag)
        when flag 
        collect elt))

;;;
;;;    Lame...
;;;    
(defun every-other (l)
  (cond ((null l) '())
        (t (cons (first l) (every-other (rest (rest l)))) )))

(deftest test-every-other ()
  (check
   (equal (every-other '()) '())
   (equal (every-other '(a)) '(a))
   (equal (every-other '(a b)) '(a))
   (equal (every-other '(a b c)) '(a c))
   (equal (every-other '(a b c d e f g)) '(a c e g))
   (equal (every-other '(i came i saw i conquered)) '(i i i))))

;;;
;;;    8.57
;;;    
(defun left-half (l)
  (left-half-aux l (ceiling (length l) 2)))

(defun left-half-aux (l n)
  (cond ((zerop n) '())
        (t (cons (first l) (left-half-aux (rest l) (cl:1- n)))) ))

(deftest test-left-half ()
  (check
   (equal (left-half '()) '())
   (equal (left-half '(a)) '(a))
   (equal (left-half '(a b)) '(a))
   (equal (left-half '(a b c)) '(a b))
   (equal (left-half '(a b c d e)) '(a b c))
   (equal (left-half '(1 2 3 4 5 6 7 8)) '(1 2 3 4))))

(defun right-half (l)
  (labels ((find-right-half (l i sublists)
             (cond ((null l) (cl:nth (truncate (cl:1- i) 2) sublists))
                   (t (find-right-half (rest l) (cl:1+ i) (cons l sublists)))) ))
    (find-right-half l 0 '())))

(deftest test-right-half ()
  (check
   (equal (right-half '()) '())
   (equal (right-half '(a)) '(a))
   (equal (right-half '(a b)) '(b))
   (equal (right-half '(a b c)) '(b c))
   (equal (right-half '(a b c d e)) '(c d e))
   (equal (right-half '(1 2 3 4 5 6 7 8)) '(5 6 7 8))))

;;;
;;;    8.58
;;;    
(defun merge-lists (l1 l2)
  (cond ((endp l1) l2)
        ((endp l2) l1)
        ((< (first l2) (first l1)) (cons (first l2) (merge-lists l1 (rest l2))))
        (t (cons (first l1) (merge-lists (rest l1) l2)))) )

(deftest test-merge-lists ()
  (check
   (equal (merge-lists '() '()) '())
   (equal (merge-lists '(1) '()) '(1))
   (equal (merge-lists '() '(1)) '(1))
   (equal (merge-lists '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
   (equal (merge-lists '(4 5 6) '(1 2 3)) '(1 2 3 4 5 6))
   (equal (merge-lists '(1 2 6 8 10 12) '(2 3 5 9 13)) '(1 2 2 3 5 6 8 9 10 12 13))
   (equal (merge-lists '(1 2 6 8 10 12) '(2d0 3 5 9 13)) '(1 2 2d0 3 5 6 8 9 10 12 13))))

;;;
;;;    8.61
;;;    
(defun count-up (n)
  (count-up-aux n '()))

(defun count-up-aux (n result)
  (cond ((zerop n) result)
        (t (count-up-aux (cl:1- n) (cons n result)))) )

;;;
;;;    8.62
;;;    
(defun factorial (n)
  (factorial-aux n 1))

(defun factorial-aux (n fact)
  (cond ((zerop n) fact)
        (t (factorial-aux (cl:1- n) (* n fact)))) )

;;;
;;;    8.63
;;;    This doesn't really require a separate accumulator other than B.
;;;    However, the result set may grow whereas B stays fixed size throughout.
;;;
;;;    Note how UNION-AUX and SET-DIFFERENCE-AUX are the same function!
;;;    
(defun union (a b)
  (union-aux a b b))

(defun union-aux (a b union)
  (cond ((null a) union)
        ((member (first a) b) (union-aux (rest a) b union))
        (t (union-aux (rest a) b (cons (first a) union)))) )

(defun intersection (a b)
  (intersection-aux a b '()))

(defun intersection-aux (a b intersection)
  (cond ((null a) intersection)
        ((member (first a) b) (intersection-aux (rest a) b (cons (first a) intersection)))
        (t (intersection-aux (rest a) b intersection))))

(defun set-difference (a b)
  (set-difference-aux a b '()))

(defun set-difference-aux (a b difference)
  (cond ((null a) difference)
        ((member (first a) b) (set-difference-aux (rest a) b difference))
        (t (set-difference-aux (rest a) b (cons (first a) difference)))) )

;;;
;;;    8.64
;;;    
(defun tree-find-if (f tree)
  (cond ((null tree) nil)
        ((atom tree) (if (funcall f tree)
                         tree
                         nil))
        (t (or (tree-find-if f (car tree))
               (tree-find-if f (cdr tree)))) ))

(deftest test-tree-find-if ()
  (check
   (equal (tree-find-if #'oddp '((2 4) (5 6) 7)) 5)
   (equal (tree-find-if #'evenp '((2 4) (5 6) 7)) 2)
   (null (tree-find-if #'minusp '((2 4) (5 6) 7)))) )

;;;
;;;    8.65
;;;    
(defun tr-count-slices (loaf)
  (labels ((count-slices (loaf result)
             (cond ((null loaf) result)
                   (t (count-slices (rest loaf) (cl:1+ result)))) ))
    (count-slices loaf 0)))

(deftest test-tr-count-slices ()
  (check
   (= (tr-count-slices '()) 0)
   (= (tr-count-slices '(x)) 1)
   (= (tr-count-slices '(x x x x x)) 5)))

(defun reverse (l)
  (labels ((do-reverse (l result)
             (cond ((endp l) result)
                   (t (do-reverse (rest l) (cons (first l) result)))) ))
    (do-reverse l '())))

;;;
;;;    8.66
;;;    
(defun arith-eval (expr)
  (cond ((atom expr) expr)
        (t (funcall (second expr) (arith-eval (first expr)) (arith-eval (third expr)))) ))

(deftest test-arith-eval ()
  (check
   (= (arith-eval 8) 8)
   (= (arith-eval '(3 + 4)) 7)
   (= (arith-eval '(3 - 4)) -1)
   (= (arith-eval '(3 * 4)) 12)
   (= (arith-eval '(3 / 4)) 3/4)
   (= (arith-eval '(2 + (3 * 4))) 14)
   (= (arith-eval '((3 + 5) - (8 + 6))) -6)
   (= (arith-eval '((2 + 2) - (3 * (4 * (12 / 6)))) ) -20)))

;;;
;;;    8.67
;;;    
(defun legalp (expr)
  (cond ((atom expr) (numberp expr))
        (t (and (member (second expr) '(+ - * /))
                (legalp (first expr))
                (legalp (third expr)))) ))

(defun operatorp (obj)
  (member obj '(+ - * /)))

(defun legalp (expr)
  (cond ((atom expr) (numberp expr))
        (t (handler-case (destructuring-bind (op1 operator op2) expr
                           (and (operatorp operator)
                                (legalp op1)
                                (legalp op2)))
             (error (e)
               (declare (ignore e)))) )))

(deftest test-legalp ()
  (check
   (legalp 4)
   (legalp '((2 * 2) - 3))
   (not (legalp nil))
   (not (legalp '(a b c d)))
   (not (legalp '(2 +)))
   (not (legalp '(2 + (3))))
   (not (legalp '(2 + 3 4)))
   (legalp 8)
   (legalp '(3 + 4))
   (legalp '(3 - 4))
   (legalp '(3 * 4))
   (legalp '(3 / 4))
   (legalp '(2 + (3 * 4)))
   (legalp '((3 + 5) - (8 + 6)))
   (legalp '((2 + 2) - (3 * (4 * (12 / 6)))) )))

;;;
;;;    8.70
;;;    
(defun factors (n)
  (labels ((factor (n p)
             (cond ((= n 1) '())
                   ((zerop (rem n p)) (cons p (factor (/ n p) p)))
                   (t (factor n (cl:1+ p)))) ))
    (factor n 2)))

(defun factor-tree (n)
  (labels ((factor (n p)
             (cond ((= n 1) '()) ; Only necessary for initial arg of 1!
                   ((= n p) n)
                   (t (multiple-value-bind (q r) (truncate n p)
                        (if (zerop r)
                            (list n p (factor q p))
                            (factor n (cl:1+ p)))) ))))
    (factor n 2)))

(deftest test-factor-tree ()
  (check
   (equal (factor-tree 60) '(60 2 (30 2 (15 3 5))))
   (equal (factor-tree 7) 7)
   (equal (factor-tree 9) '(9 3 3))
   (equal (factor-tree 8) '(8 2 (4 2 2)))
   (equal (factor-tree 1) NIL)
   (equal (factor-tree 2) 2)))
