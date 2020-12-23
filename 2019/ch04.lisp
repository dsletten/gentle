;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               ch04.lisp
;;;;
;;;;   Started:            Sun Sep  1 02:00:22 2019
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
;(load "/Users/dsletten/lisp/packages/collections.lisp")

(defpackage :ch04 (:use :common-lisp :test) (:shadow :not))
;(defpackage :ch04 (:use :common-lisp :test :collections))

(in-package :ch04)

;;;
;;;    4.1
;;;    
(defun make-even (n)
  (if (oddp n)
      (1+ n)
      n))

(deftest test-make-even ()
  (check
   (= (make-even 0) 0)
   (= (make-even 1) 2)
   (= (make-even 2) 2)
   (= (make-even -1) 0)
   (= (make-even -2) -2)))

;;;
;;;    4.2
;;;    
;; (defun further (x)
;;   (if (plusp x)
;;       (1+ x)
;;       (1- x)))

(defun further (x)
  (if (plusp x)
      (1+ x)
      (if (minusp x)
          (1- x)
          x)))

(defun further (x)
  (cond ((plusp x) (1+ x))
        ((minusp x) (1- x))
        (t x)))

(defun further (x)
  (if (zerop x)
      x
      (+ x (signum x))))

(defun further (x)
  (+ x (signum x)))

(defun further (x)
  (if (zerop x)
      x
      (+ x (/ (abs x) x)))) ; Poor man's SIGNUM

(deftest test-further ()
  (check
   (= (further 1) 2)
   (= (further 0.5) 1.5)
   (= (further 3/2) 5/2)
   (= (further -1) -2)
   (= (further -0.5) -1.5)
   (= (further -3/2) -5/2)
   (= (further 0) 0)
   (= (further 0.0) 0.0)))

;;;
;;;    4.3
;;;    
(defun not (p)
  (if p nil t))

(deftest test-not ()
  (check
   (eq (not #1=(= 2 3)) (cl:not #1#))
   (eq (not #2=(< 2 3)) (cl:not #2#))))

;;;
;;;    4.4
;;;    
(defun ordered (a b)
  (if (< b a)
      (list b a)
      (list a b)))

(deftest test-ordered ()
  (check
   (equal (ordered 2 3) '(2 3))
   (equal (ordered 3 2) '(2 3))
   (equal (ordered 2 2) '(2 2))))

;;;
;;;    4.8
;;;    
(defun emphasize (s)
  (cond ((eql (first s) 'good) (cons 'great (rest s)))
        ((eql (first s) 'bad) (cons 'awful (rest s)))
        (t (cons 'very s))))

(defun emphasize (s)
  (destructuring-bind (adjective . more) s
    (case adjective
      (good (cons 'great more))
      (bad (cons 'awful more))
      (otherwise (cons 'very s)))) )

(defun emphasize (s)
  (case (first s)
    (good (cons 'great (rest s)))
    (bad (cons 'awful (rest s)))
    (otherwise (cons 'very s))))

(deftest test-emphasize ()
  (check
   (equal (emphasize '(good day)) '(great day))
   (equal (emphasize '(bad day)) '(awful day))
   (equal (emphasize '(long day)) '(very long day))))

;;;
;;;    4.9
;;;    
(defun make-odd (n)
  (cond ((cl:not (oddp n)) (1+ n))
        (t n)))

(defun make-odd (n)
  (+ n (if (oddp n) 0 1)))

(deftest test-make-odd ()
  (check
   (oddp (make-odd -2))
   (oddp (make-odd -1))
   (oddp (make-odd 0))
   (oddp (make-odd 1))
   (oddp (make-odd 2))))

;;;
;;;    4.10
;;;
(defun constrain (x min max)
  (cond ((< x min) min)
        ((> x max) max)
        (t x)))

(defun constrain (x min max)
  (if (< x min)
      min
      (if (> x max)
          max
          x)))

(defun constrain (x min max)
  (assert (< min max))
  (max (min x max) min))

(deftest test-constrain ()
  (check
   (= (constrain 3 -50 50) 3)
   (= (constrain 92 -50 50) 50)
   (= (constrain -1 0 10) 0)))

;;;
;;;    4.11
;;;
(defun first-zero (nums)
  (cond ((zerop (first nums)) 'first)
        ((zerop (second nums)) 'second)
        ((zerop (third nums)) 'third)
        (t 'none)))

(defun first-zero (ns)
  (destructuring-bind (a b c) ns
    (cond ((zerop a) 'first)
          ((zerop b) 'second)
          ((zerop c) 'third)
          (t 'none))))

(defun first-zero (ns)
  (case (position 0 ns)
    (0 'first)
    (1 'second)
    (2 'third)
    (otherwise 'none)))

(defun first-zero (ns)
  (let ((i (position 0 ns)))
    (if (null i)
        'none
        (aref #(first second third) i))))

(defun first-zero (ns)
  (let ((i (position 0 ns)))
    (if (null i)
        'none
        (intern (format nil "~:@(~:R~)" (1+ i)))) ))

(defun first-zero (ns)
  (intern (format nil "~:@(~:[none~;~:*~:R~]~)" (position 0 (cons :pung ns)))) ) ; !!!!!!!!!

;;;
;;;    Yuck...
;;;    
;; (let ((size 3))
;;   (defun first-zero (ns)
;;     (labels ((check-it (i l result)
;;                (cond ((minusp i) (error "Bad input: ~A" ns))
;;                      ((endp l) (if (zerop i)
;;                                    'none
;;                                    (error "Bad input: ~A" ns)))
;;                      ((zerop (first l)) (first result))
;;                      (t (check-it (1- i) (rest l) (rest result)))) ))
;;       (check-it 2 ns (loop for i from 1 to size collect (intern (format nil "~:@(~:R~)" i)))) )))

(defun first-zero (ns)
  (labels ((check-it (l result)
             (cond ((and (endp l) (endp result)) 'none)
                   ((or (endp l) (endp result)) (error "Bad input: ~A" ns))
                   ((zerop (first l)) (first result))
                   (t (check-it (rest l) (rest result)))) ))
    (check-it ns '(first second third))))
;    (check-it ns (loop for i from 1 to 3 collect (intern (format nil "~:@(~:R~)" i)))) ))

;;;
;;;    Not as clean as recursive version!
;;;    
(defun first-zero (ns)
  (do ((l ns (rest l))
       (result '(first second third) (rest result)))
      ((endp l) (if (endp result) 'none (error "Bad input: ~A" ns)))
    (cond ((endp result) (error "Bad input: ~A" ns))
          ((zerop (first l)) (return (first result)))) ))

;;;
;;;    Better translation.
;;;    
(defun first-zero (ns)
  (do ((l ns (rest l))
       (result '(first second third) (rest result)))
      ((and (endp l) (endp result)) 'none)
    (cond ((or (endp l) (endp result)) (error "Bad input: ~A" ns))
          ((zerop (first l)) (return (first result)))) ))

(deftest test-first-zero ()
  (check
   (eq (first-zero '(0 3 4)) 'first)
   (eq (first-zero '(3 0 4)) 'second)
   (eq (first-zero '(3 4 0)) 'third)
   (eq (first-zero '(1 2 3)) 'none)))

;;;
;;;    4.12
;;;
(defconstant limit 99)
;;
;;    Broken!
;; (defun cycle (n)
;;   (cond ((= n limit) 1)
;;         (t (1+ n))))

(defun cycle (n)
  (1+ (mod n limit)))

;; (let ((ring (make-circular-list (loop for i from 1 limit collect i))))
;;   (defun cycle (n)
;;     (

(deftest test-cycle ()
  (check
   (every #'(lambda (x y) (= (1+ x) y)) (loop for i from 1 to (1- limit) collect i) (loop for i from 1 to (1- limit) collect (cycle i)))
   (= (cycle limit) 1)))

;;;
;;;    4.13
;;;
(defun how-compute (op1 op2 val)
  (cond ((= (+ op1 op2) val) 'sum-of)
        ((= (* op1 op2) val) 'product-of)
        (t '(beats me))))

(deftest test-how-compute ()
  (check
   (eq (how-compute 3 4 7) 'sum-of)
   (eq (how-compute 3 4 12) 'product-of)
   (equal (how-compute 1 2 9) '(beats me))
   (eq (how-compute 2 2 4) 'sum-of) ;????
   (eq (how-compute 0 0 0) 'sum-of))) ;????

;;;
;;;    4.15
;;;    
;; Ha!
;; (defun geq (x y)
;;   (>= x y))

(defun geq (x y)
  (or (> x y) (= x y)))

(deftest test-geq ()
  (check
   (geq 8 8)
   (geq 8 2)
   (cl:not (geq 2 8))))

;;;
;;;    4.16
;;;
(defun fancy (x)
  (cond ((and (oddp x) (plusp x)) (* x x))
        ((and (oddp x) (minusp x)) (* 2 x))
        (t (/ x 2))))

(defun fancy (x)
  (cond ((typep x '(and (integer 1) (satisfies oddp))) (* x x))
        ((typep x '(and (integer * (0)) (satisfies minusp))) (* 2 x))
        (t (/ x 2))))

(defun fancy-factor (x)
  (if (oddp x)
      (if (plusp x) x 2)
      1/2))

(defun fancy (x)
  (* x (fancy-factor x)))

(deftest test-fancy ()
  (check
   (= (fancy 3) 9)
   (= (fancy -7) -14)
   (= (fancy 0) 0)
   (= (fancy 8) 4)
   (= (fancy -4) -2)))
;   (= (fancy pi) (/ pi 2)))) ; First version of FANCY => error

;;;
;;;    4.17
;;;
(defun categorize (sex age)
  (cond ((or (eq sex 'boy) (eq sex 'girl)) (eq age 'child))
        ((or (eq sex 'man) (eq sex 'woman)) (eq age 'adult))
        (t nil)))

(defun categorize (sex age)
  (case sex
    ((boy girl) (eq age 'child))
    ((man woman) (eq age 'adult))
    (otherwise nil)))

(deftype child ()
  '(member boy girl))

(deftype adult ()
  '(member man woman))

(defun categorize (sex age)
  (typep sex age))

(deftest test-categorize ()
  (check
   (categorize 'boy 'child)
   (categorize 'girl 'child)
   (categorize 'man 'adult)
   (categorize 'woman 'adult)
   (cl:not (categorize 'boy 'adult))
   (cl:not (categorize 'girl 'adult))
   (cl:not (categorize 'man 'child))
   (cl:not (categorize 'woman 'child))))

;;;
;;;    4.18 See pensoj 130807!!
;;;
(defun play (first second)
  (cond ((eq first second) 'tie)
        ((or (and (eq first 'rock) (eq second 'paper))
             (and (eq first 'paper) (eq second 'scissors))
             (and (eq first 'scissors) (eq second 'rock)))
         'second-wins)
        (t 'first-wins)))

(defun play (first second)
  (ecase first
    (rock (ecase second
            (rock 'tie)
            (paper 'second-wins)
            (scissors 'first-wins)))
    (paper (ecase second
             (rock 'first-wins)
             (paper 'tie)
             (scissors 'second-wins)))
    (scissors (ecase second
                (rock 'second-wins)
                (paper 'first-wins)
                (scissors 'tie)))) )

(defvar *jan-ken-pon* '((rock (rock scissors paper))
                        (paper (paper rock scissors))
                        (scissors (scissors paper rock))))
(defun play (first second)
  (destructuring-bind (_ (tie win lose)) (assoc first *jan-ken-pon*)
    (declare (ignore _))
    (cond ((eq second tie) 'tie)
          ((eq second win) 'first-wins)
          ((eq second lose) 'second-wins))))

(defun play (first second)
  (labels ((wins (a b)
             (ecase a
               (rock (eq b 'scissors))
               (paper (eq b 'rock))
               (scissors (eq b 'paper)))) )
    (cond ((eq first second) 'tie)
          ((wins first second) 'first-wins)
          (t 'second-wins))))
          ;; ((wins second first) 'second-wins) ; Unnecessary. Second player wins if first 2 clauses fail.
          ;; (t (error "Huh?")))) )

;(dolist (first #1='(rock paper scissors)) (dolist (second #1#) (print `(eq (play ',first ',second)))))

(deftest test-play ()
  (check
   (EQ (PLAY 'ROCK 'ROCK) 'tie) 
   (EQ (PLAY 'ROCK 'PAPER) 'second-wins) 
   (EQ (PLAY 'ROCK 'SCISSORS) 'first-wins) 
   (EQ (PLAY 'PAPER 'ROCK) 'first-wins) 
   (EQ (PLAY 'PAPER 'PAPER) 'tie) 
   (EQ (PLAY 'PAPER 'SCISSORS) 'second-wins) 
   (EQ (PLAY 'SCISSORS 'ROCK) 'second-wins) 
   (EQ (PLAY 'SCISSORS 'PAPER) 'first-wins) 
   (EQ (PLAY 'SCISSORS 'SCISSORS) 'tie)))

;;;
;;;    4.20
;;;
(defun compare (x y)
  (cond ((= x y) 'numbers-are-the-same)
        ((< x y) 'first-is-smaller)
        ((> x y) 'first-is-bigger)))

(defun compare (x y)
  (cond ((= x y) 'numbers-are-the-same)
        ((< x y) 'first-is-smaller)
        (t 'first-is-bigger)))

(defun compare (x y)
  (if (= x y)
      'numbers-are-the-same
      (if (< x y)
          'first-is-smaller
          'first-is-bigger)))

(defun compare (x y)
  (or (and (= x y) 'numbers-are-the-same)
      (and (< x y) 'first-is-smaller)
      'first-is-bigger))

(deftest test-compare ()
  (check
   (eq (compare 2 2) 'numbers-are-the-same)
   (eq (compare 2d0 2d0) 'numbers-are-the-same)
   (eq (compare 2 2d0) 'numbers-are-the-same)
   (eq (compare 2 3) 'first-is-smaller)
   (eq (compare 2d0 3d0) 'first-is-smaller)
   (eq (compare 2 3d0) 'first-is-smaller)
   (eq (compare -2 -3) 'first-is-bigger)
   (eq (compare -2d0 -3d0) 'first-is-bigger)
   (eq (compare -2 -3d0) 'first-is-bigger)))

;;;
;;;    4.21
;;;
(defun gtest (x y)
  (or (> x y)
      (zerop x)
      (zerop y)))

(defun gtest (x y)
  (if (> x y)
      t
      (if (zerop x)
          t
          (zerop y))))

(defun gtest (x y)
  (if (not (> x y))
      (if (not (zerop x))
          (zerop y)
          t)
      t))

;;    Superfluous
(defun gtest (x y)
  (cond ((> x y) t)
        ((zerop x) t)
        ((zerop y) t)
        (t nil)))

(defun gtest (x y)
  (cond ((> x y))
        ((zerop x))
        ((zerop y))
        (t nil)))

(deftest test-gtest ()
  (check
   (gtest 9 4)
   (gtest 9d0 4d0)
   (gtest 9 4d0)
   (not (gtest 4 9))
   (gtest 9 0)
   (gtest 0 4)
   (gtest 0d0 0d0)))

;;;
;;;    4.22
;;;    
(defun boilingp (temperature scale)
  (cond ((eq scale 'fahrenheit) (> temperature 212))
        ((eq scale 'celsius) (> temperature 100))
        (t (error "Unknown scale: ~A" scale))))

(defun boilingp (temperature scale)
  (ecase scale
    (fahrenheit (> temperature 212))
    (celsius (> temperature 100))))

(defun boilingp (temperature scale)
  (if (eq scale 'fahrenheit)
      (> temperature 212)
      (if (eq scale 'celsius)
          (> temperature 100)
          (error "Unknown scale: ~A" scale))))

(let ((boiling-points `((fahrenheit . ,#'(lambda (temperature) (> temperature 212)))
                        (celsius . ,#'(lambda (temperature) (> temperature 100)))) ))
  (defun boilingp (temperature scale)
    (let ((test (cdr (assoc scale boiling-points))))
      (if (null test)
          (error "Unknown scale: ~A" scale)
          (funcall test temperature)))) )

;;;
;;;    We lose the error checking here...
;;;    
(defun boilingp (temperature scale)
  (or (and (eq scale 'fahrenheit) (> temperature 212))
      (and (eq scale 'celsius) (> temperature 100))))

(deftest test-boilingp ()
  (check
   (boilingp 270 'fahrenheit)
   (boilingp 212.1d0 'fahrenheit)
   (not (boilingp 200 'fahrenheit))
   (not (boilingp 32d0 'fahrenheit))
   (boilingp 115 'celsius)
   (boilingp 100.1d0 'celsius)
   (not (boilingp 99 'celsius))
   (not (boilingp -40d0 'celsius))))

;;;
;;;    4.29
;;;
(defun logical-and (p q)
  (if p
      (if q
          t
          nil)
      nil))

(defun logical-and (p q)
  (cond (p (cond (q t) (t nil)))
        (t nil)))

(deftest test-logical-and ()
  (check
   (eq (logical-and 2 4) t)
   (eq (logical-and "pung" 'foo) t)
   (eq (logical-and t (oddp 3)) t)
   (eq (logical-and nil t) nil)
   (eq (logical-and nil 8) nil)
   (eq (logical-and t nil) nil)
   (eq (logical-and (> 4 3) (< 4 3)) nil)))

;;;
;;;    4.30
;;;
(defun logical-or (p q)
  (or (and p t)
      (and q t)))

(defun logical-or (p q)
  (if p
      t
      (if q
          t
          nil)))

(defun logical-or (p q)
  (cond (p t)
        (q t)
        (t nil)))

(deftest test-logical-or ()
  (check
   (eq (logical-or 2 4) t)
   (eq (logical-or "pung" 'foo) t)
   (eq (logical-or nil 4) t)
   (eq (logical-or 4 nil) t)
   (eq (logical-or (> 4 3) (> 3 4)) t)
   (eq (logical-or nil nil) nil)
   (eq (logical-or (> 2 3) (> 3 4)) nil)))
