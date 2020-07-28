;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               ch04.lisp
;;;;
;;;;   Started:            Wed Jul 31 11:49:19 2013
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

(defpackage :ch04 (:use :common-lisp :test) (:shadow :not))

(in-package :ch04)

;;;
;;;    4.10
;;;
(defun constrain (x min max)
  (cond ((< x min) min)
        ((> x max) max)
        (t x)))

;;
;;    See SICP ch. 1 MAX-2 and MEDIAN
;;    
(defun constrain (x min max)
  (max (min x max) min))

(deftest test-constrain ()
  (check
   (= (constrain 3 -50 50) 3)
   (= (constrain 92 -50 50) 50)
   (= (constrain -74 -50 50) -50)))

;;;
;;;    4.11
;;;
(defun firstzero (l)
  (cond ((zerop (first l)) 'first)
        ((zerop (second l)) 'second)
        ((zerop (third l)) 'third)
        (t 'none)))

(defun firstzero (l)
  (destructuring-bind (first second third) l
    (cond ((zerop first) 'first)
          ((zerop second) 'second)
          ((zerop third) 'third)
          (t 'none))))

(defun firstzero (l)
  (case (position 0 l :test #'=)
    (0 'first)
    (1 'second)
    (2 'third)
    (otherwise 'none)))

(defun firstzero (l)
  (labels ((find-first (l labels)
             (cond ((endp labels) 'none)
                   ((zerop (first l)) (first labels))
                   (t (find-first (rest l) (rest labels)))) ))
    (find-first l '(first second third))))

(defun firstzero (l)
  (loop for i from 1
        for n in l
        when (zerop n)
        do (return (intern (string-upcase (format nil "~:R" i))))
        finally (return 'none)))

(deftest test-firstzero ()
  (check
   (eq (firstzero '(0 1 2)) 'first)
   (eq (firstzero '(1 0 2)) 'second)
   (eq (firstzero '(1 2 0)) 'third)
   (eq (firstzero '(1 2 3 4 5 6 7 8 9 0 11 12)) 'tenth)
   (eq (firstzero '(1 2 3)) 'none)))

;;;
;;;    4.12
;;;
(defun cycle (n)
  (assert (<= 1 n 99) (n) "N should be between 1 and 99 inclusive.")
  (if (= n 99)
      1
      (+ n 1)))

(defun cycle (n)
  (1+ (mod n 99)))

(deftest test-cycle ()
  (check
   (= (cycle 1) 2)
   (= (cycle 99) 1)))

;;;
;;;    4.13
;;;
(defun howcompute (a b c)
  (cond ((= (+ a b) c) 'sum-of)
        ((= (- a b) c) 'difference-of)
        ((= (* a b) c) 'product-of)
        ((and (cl:not (zerop b)) (= (/ a b) c)) 'quotient-of)
        (t '(beats me))))

(deftest test-howcompute ()
  (check
   (eq (howcompute 3 4 7) 'sum-of)
   (eq (howcompute 3 4 12) 'product-of)
   (eq (howcompute 7 4 3) 'difference-of)
   (eq (howcompute 12 3 4) 'quotient-of)
   (eq (howcompute 2 2 4) 'sum-of) ; Ambiguous
   (eq (howcompute 0 0 0) 'sum-of) ; Ambiguous
   (equal (howcompute 3 4 5) '(beats me))))

(defun how-alike (a b)
  (assert (integerp a) (a) "A must be an integer.")
  (assert (integerp b) (b) "B must be an integer.")
  (cond ((= a b) 'the-same)
        ((and (oddp a) (oddp b)) 'both-odd)
        ((and (evenp a) (evenp b)) 'both-even)
        ((and (minusp a) (minusp b)) 'both-negative)
        ((and (plusp a) (plusp b)) 'both-positive)
        ((and (zerop a) (zerop b)) 'both-zero)
        (t 'not-alike)))

(defun same-sign (x y)
  (or (and (zerop x) (zerop y))
      (and (minusp x) (minusp y))
      (and (plusp x) (plusp y))))

(defun same-sign (x y)
  (cond ((zerop x) (zerop y))
        ((minusp x) (minusp y))
        ((plusp x) (plusp y))
        (t nil)))

(defun same-sign (x y)
  (or (= x y 0)
      (> (* x y) 0)))

(defun same-sign (x y)
  (= (signum x) (signum y)))

;;;
;;;    4.15
;;;
(defun geq (x y)
  (or (> x y)
      (= x y)))

(defun geq (x y)
  (>= x y))

(defun geq (x y)
  (cl:not (< x y)))

(deftest test-geq ()
  (check
   (geq 3 2)
   (geq 3 3)
   (cl:not (geq 2 3))))

;;;
;;;    4.16
;;;
(defun funky (n)
  (assert (integerp n) () "N must be an integer.")
  (cond ((and (oddp n) (plusp n)) (* n n))
        ((and (oddp n) (minusp n)) (* n 2))
        (t (/ n 2))))

(defun funky (n)
  (assert (integerp n) () "N must be an integer.")
  (cond ((evenp n) (/ n 2))
        ((plusp n) (* n n))
        ((minusp n) (* n 2))))

(deftest test-funky ()
  (check
   (= (funky 5) 25)
   (= (funky -3) -6)
   (= (funky 8) 4)
   (= (funky 0) 0)
   (= (funky -6) -3)))

;;;
;;;    4.17
;;;
(defun categorize (sex age)
  (cond ((or (eq sex 'boy) (eq sex 'girl)) (eq age 'child))
        ((or (eq sex 'woman) (eq sex 'man)) (eq age 'adult))
        (t nil)))

(defun categorize (sex age)
  (case sex
    ((boy girl) (eq age 'child))
    ((woman man) (eq age 'adult))
    (otherwise nil)))

(deftest test-categorize ()
  (check
   (categorize 'boy 'child)
   (categorize 'girl 'child)
   (categorize 'woman 'adult)
   (categorize 'man 'adult)
   (cl:not (categorize 'boy 'adult))
   (cl:not (categorize 'girl 'adult))
   (cl:not (categorize 'woman 'child))
   (cl:not (categorize 'man 'child))))

;;;
;;;    4.18
;;;
;; (defun play (player1 player2)
;;   (cond ((eq player1 'rock) (cond ((eq player2 'scissors) 'first-wins)
;;                                   ((eq player2 'paper) 'second-wins)
;;                                   ((eq player2 'rock) 'tie)
;;                                   (t 'illegal-player2)))
;;         ((eq player1 'paper) (cond ((eq player2 'rock) 'first-wins)
;;                                    ((eq player2 'scissors) 'second-wins)
;;                                    ((eq player2 'paper) 'tie)
;;                                    (t 'illegal-player2)))
;;         ((eq player1 'scissors) (cond ((eq player2 'paper) 'first-wins)
;;                                       ((eq player2 'rock) 'second-wins)
;;                                       ((eq player2 'scissors) 'tie)
;;                                       (t 'illegal-player2)))
;;         (t 'illegal-player1)))

;; (defun play (player1 player2)
;;   (cond ((not (legal-move-p player1)) (if (legal-move-p player2)
;;                                           'illegal-player1
;;                                           'both-illegal))
;;         ((not (legal-move-p player2)) 'illegal-player2)
;;         ((eq player1 player2) 'tie)
;;         ((eq player1 'rock) (cond ((eq player2 'scissors) 'first-wins)
;;                                   ((eq player2 'paper) 'second-wins)))
;;         ((eq player1 'paper) (cond ((eq player2 'rock) 'first-wins)
;;                                    ((eq player2 'scissors) 'second-wins)))
;;         ((eq player1 'scissors) (cond ((eq player2 'paper) 'first-wins)
;;                                       ((eq player2 'rock) 'second-wins)))) )

;;;
;;;    Simplified based on Touretzky's version.
;;;    
(defun play (player1 player2)
  (cond ((cl:not (legal-move-p player1)) (if (legal-move-p player2)
                                          'illegal-player1
                                          'both-illegal))
        ((cl:not (legal-move-p player2)) 'illegal-player2)
        ((eq player1 player2) 'tie)
        ((and (eq player1 'rock) (eq player2 'scissors)) 'first-wins)
        ((and (eq player1 'paper) (eq player2 'rock)) 'first-wins)
        ((and (eq player1 'scissors) (eq player2 'paper)) 'first-wins)
        (t 'second-wins)))

(defun play (player1 player2)
  (case player1
    (rock (case player2
            (rock 'tie)
            (paper 'second-wins)
            (scissors 'first-wins)
            (otherwise 'illegal-player2)))
    (paper (case player2
             (rock 'first-wins)
             (paper 'tie)
             (scissors 'second-wins)
             (otherwise 'illegal-player2)))
    (scissors (case player2
                (rock 'second-wins)
                (paper 'first-wins)
                (scissors 'tie)
                (otherwise 'illegal-player2)))
    (otherwise (if (legal-move-p player2)
                   'illegal-player1
                   'both-illegal))))

(defun legal-move-p (move)
  (collections:contains *moves* move))

(defvar *moves* #{'rock 'scissors 'paper})
(defvar *moves-cycle* (collections:make-circular-list (collections:elements *moves*)))
;;;
;;;    This only flags MOVE1 as possibly illegal. MOVE2 will pass through. Presumably in
;;;    a subsequent call to DOMINATESP MOVE2 will be passed back in as the first argument.
;;;    
(defun dominatesp (move1 move2)
  (do ((moves *moves-cycle* (rest moves))
       (seen #{} (collections:add-elt seen (first moves))))
      ((or (eq move1 (first moves)) (collections:contains seen (first moves)))
       (cond ((eq move1 (first moves)) (eq move2 (second moves)))
             ((collections:contains seen (first moves)) 'illegal-move)
             (t nil)))))

(defun dominatesp (move1 move2)
  (labels ((compare-moves (moves seen)
             (cond ((collections:contains seen (first moves)) 'illegal-move)
                   ((eq move1 (first moves)) (eq move2 (second moves)))
                   (t (compare-moves (rest moves) (collections:add-elt seen (first moves)))) )))
    (compare-moves *moves-cycle* #{})))

(defun play (player1 player2)
  (cond ((cl:not (legal-move-p player1)) (if (legal-move-p player2)
                                          'illegal-player1
                                          'both-illegal))
        ((cl:not (legal-move-p player2)) 'illegal-player2)
        ((eq player1 player2) 'tie)
        ((dominatesp player1 player2) 'first-wins)
        ((dominatesp player2 player1) 'second-wins)))

(deftest test-dominatesp ()
  (check
   (dominatesp 'rock 'scissors)
   (cl:not (dominatesp 'rock 'rock))
   (cl:not (dominatesp 'rock 'paper))
   (dominatesp 'scissors 'paper)
   (cl:not (dominatesp 'scissors 'scissors))
   (cl:not (dominatesp 'scissors 'rock))
   (dominatesp 'paper 'rock)
   (cl:not (dominatesp 'paper 'paper))
   (cl:not (dominatesp 'paper 'scissors))
   (eq (dominatesp 'lizard 'rock) 'illegal-move)))

(deftest test-play ()
  (check
   (eq (play 'rock 'scissors) 'first-wins)
   (eq (play 'rock 'paper) 'second-wins)
   (eq (play 'rock 'rock) 'tie)
   (eq (play 'rock 'lizard) 'illegal-player2)
   (eq (play 'paper 'rock) 'first-wins)
   (eq (play 'paper 'scissors) 'second-wins)
   (eq (play 'paper 'paper) 'tie)
   (eq (play 'paper 'lizard) 'illegal-player2)
   (eq (play 'scissors 'paper) 'first-wins)
   (eq (play 'scissors 'rock) 'second-wins)
   (eq (play 'scissors 'scissors) 'tie)
   (eq (play 'scissors 'lizard) 'illegal-player2)
   (eq (play 'snake 'scissors) 'illegal-player1)
   (eq (play 'snake 'paper) 'illegal-player1)
   (eq (play 'snake 'snake) 'both-illegal)
   (eq (play 'snake 'lizard) 'both-illegal)))

;;;
;;;    4.19
;;;
(defun cond-and (x y z w)
  (cond ((cl:not x) nil)
        ((cl:not y) nil)
        ((cl:not z) nil)
        (t w)))

(deftest test-cond-and ()
  (check
   (cond-and T T T T)
   (notany #'(lambda (l) (apply #'cond-and l))
           '((T T T NIL) (T T NIL T) (T T NIL NIL) (T NIL T T) (T NIL T NIL) (T NIL NIL T) (T NIL NIL NIL) (NIL T T T) (NIL T T NIL) (NIL T NIL T) (NIL T NIL NIL) (NIL NIL T T) (NIL NIL T NIL) (NIL NIL NIL T) (NIL NIL NIL NIL)))) )

(defun cond-if (x y z w)
  (if x
      (if y
          (if z
              w
              nil)
          nil)
      nil))

(deftest test-cond-if ()
  (check
   (cond-if T T T T)
   (notany #'(lambda (l) (apply #'cond-if l))
           '((T T T NIL) (T T NIL T) (T T NIL NIL) (T NIL T T) (T NIL T NIL) (T NIL NIL T) (T NIL NIL NIL) (NIL T T T) (NIL T T NIL) (NIL T NIL T) (NIL T NIL NIL) (NIL NIL T T) (NIL NIL T NIL) (NIL NIL NIL T) (NIL NIL NIL NIL)))) )

;;;
;;;    4.20
;;;
(defun compare (x y)
  (cond ((= x y) 'numbers-are-the-same)
        ((< x y) 'first-is-smaller)
        ((> x y) 'first-is-bigger)))

(defun compare (x y)
  (aref #(first-is-smaller numbers-are-the-same first-is-bigger) (1+ (truncate (signum (- x y)))) ))

(defun compare-if (x y)
  (if (= x y)
      'numbers-are-the-same
      (if (< x y)
          'first-is-smaller
          'first-is-bigger)))

(deftest test-compare-if ()
  (check
   (eq (compare 2 2) (compare-if 2 2))
   (eq (compare 2 3) (compare-if 2 3))
   (eq (compare 3 2) (compare-if 3 2))))

(defun compare-and-or (x y)
  (or (and (= x y) 'numbers-are-the-same)
      (and (< x y) 'first-is-smaller)
      'first-is-bigger))

(deftest test-compare-and-or ()
  (check
   (eq (compare 2 2) (compare-and-or 2 2))
   (eq (compare 2 3) (compare-and-or 2 3))
   (eq (compare 3 2) (compare-and-or 3 2))))

;;;
;;;    4.21
;;;
(defun gtest (x y)
  (or (> x y)
      (zerop x)
      (zerop y)))

(defun gtest (x y)
  (or (> x y) (zerop (* x y))))

(defun gtest-if (x y)
  (if (> x y)
      t
      (if (zerop x)
          t
          (zerop y))))

(deftest test-gtest-if ()
  (check
   (eq (gtest 2 3) (gtest-if 2 3))
   (eq (gtest 3 2) (gtest-if 3 2))
   (eq (gtest 0 3) (gtest-if 0 3))
   (eq (gtest -2 0) (gtest-if -2 0))))

(defun gtest-cond (x y)
  (cond ((> x y))
        ((zerop x))
        ((zerop y))
        (t nil)))

(deftest test-gtest-cond ()
  (check
   (eq (gtest 2 3) (gtest-cond 2 3))
   (eq (gtest 3 2) (gtest-cond 3 2))
   (eq (gtest 0 3) (gtest-cond 0 3))
   (eq (gtest -2 0) (gtest-cond -2 0))))

;;;
;;;    4.22
;;;
(defun boilingp (temp scale)
  (cond ((eq scale 'fahrenheit) (> temp 212))
        ((eq scale 'celsius) (> temp 100))
        ((eq scale 'kelvin) (> temp 373.15))
        (t 'unknown-scale)))

(defun boilingp (temp scale)
  (if (eq scale 'fahrenheit)
      (> temp 212)
      (if (eq scale 'celsius)
          (> temp 100)
          (if (eq scale 'kelvin)
              (> temp 373.15)
              'unknown-scale))))

;; (defun boilingp (temp scale)
;;   (or (and (eq scale 'fahrenheit) (> temp 212))
;;       (and (not (eq scale 'fahrenheit)) (eq scale 'celsius) (> temp 100))
;;       (and (not (eq scale 'fahrenheit)) (not (eq scale 'celsius)) (eq scale 'kelvin) (> temp 373.15))
;;       (and (not (eq scale 'fahrenheit)) (not (eq scale 'celsius)) (not (eq scale 'kelvin)) 'unknown-scale)))

;;;
;;;    Don't need full generality here as above...
;;;    
;;;    This version computes the same function as the COND/IF versions, but it is not as efficient.
;;;    It's possible that multiple redundant tests are evaluated here. Consider (BOILINGP 'FAHRENHEIT 211).
;;;    The first expression in the first AND is true, but the 2nd is false, so the AND returns false. The
;;;    IF and COND versions would be done at this point, however, this version then fails all of the subsequent
;;;    tests of SCALE before OR finally returns false.
;;;    
(defun boilingp (temp scale)
  (or (and (eq scale 'fahrenheit) (> temp 212))
      (and (eq scale 'celsius) (> temp 100))
      (and (eq scale 'kelvin) (> temp 373.15))
      (and (cl:not (eq scale 'fahrenheit)) (cl:not (eq scale 'celsius)) (cl:not (eq scale 'kelvin)) 'unknown-scale)))

(defun boilingp (temp scale)
  (case scale
    (fahrenheit (> temp 212))
    (celsius (> temp 100))
    (kelvin (> temp 373.15))
    (otherwise 'unknown-scale)))

(deftest test-boilingp ()
  (check
   (boilingp 220 'fahrenheit)
   (cl:not (boilingp 212 'fahrenheit))
   (cl:not (boilingp 32 'fahrenheit))
   (boilingp 120 'celsius)
   (cl:not (boilingp 100 'celsius))
   (cl:not (boilingp 0 'celsius))
   (boilingp 380 'kelvin)
   (cl:not (boilingp 373.15 'kelvin))
   (cl:not (boilingp 273.15 'kelvin))
   (eq (boilingp 1000 'binary) 'unknown-scale)))

;;;
;;;    4.29
;;;
(defun logical-and (x y)
  (and x y t))

(defun logical-and (x y)
  (if x
      (if y
          t
          nil)
      nil))

(defun logical-and (x y)
  (cond ((cl:not x) nil)
        ((cl:not y) nil)
        (t t)))
      
(defun logical-and (x y)
  (cond (x (cond (y t)
                 (t nil)))
        (t nil)))

(deftest test-logical-and ()
  (check
   (eq (logical-and (> 3 2) (oddp 7)) t)
   (eq (logical-and 'tweet 'woof) t)
   (eq (logical-and :pung :foo) t)
   (eq (logical-and (numberp 'foo) (= 5 (+ 2 3))) nil)))

;;;
;;;    4.30
;;;
(defun logical-or (x y)
  (or (and x t) (and y t)))

(defun logical-or (x y)
  (if x
      t
      (if y
          t
          nil)))

(defun logical-or (x y)
  (cond (x t)
        (y t)
        (t nil)))

(deftest test-logical-or ()
  (check
   (eq (logical-or (> 3 2) (oddp 7)) t)
   (eq (logical-or 'tweet 'woof) t)
   (eq (logical-or :pung :foo) t)
   (eq (logical-or (numberp 'foo) (= 5 (+ 2 3))) t)
   (eq (logical-or (< 3 2) (evenp 7)) nil)))

;;;
;;;    4.37
;;;    
(defun nand (x y)
  (cl:not (and x y)))

(deftest test-nand ()
  (check
   (eq (nand t t) (cl:not (and t t)))
   (eq (nand t nil) (cl:not (and t nil)))
   (eq (nand nil t) (cl:not (and nil t)))
   (eq (nand nil nil) (cl:not (and nil nil)))) )
   
(defun not (x)
  (nand x x))

(deftest test-not ()
  (check
   (eq (not t) (cl:not t))
   (eq (not nil) (cl:not nil))))

(defun logical-and (x y)
  (nand (nand x y) (nand x y)))

(defun logical-or (x y)
  (nand (nand x x) (nand y y)))

(defun nor (x y)
  (nand (nand (nand x x) (nand y y))
        (nand (nand x x) (nand y y))))

(deftest test-nor ()
  (check
   (eq (nor t t) (cl:not (or t t)))
   (eq (nor t nil) (cl:not (or t nil)))
   (eq (nor nil t) (cl:not (or nil t)))
   (eq (nor nil nil) (cl:not (or nil nil)))) )

;;;
;;;    4.38
;;;    
(defun nor (x y)
  (cl:not (or x y)))

(defun not (x)
  (nor x x))

(defun logical-or (x y)
  (nor (nor x y) (nor x y)))

(defun logical-and (x y)
  (nor (nor x x) (nor y y)))

(defun nand (x y)
  (nor (nor (nor x x) (nor y y))
       (nor (nor x x) (nor y y))))
