;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   LISP has been jokingly described as "the most intelligent way to misuse a computer".
;;;;   -- Edsger W. Dijkstra
;;;;
;;;;   Name:               cards.lisp
;;;;
;;;;   Started:            Tue Sep 29 02:57:06 2020
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

(defpackage :cards (:use :common-lisp :test))

(in-package :cards)

(defvar *ranks* '(2 3 4 5 6 7 8 9 10 jack queen king ace))
(defvar *suits* '(clubs diamonds hearts spades))

(defun random-hand (&optional (count (1+ (random 7))))
  (let ((ranks (length *ranks*))
        (suits (length *suits*)))
    (loop repeat count
          collect (list (nth (random ranks) *ranks*) (nth (random suits) *suits*)))) )

(defvar *hand* '((3 hearts)
                 (5 clubs)
                 (2 diamonds)
                 (4 diamonds)
                 (ace spades)))

(defvar *colors* '((clubs black)
                   (diamonds red)
                   (hearts red)
                   (spades black)))

(defun rank (card)
  (first card))

(deftest test-rank ()
  (check
   (eq (rank (first *hand*)) 3)
   (eq (rank (second *hand*)) 5)
   (eq (rank (third *hand*)) 2)
   (eq (rank (fourth *hand*)) 4)
   (eq (rank (fifth *hand*)) 'ace)))

(defun suit (card)
  (second card))

(deftest test-suit ()
  (check
   (eq (suit (first *hand*)) 'hearts)
   (eq (suit (second *hand*)) 'clubs)
   (eq (suit (third *hand*)) 'diamonds)
   (eq (suit (fourth *hand*)) 'diamonds)
   (eq (suit (fifth *hand*)) 'spades)))

(defun count-suit (suit hand)
  (count suit hand :key #'suit))

(deftest test-count-suit ()
  (check
   (= (count-suit 'clubs '((2 clubs) (3 diamonds) (4 clubs))) 2)
   (= (count-suit 'spades '((2 clubs) (3 diamonds) (4 clubs))) 0)))

(defun color-of (card)
  (second (assoc (suit card) *colors*)))

(deftest test-color-of ()
  (check
   (eq (color-of '(2 clubs)) 'black)
   (eq (color-of '(6 hearts)) 'red)))

(defun redp (card)
  (eq (color-of card) 'red))

(defun blackp (card)
  (eq (color-of card) 'black))

(defun first-red (hand)
  (find-if #'(lambda (card) (eq (color-of card) 'red)) hand))

(defun first-red (hand)
  (find-if #'redp hand))

(deftest test-first-red ()
  (check
   (equal (first-red '((ace clubs) #1=(king diamonds) (queen hearts))) '#1#)
   (equal (first-red '(#2=(3 hearts) (4 hearts) (5 diamonds))) '#2#)
   (null (first-red '((7 spades) (8 clubs) (9 clubs) (jack spades)))) ))

(defun black-cards (hand)
  (remove-if-not #'(lambda (card) (eq (color-of card) 'black)) hand))

(defun black-cards (hand)
  (remove-if-not #'blackp hand))

(deftest test-black-cards ()
  (check
   (equal (black-cards '((8 DIAMONDS) (KING SPADES) (9 HEARTS) (9 CLUBS) (4 DIAMONDS) (5 CLUBS) (ACE DIAMONDS))) '((KING SPADES) (9 CLUBS) (5 CLUBS)))
   (equal (black-cards '((JACK DIAMONDS) (8 CLUBS) (4 DIAMONDS))) '((8 CLUBS)))
   (equal (black-cards '((2 DIAMONDS) (7 DIAMONDS) (2 CLUBS) (5 HEARTS) (QUEEN SPADES) (3 CLUBS) (ACE HEARTS))) '((2 CLUBS) (QUEEN SPADES) (3 CLUBS)))
   (equal (black-cards '((7 DIAMONDS) (4 SPADES) (3 HEARTS) (2 CLUBS))) '((4 SPADES) (2 CLUBS)))
   (equal (black-cards '((ACE DIAMONDS) (5 HEARTS) (2 DIAMONDS) (10 SPADES))) '((10 SPADES)))
   (equal (black-cards '((KING CLUBS) (KING DIAMONDS) (8 SPADES))) '((KING CLUBS) (8 SPADES)))))

(defun what-ranks (suit hand)
  (mapcar #'rank (remove-if-not #'(lambda (card) (eq (suit card) suit)) hand)))

(deftest test-what-ranks ()
  (check
   (equal (what-ranks 'diamonds '((8 DIAMONDS) (10 CLUBS) (6 DIAMONDS) (10 CLUBS) (6 SPADES) (JACK SPADES) (10 DIAMONDS))) '(8 6 10))
   (equal (what-ranks 'spades '((KING DIAMONDS) (5 DIAMONDS) (10 HEARTS) (9 HEARTS) (QUEEN CLUBS) (JACK SPADES) (2 SPADES))) '(jack 2))
   (equal (what-ranks 'clubs '((7 SPADES) (QUEEN SPADES) (4 CLUBS) (QUEEN DIAMONDS) (5 HEARTS) (9 DIAMONDS) (10 SPADES))) '(4))
   (equal (what-ranks 'hearts '((QUEEN HEARTS) (8 SPADES) (7 SPADES) (5 HEARTS) (10 SPADES) (3 SPADES) (9 CLUBS))) '(queen 5))))

(defun higher-rank-p (card1 card2)
  (member (rank card1) (rest (member (rank card2) *ranks*))))

(deftest test-higher-rank-p ()
  (check
   (not (higher-rank-p '(KING CLUBS) '(KING DIAMONDS)))
   (not (higher-rank-p '(KING CLUBS) '(ACE DIAMONDS)))
   (higher-rank-p '(ACE DIAMONDS) '(KING CLUBS))
   (not (higher-rank-p '(3 DIAMONDS) '(QUEEN HEARTS)))
   (not (higher-rank-p '(6 SPADES) '(QUEEN CLUBS)))
   (not (higher-rank-p '(3 SPADES) '(8 DIAMONDS)))
   (higher-rank-p '(8 CLUBS) '(5 CLUBS))
   (not (higher-rank-p '(2 CLUBS) '(5 SPADES)))) )

(defun high-card (hand)
  (labels ((highest (card hand)
             (cond ((endp hand) card)
                   ((higher-rank-p (first hand) card) (highest (first hand) (rest hand)))
                   (t (highest card (rest hand)))) ))
    (highest (first hand) (rest hand))))

;;;
;;;    Ugly...but recursion above not yet covered...
;;;    
(defun high-card (hand)
  (let ((highest (first hand)))
    (mapcar #'(lambda (card) (when (higher-rank-p card highest) (setf highest card))) (rest hand))
    highest))

;;;
;;;    Touretzky's weird suggestion...
;;;    
(defun high-card (hand)
  (let ((ranks (mapcar #'rank hand)))
    (assoc (find-if #'(lambda (rank) (member rank ranks)) (reverse *ranks*)) hand)))

;;;
;;;    Touretzky's better suggestion...
;;;    
(defun high-card (hand)
  (reduce #'(lambda (high card) (if (higher-rank-p card high) card high)) hand))

(deftest test-high-card ()
  (check
   (equal (high-card '((2 SPADES))) '(2 spades))
   (equal (high-card '((2 SPADES) #1=(ACE SPADES) (8 CLUBS) (QUEEN DIAMONDS) (JACK CLUBS) (5 HEARTS) (6 SPADES))) '#1#)
   (equal (high-card '((5 DIAMONDS) (4 HEARTS) #2=(ACE CLUBS) (7 SPADES) (8 CLUBS) (4 DIAMONDS) (6 DIAMONDS))) '#2#)
   (equal (high-card '((5 CLUBS) (3 SPADES) (6 SPADES) (6 DIAMONDS) #3=(QUEEN CLUBS) (JACK SPADES) (5 DIAMONDS))) '#3#)
   (equal (high-card '((4 CLUBS) (9 SPADES) (6 SPADES) #4=(QUEEN CLUBS) (QUEEN DIAMONDS) (2 DIAMONDS) (9 SPADES))) '#4#)
   (equal (high-card '((2 CLUBS) (3 HEARTS) (5 SPADES) (5 SPADES) #5=(9 SPADES) (3 SPADES) (9 HEARTS))) '#5#)
   (equal (high-card '((4 DIAMONDS) (10 HEARTS) (4 DIAMONDS) (10 DIAMONDS) (JACK SPADES) (8 DIAMONDS) #6=(QUEEN DIAMONDS))) '#6#)
   (equal (high-card '(#7=(KING HEARTS) (10 HEARTS) (6 DIAMONDS) (8 HEARTS) (5 DIAMONDS) (JACK SPADES) (JACK SPADES))) '#7#)))

