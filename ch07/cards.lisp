;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               cards.lisp
;;;
;;;   STARTED:            Wed Jun  5 00:53:30 2002
;;;   MODIFICATIONS:
;;;
;;;   PURPOSE:
;;;
;;;
;;;
;;;   CALLING SEQUENCE:
;;;
;;;
;;;   INPUTS:
;;;
;;;   OUTPUTS:
;;;
;;;   EXAMPLE:
;;;
;;;   NOTES:
;;;
;;;

(defvar *my-hand* '((3 hearts)
		    (5 clubs)
		    (2 diamonds)
		    (4 diamonds)
		    (ace spades)))

(let ((colors '((clubs black)
		(diamonds red)
		(hearts red)
		(spades black)))
      (all-ranks '(2 3 4 5 6 7 8 9 10 jack queen king ace)))

  (defun rank (card)
    (first card))

  (defun suit (card)
    (second card))

; (defun count-suit (suit hand)
;   (length (remove-if-not #'(lambda (card)
; 			     (eq (suit card) suit))
; 			 hand)))

  (defun count-suit (suit hand)
    (count suit hand :key #'cadr))

  (defun color-of (card)
    (second (assoc (suit card) colors)))

;   (defun first-red (hand)
;     (first (remove-if-not #'(lambda (card)
; 			      (eq (color-of card) 'red))
; 			  hand)))
  
;   (defun first-red (hand)
;     (cond ((null hand) nil)
; 	  ((eq (color-of (car hand)) 'red) (car hand))
; 	  (t (first-red (cdr hand)))) )

  (defun first-red (hand)
    (find-if #'(lambda (card)
		 (eq (color-of card) 'red))
	     hand))

  (defun black-cards (hand)
    (remove-if-not #'(lambda (card)
		       (eq (color-of card) 'black))
		   hand))

  (defun what-ranks (suit hand)
    ;;RANK not CAR below!!
    (mapcar #'rank (remove-if-not #'(lambda (card)
				     (eq (suit card) suit))
				 hand)))

  (defun higher-rank-p (card1 card2)
    (> (position (rank card1) all-ranks)
       (position (rank card2) all-ranks)))

;   (defun higher-rank-p (card1 card2)
;     (and (member (rank card1) (member (rank card2) all-ranks)) t))
  
  (defun order-hand (hand)
    (sort (copy-list hand) #'higher-rank-p))

  (defun high-card (hand)
    (first (order-hand hand)))

;   (defun high-card (hand)
;     (assoc (find-if #'(lambda (rank)
; 			(assoc rank hand))
; 		    (reverse all-ranks))
; 	   hand))
  )
