;#!/usr/local/bin/clisp

;;
;   NAME:               tour7.lsp
;
;   STARTED:            001127
;   MODIFICATIONS:
;
;   PURPOSE:
;
;
;
;   CALLING SEQUENCE:
;
;
;   INPUTS:
;
;   OUTPUTS:
;
;   EXAMPLE:
;
;   NOTES:
;
;;
(defun roughly-equal (k x)
  (find-if #'(lambda (n)
	       (<= (- k 10) n (+ k 10)))
	   x) )

(defun find-nested (l)
  (find-if #'(lambda (el)
	       (and (listp el) 
		    (not (null el))))
	   l) )

;Touretzky's version...
;Duh!
;(defun find-nested (l)
;  (find-if #'consp l) )

;;;ex 7.11
(defun choose-numbers (l)
  (remove-if #'(lambda (x)
		 (< 1 x 5))
	     l) )

;;;ex 7.12
(defun count-the (sentence)
  (length (remove-if-not #'(lambda (w)
			     (equal w 'the))
			 sentence)) )

;;;ex 7.13
(defun find-pairs (l)
  (remove-if-not #'(lambda (nested-list)
		     (= 2 (length nested-list)))
		 l) )
;;;ex 7.14
(defun my-intersection (a b)
  (remove-if-not #'(lambda (element)
		     (member element b))
		 a) )

#|
; ;;;The above does not reduce lists that are not proper sets, i.e., those that contain
; ;;;multiple elements with the same value. This does:
; (defun my-intersection2 (a b)
;   (remove-if-not #'(lambda (element)
; 		     (and (member element b)
; 			  (not (
; 		 a) )

; ;;;This doesn't work--it removes all copies of duplicates
; (defun make-set (a)
;   (remove-if #'(lambda (element)
; 		 (member element (cdr (member element a))))
; 	     a))
 |#

(defun my-union (a b)
  (append (remove-if #'(lambda (element)
			 (member element b))
		     a)
	  b) )

;;;ex 7.15
(defun rank (card)
  (first card) )

(defun suit (card)
  (second card) )

(defvar *my-hand* '((3 hearts)
		    (5 clubs)
		    (2 diamonds)
		    (4 diamonds)
		    (ace spades)))

(defun count-suit (card-suit hand)
  (length (remove-if-not #'(lambda (card)
		       (equal (suit card) card-suit))
		   hand)) )

(defconstant colors '((clubs black)
		      (diamonds red)
		      (hearts red)
		      (spades black)))

(defun color-of (card)
  (second (assoc (suit card) colors)) )

(defun first-red (hand)
  (find-if #'(lambda (card)
	       (equal (color-of card) 'red))
	   hand) )

(defun black-cards (hand)
  (remove-if-not #'(lambda (card)
		     (equal (color-of card) 'black))
		 hand) )

(defun what-ranks (card-suit hand)
  (mapcar #'first (remove-if-not #'(lambda (card)
				     (equal (suit card) card-suit))
				 hand)) )

(defconstant all-ranks '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun higher-rank-p (card-1 card-2)
  (consp (member (rank card-1) 
		 (member (rank card-2) all-ranks))) )

(defun high-card (hand)
  (assoc (find-if #'(lambda (card)
		      (assoc card hand))
		  (reverse all-ranks))
	 hand) )
