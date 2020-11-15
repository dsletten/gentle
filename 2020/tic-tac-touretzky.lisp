;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               tic-tac-touretzky.lisp
;;;;
;;;;   Started:            Tue Nov 10 00:10:46 2020
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

(defpackage :tic-tac-touretzky (:use :common-lisp :test))

(in-package :tic-tac-touretzky)

(defun make-board ()
  (list 'board 0 0 0 0 0 0 0 0 0))

(defun convert-to-letter (v)
  (cond ((equal v 1) "O")
        ((equal v 10) "X")
        (t " ")))

(defun print-row (x y z)
  (format t "~&   ~A | ~A | ~A" (convert-to-letter x) (convert-to-letter y) (convert-to-letter z)))

(defun print-board (board)
  (format t "~%")
  (print-row (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~&  -----------")
  (print-row (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~&  -----------")
  (print-row (nth 7 board) (nth 8 board) (nth 9 board))
  (format t "~%~%"))

(defvar *computer* 10)
(defvar *opponent* 1)

(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

(defvar *triplets* '((1 2 3) (4 5 6) (7 8 9)
                     (1 4 7) (2 5 8) (3 6 9)
                     (1 5 9) (3 5 7)))

(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))

(defun compute-sums (board)
  (mapcar #'(lambda (triplet)
              (sum-triplet board triplet))
          *triplets*))

(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *computer*) sums)
        (member (* 3 *opponent*) sums))))

(defun play-one-game ()
  (if (y-or-n-p "Would you like to go first? ")
      (opponent-move (make-board))
      (computer-move (make-board))))

(defun opponent-move (board)
  (let* ((pos (read-a-legal-move board))
         (new-board (make-move *opponent* pos board)))
    (print-board new-board)
    (cond ((winner-p new-board) (format t "~&You win!"))
          ((board-full-p new-board) (format t "~&Tie game."))
          (t (computer-move new-board)))) )

(defun read-a-legal-move (board)
  (format t "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos) (<= 1 pos 9)))
           (format t "~&Invalid input.")
           (read-a-legal-move board))
          ((not (zerop (nth pos board)))
           (format t "~&That space is already occupied.")
           (read-a-legal-move board))
          (t pos))))

(defun board-full-p (board)
  (not (member 0 board)))

(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
         (pos (first best-move))
         (strategy (second best-move))
         (new-board (make-move *computer* pos board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~A~%" strategy)
    (print-board new-board)
    (cond ((winner-p new-board) (format t "~&I win!"))
          ((board-full-p new-board) (format t "~&Tie game."))
          (t (opponent-move new-board)))) )

;; (defun choose-best-move (board)
;;   (or (make-three-in-a-row board)
;;       (block-opponent-win board)
;;       (random-move-strategy board)))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board) "random move"))

(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
        pos
        (pick-random-empty-position board))))

(defun make-three-in-a-row (board)
  (let ((pos (win-or-block board (* 2 *computer*))))
    (and pos (list pos "make three in a row"))))

(defun block-opponent-win (board)
  (let ((pos (win-or-block board (* 2 *opponent*))))
    (and pos (list pos "block opponent"))))

(defun win-or-block (board target-sum)
  (let ((triplet (find-if #'(lambda (trip)
                              (equal (sum-triplet board trip) target-sum))
                          *triplets*)))
    (when triplet
      (find-empty-position board triplet))))

(defun find-empty-position (board squares)
  (find-if #'(lambda (pos)
               (zerop (nth pos board)))
           squares))

;;;
;;;    Touretzky's additional strategies
;;;    
(defvar *corners* '(1 3 7 9))
(defvar *sides* '(2 4 6 8))

;; (defun block-squeeze-play (board)
;;   (sq-and-2 board *computer* *sides* 12 "block squeeze play"))

;; (defun block-two-on-one (board)
;;   (sq-and-2 board *opponent* *corners* 12 "block two-on-one"))

;; (defun try-squeeze-play (board)
;;   (sq-and-2 board *opponent* nil 11 "set up a squeeze play"))

;; (defun try-two-on-one (board)
;;   (sq-and-2 board *computer* nil 11 "set up a two-on-one"))

;;;
;;;    He refactored this well, but TRY-TWO-ON-ONE is deficient. It
;;;    requires that the center is occupied rather than considering it a square to occupy.
;;;    He painted himself into a corner by reducing triplets to sums.
;;;    He can't distinguish between (10 0 1) and (0 10 1). Consequently, EXPLOIT-TWO-ON-ONE
;;;    is only invoked if: 1. Computer goes first. 2. Initial move is center (to trigger TRY-TWO-ON-ONE).
;;;    (And only then if the opponent makes a dumb move! Otherwise BLOCK-OPPONENT-WIN is called.)
;;;    
(defun block-squeeze-play (board)
  (sq-and-2 board *computer* *sides* (+ *computer* *opponent* *opponent*) "block squeeze play"))

(defun block-two-on-one (board)
  (sq-and-2 board *opponent* *corners* (+ *computer* *opponent* *opponent*) "block two-on-one"))

(defun try-squeeze-play (board)
  (sq-and-2 board *opponent* nil (+ *computer* *opponent*) "set up a squeeze play"))

(deftest test-try-squeeze-play ()
  (check
   (= (first (try-squeeze-play (list 2 10 0 0 0 1 0 0 0 0))) 9)
   (= (first (try-squeeze-play (list 2 0 0 0 0 1 0 0 0 10))) 1)
   (= (first (try-squeeze-play (list 2 0 0 10 0 1 0 0 0 0))) 7)
   (= (first (try-squeeze-play (list 2 0 0 0 0 1 0 10 0 0))) 3)))

(defun try-two-on-one (board)
  (sq-and-2 board *computer* nil (+ *computer* *opponent*) "set up a two-on-one"))

(deftest test-try-two-on-one ()
  (check
   (= (first (try-two-on-one (list 2 1 0 0 0 10 0 0 0 0))) 9)
   (= (first (try-two-on-one (list 2 0 0 0 0 10 0 0 0 1))) 1)
   (= (first (try-two-on-one (list 2 0 0 1 0 10 0 0 0 0))) 7)
   (= (first (try-two-on-one (list 2 0 0 0 0 10 0 1 0 0))) 3)
;   (= (first (try-two-on-one (list 2 10 0 0 0 0 0 0 0 1))) 5) ; These should all succeed
;   (= (first (try-two-on-one (list 2 1 0 0 0 0 0 0 0 10))) 5)
;   (= (first (try-two-on-one (list 2 0 0 10 0 0 0 1 0 0))) 5)
;   (= (first (try-two-on-one (list 2 0 0 1 0 0 0 10 0 0))) 5)
   ))

(defun sq-and-2 (board player pool v strategy)
  (when (equal (nth 5 board) player)
    (or (sq-helper board 1 9 v strategy pool)
        (sq-helper board 3 7 v strategy pool))))

(defun sq-helper (board c1 c2 val strategy pool)
  (when (equal val (sum-triplet board (list c1 5 c2)))
    (let ((pos (find-empty-position board (or pool (list c1 c2)))) )
      (and pos (list pos strategy)))) )

;;;
;;;    Yuck...
;;;    
;; (defun exploit-two-on-one (board)
;;   (when (equal (nth 5 board) *computer*)
;;     (or (exploit-two board 1 2 4 3 7)
;;         (exploit-two board 3 2 6 1 9)
;;         (exploit-two board 7 4 8 1 9)
;;         (exploit-two board 9 6 8 3 7))))

;; (defun exploit-two (board pos d1 d2 c1 c2)
;;   (and (equal (sum-triplet board (list c1 5 c2)) (+ *computer* *computer* *opponent*))
;;        (zerop (nth pos board))
;;        (zerop (nth d1 board))
;;        (zerop (nth d2 board))
;;        (list pos "exploit two-on-one")))

(defun exploit-two-on-one (board)
  (when (equal (nth 5 board) *computer*)
    (or (exploit-two board 1 '(2 4 3 7))
        (exploit-two board 3 '(2 6 1 9))
        (exploit-two board 7 '(4 8 1 9))
        (exploit-two board 9 '(6 8 3 7)))) )

(defun exploit-two (board pos cells)
  (destructuring-bind (d1 d2 c1 c2) cells
    (and (equal (sum-triplet board (list c1 5 c2)) (+ *computer* *computer* *opponent*))
         (zerop (nth pos board))
         (zerop (nth d1 board))
         (zerop (nth d2 board))
         (list pos "exploit two-on-one"))))

(defun choose-best-move (board)
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (block-squeeze-play board)
      (block-two-on-one board)
      (exploit-two-on-one board)
      (try-squeeze-play board)
      (try-two-on-one board)
;(take-center board)
      (random-move-strategy board)))

(defun take-center (board)
  (if (zerop (nth 5 board)) (list 5 "take center") nil))
