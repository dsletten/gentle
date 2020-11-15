;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is the medium of choice for people who enjoy free style and flexibility.
;;;;   -- Gerald Jay Sussman
;;;;
;;;;   Name:               tic-tac-toe.lisp
;;;;
;;;;   Started:            Tue Nov 10 00:11:40 2020
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
;;;;   Notes: The game is not foolproof, but it does well if the computer goes first.
;;;;
;;;;
(load "/home/slytobias/lisp/packages/lang.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :tic-tac-toe (:use :common-lisp :lang :test))

(in-package :tic-tac-toe)

;; (defun make-board ()
;;   (cons 'board (make-list 9 :initial-element 0)))

;;;
;;;    1st element is count of occupied cells.
;;;    
(defun make-board ()
  (make-list 10 :initial-element 0))

(defun convert-to-letter (v)
  (case v
    (1 "O")
    (10 "X")
    (otherwise " ")))

(defun print-row (&rest cells)
  (apply #'format t "   ~A | ~A | ~A~%" (mapcar #'convert-to-letter cells)))

(defun print-board (board)
  (format t "~%")
  (loop for (left middle right) on (rest board) by #'cdddr
        for row from 2 downto 0
        do (print-row left middle right)
        unless (zerop row)
        do (format t "  -----------~%"))
  (format t "~%"))

(defvar *computer* 10)
(defvar *opponent* 1)
(defvar *empty* 0)

(defun make-move (player pos board)
  (setf (nth pos board) player)
  (incf (first board))
  board)

(defvar *triplets* '((1 2 3) (4 5 6) (7 8 9)
                     (1 4 7) (2 5 8) (3 6 9)
                     (1 5 9) (3 5 7)))

(defun sum-triplet (board triplet)
  (apply #'+ (mapcar #'(lambda (i) (nth i board)) triplet)))

(defun compute-sums (board)
  (mapcar #'(lambda (triplet)
              (sum-triplet board triplet))
          *triplets*))

(defun winnerp (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *computer*) sums)
        (member (* 3 *opponent*) sums))))

(defun play-one-game ()
  (if (y-or-n-p "Would you like to go first? ")
      (opponent-move (make-board))
      (computer-move (make-board))))

(defun opponent-move (board)
  (make-move *opponent* (read-move board) board)
  (print-board board)
  (cond ((winnerp board) (format t "You win!~%"))
        ((board-full-p board) (format t "Tie game.~%"))
        (t (computer-move board))))

(defun read-move (board)
  (let ((pos (get-num "Your move: " :test #'(lambda (n) (and (integerp n) (<= 1 n 9)))) ))
    (cond ((not (zerop (nth pos board)))
           (format t "That space is already occupied.~%")
           (read-move board))
          (t pos))))

(defun occupied-cells (board)
  (first board))

(defun board-full-p (board)
  (= (occupied-cells board) 9))

(defun computer-move (board)
  (destructuring-bind (pos strategy) (choose-best-move board)
    (make-move *computer* pos board)
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~A~%" strategy)
    (print-board board)
    (cond ((winnerp board) (format t "~&I win!"))
          ((board-full-p board) (format t "~&Tie game."))
          (t (opponent-move board)))) )

;; (defun choose-best-move (board)
;;   (list 9 'duh))

(defun choose-best-move (board)
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (attempt-squeeze-play board)
      (attempt-two-on-one board)
      (block-squeeze-play board)
      (block-two-on-one board)
      (random-move-strategy board)))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board) "random move"))

(defun pick-random-empty-position (board)
  (let ((pos (1+ (random 9))))
    (if (zerop (nth pos board))
        pos
        (pick-random-empty-position board))))

(defun make-three-in-a-row (board)
  (let ((pos (win-or-block board (* 2 *computer*))))
    (if pos
        (list pos "make three in a row")
        nil)))

(defun block-opponent-win (board)
  (let ((pos (win-or-block board (* 2 *opponent*))))
    (if pos
        (list pos "block opponent")
        nil)))

(defun win-or-block (board target-sum)
  (let ((triplet (find-if #'(lambda (trip)
                              (equal (sum-triplet board trip) target-sum))
                          *triplets*)))
    (if triplet
      (find-empty-position board triplet)
      nil)))

(defun find-empty-position (board squares)
  (find-if #'(lambda (pos)
               (zerop (nth pos board)))
           squares))

;;;
;;;    Additional strategies (Keyboard exercise pg. 328)
;;;    10.8
;;;    
(defvar *corners* '(1 3 7 9))
(defvar *sides* '(2 4 6 8))

(defun block-squeeze-play (board)
  (let ((squeeze (list *opponent* *computer* *opponent*))
        (diagonal1 (mapcar #'(lambda (i) (nth i board)) '(1 5 9)))
        (diagonal2 (mapcar #'(lambda (i) (nth i board)) '(3 5 7))))
    (if (or (equal squeeze diagonal1)
            (equal squeeze diagonal2))
        (let ((block (find-if #'(lambda (i) (zerop (nth i board))) *sides*)))
          (if block
              (list block "block squeeze play")
              nil))
        nil)))

(defun block-two-on-one (board)
  (let ((two-on-one-a (list *opponent* *opponent* *computer*))
        (two-on-one-b (list *computer* *opponent* *opponent*))
        (diagonal1 (mapcar #'(lambda (i) (nth i board)) '(1 5 9)))
        (diagonal2 (mapcar #'(lambda (i) (nth i board)) '(3 5 7))))
    (if (or (equal diagonal1 two-on-one-a)
            (equal diagonal1 two-on-one-b)
            (equal diagonal2 two-on-one-a)
            (equal diagonal2 two-on-one-b))
        (let ((block (find-if #'(lambda (i) (zerop (nth i board))) *corners*)))
          (if block
              (list block "block two on one")
              nil))
        nil)))

(defun attempt-squeeze-play (board)
  (cond ((= (occupied-cells board) 2)
         (let ((squeeze-a (list *computer* *opponent* *empty*))
               (squeeze-b (list *empty* *opponent* *computer*))
               (diagonal1 (mapcar #'(lambda (i) (nth i board)) '(1 5 9)))
               (diagonal2 (mapcar #'(lambda (i) (nth i board)) '(3 5 7))))
           (cond ((equal diagonal1 squeeze-a) (list 9 "make squeeze play"))
                 ((equal diagonal2 squeeze-a) (list 7 "make squeeze play"))
                 ((equal diagonal1 squeeze-b) (list 1 "make squeeze play"))
                 ((equal diagonal2 squeeze-b) (list 3 "make squeeze play"))
                 (t nil))))
        (t nil)))

(defun attempt-two-on-one (board)
  (cond ((= (occupied-cells board) 2)
         (let ((two-on-one-a (list *opponent* *empty* *computer*))
               (two-on-one-b (list *opponent* *computer* *empty*))
               (two-on-one-c (list *computer* *empty* *opponent*))
               (two-on-one-d (list *empty* *computer* *opponent*))
               (diagonal1 (mapcar #'(lambda (i) (nth i board)) '(1 5 9)))
               (diagonal2 (mapcar #'(lambda (i) (nth i board)) '(3 5 7))))
           (cond ((or (equal diagonal1 two-on-one-a) (equal diagonal1 two-on-one-c)) (list 5 "make two-on-one play"))
                 ((or (equal diagonal2 two-on-one-a) (equal diagonal2 two-on-one-c)) (list 5 "make two-on-one play"))
                 ((equal diagonal1 two-on-one-b) (list 9 "make two-on-one play"))
                 ((equal diagonal1 two-on-one-d) (list 1 "make two-on-one play"))
                 ((equal diagonal2 two-on-one-b) (list 7 "make two-on-one play"))
                 ((equal diagonal2 two-on-one-d) (list 3 "make two-on-one play"))
                 (t nil))))
        (t nil)))
