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

(defun emptyp (board cell)
  (zerop (nth cell board)))

(defun occupied-cells (board)
  (first board))

(defun board-full-p (board)
  (= (occupied-cells board) 9))

(defun convert-to-letter (v)
  (case v
    (1 "O")
    (10 "X")
    (otherwise " ")))

(defun cell-values (board cells)
  (mapcar #'(lambda (i) (nth i board)) cells))

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
  (assert (<= 1 pos 9) (pos) "Invalid board position: ~A" pos)
  (setf (nth pos board) player)
  (incf (first board))
  board)

(defvar *triplets* '((1 2 3) (4 5 6) (7 8 9)
                     (1 4 7) (2 5 8) (3 6 9)
                     (1 5 9) (3 5 7)))

(defun sum-triplet (board triplet)
  (apply #'+ (cell-values board triplet)))

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
    (cond ((not (emptyp board pos))
           (format t "That space is already occupied.~%")
           (read-move board))
          (t pos))))

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
    (if (emptyp board pos)
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
               (emptyp board pos))
           squares))

;;;
;;;    Additional strategies (Keyboard exercise pg. 328)
;;;    10.8
;;;    
(defvar *corners* '(1 3 7 9))
(defvar *sides* '(2 4 6 8))

(defvar *diagonals* '((1 5 9) (3 5 7)))

(defun find-empty-side (board)
  (find-empty-position board *sides*))

(defun find-empty-corner (board)
  (find-empty-position board *corners*))

;; (defun block-squeeze-play (board)
;;   (let ((squeeze (list *opponent* *computer* *opponent*)))
;;     (dolist (diagonal *diagonals* nil)
;;       (let ((diagonal-values (cell-values board diagonal)))
;;         (when (equal diagonal-values squeeze)
;;           (let ((block (find-empty-side board)))
;;             (when block
;;               (return (list block "block squeeze play")))) )))) )

(defun block-squeeze-play (board)
  (labels ((find-squeeze-play (diagonals squeeze)
             (cond ((endp diagonals) nil)
                   ((equal (first diagonals) squeeze) (block-squeeze)) ; Only need to find block for one diagonal. If no move is available then checking other diagonal won't help.
                   (t (find-squeeze-play (rest diagonals) squeeze))))
           (block-squeeze ()
             (let ((block (find-empty-side board)))
               (if block
                   (list block "block squeeze play")
                   nil))))
    (find-squeeze-play (mapcar #'(lambda (diagonal) (cell-values board diagonal)) *diagonals*) 
                       (list *opponent* *computer* *opponent*))))

;;; Refactor using SUM-TRIPLET. No can do...Touretzky's strategy is different.

(deftest test-block-squeeze-play ()
  (check
   (member (first (block-squeeze-play (list 3 1 0 0 0 10 0 0 0 1))) *sides*)
   (member (first (block-squeeze-play (list 3 0 0 1 0 10 0 1 0 0))) *sides*)))

;; (defun block-two-on-one (board)
;;   (let ((two-on-one (list *opponent* *opponent* *computer*)))
;;     (dolist (diagonal *diagonals* nil)
;;       (let ((diagonal-values (cell-values board diagonal)))
;;         (when (or (equal diagonal-values two-on-one)
;;                   (equal diagonal-values (reverse two-on-one)))
;;           (let ((block (find-empty-corner board)))
;;             (when block
;;               (return (list block "block two on one")))) )))) )

(defun block-two-on-one (board)
  (labels ((find-two-on-one (diagonals two-on-one)
             (cond ((endp diagonals) nil)
                   ((or (equal (first diagonals) two-on-one)
                        (equal (first diagonals) (reverse two-on-one)))
                    (block-two-on-one))
                   (t (find-two-on-one (rest diagonals) two-on-one))))
           (block-two-on-one ()
             (let ((block (find-empty-corner board)))
               (if block
                   (list block "block two on one")
                   nil))))
    (find-two-on-one (mapcar #'(lambda (diagonal) (cell-values board diagonal)) *diagonals*) 
                     (list *opponent* *opponent* *computer*))))

(deftest test-block-two-on-one ()
  (check
   (member (first (block-two-on-one (list 3 10 0 0 0 1 0 0 0 1))) *corners*)
   (member (first (block-two-on-one (list 3 1 0 0 0 1 0 0 0 10))) *corners*)
   (member (first (block-two-on-one (list 3 0 0 10 0 1 0 1 0 0))) *corners*)
   (member (first (block-two-on-one (list 3 0 0 1 0 1 0 10 0 0))) *corners*)))

;; (defun attempt-squeeze-play (board)
;;   (cond ((= (occupied-cells board) 2)
;;          (do* ((squeeze (list *computer* *opponent* *empty*))
;;                (diagonal1 (cell-values board (first *diagonals*)))
;;                (diagonal2 (cell-values board (second *diagonals*)))
;;                (squeezes (list squeeze (reverse squeeze) squeeze (reverse squeeze)) (rest squeezes))
;;                (diagonals (list diagonal1 diagonal1 diagonal2 diagonal2) (rest diagonals))
;;                (blocks '(9 1 7 3) (rest blocks)))
;;               ((null squeezes) nil)
;;            (when (equal (first diagonals) (first squeezes))
;;              (return (list (first blocks) "make squeeze play")))) )
;;         (t nil)))

(defun attempt-squeeze-play (board)
  (labels ((find-squeeze-play (diagonals squeeze moves)
             (cond ((endp moves) nil)
                   ((check-diagonal (first diagonals) (list squeeze (reverse squeeze)) (first moves)))
                   (t (find-squeeze-play (rest diagonals) squeeze (rest moves)))) )
           (check-diagonal (diagonal squeezes moves)
             (cond ((endp moves) nil)
                   ((equal diagonal (first squeezes)) (list (first moves) "make squeeze play"))
                   (t (check-diagonal diagonal (rest squeezes) (rest moves)))) ))
    (cond ((= (occupied-cells board) 2)
           (find-squeeze-play (mapcar #'(lambda (diagonal) (cell-values board diagonal)) *diagonals*)
                              (list *computer* *opponent* *empty*)
                              '((9 1) (7 3))))
           (t nil))))

(deftest test-attempt-squeeze-play ()
  (check
   (= (first (attempt-squeeze-play (list 2 10 0 0 0 1 0 0 0 0))) 9)
   (= (first (attempt-squeeze-play (list 2 0 0 0 0 1 0 0 0 10))) 1)
   (= (first (attempt-squeeze-play (list 2 0 0 10 0 1 0 0 0 0))) 7)
   (= (first (attempt-squeeze-play (list 2 0 0 0 0 1 0 10 0 0))) 3)))

;; (defun attempt-two-on-one (board)
;;   (cond ((= (occupied-cells board) 2)
;;          (let ((two-on-one-a (list *opponent* *empty* *computer*))
;;                (two-on-one-b (list *opponent* *computer* *empty*))
;;                (two-on-one-c (list *computer* *empty* *opponent*))
;;                (two-on-one-d (list *empty* *computer* *opponent*))
;;                (diagonal1 (mapcar #'(lambda (i) (nth i board)) '(1 5 9)))
;;                (diagonal2 (mapcar #'(lambda (i) (nth i board)) '(3 5 7))))
;;            (cond ((or (equal diagonal1 two-on-one-a) (equal diagonal1 two-on-one-c)) (list 5 "make two-on-one play"))
;;                  ((or (equal diagonal2 two-on-one-a) (equal diagonal2 two-on-one-c)) (list 5 "make two-on-one play"))
;;                  ((equal diagonal1 two-on-one-b) (list 9 "make two-on-one play"))
;;                  ((equal diagonal1 two-on-one-d) (list 1 "make two-on-one play"))
;;                  ((equal diagonal2 two-on-one-b) (list 7 "make two-on-one play"))
;;                  ((equal diagonal2 two-on-one-d) (list 3 "make two-on-one play"))
;;                  (t nil))))
;;         (t nil)))

(defun attempt-two-on-one (board)
  (labels ((find-two-on-one (diagonals)
             (or (check-center diagonals (list *opponent* *empty* *computer*))
                 (check-corners diagonals (list *opponent* *computer* *empty*) '((9 1) (7 3)))) )
           (check-center (diagonals center-two-on-one)
             (cond ((endp diagonals) nil)
                   ((or (equal (first diagonals) center-two-on-one)
                        (equal (first diagonals) (reverse center-two-on-one)))
                    (list 5 "make two-on-one play"))
                   (t (check-center (rest diagonals) center-two-on-one))))
           (check-corners (diagonals corner-two-on-one moves)
             (cond ((endp moves) nil)
                   ((check-diagonal (first diagonals) (list corner-two-on-one (reverse corner-two-on-one)) (first moves)))
                   (t (check-corners (rest diagonals) corner-two-on-one (rest moves)))) )
           (check-diagonal (diagonal corner-two-on-ones moves)
             (cond ((endp moves) nil)
                   ((equal diagonal (first corner-two-on-ones)) (list (first moves) "make two-on-one play"))
                   (t (check-diagonal diagonal (rest corner-two-on-ones) (rest moves)))) ))
    (cond ((= (occupied-cells board) 2)
           (find-two-on-one (mapcar #'(lambda (diagonal) (cell-values board diagonal)) *diagonals*)))
          (t nil))))

(deftest test-attempt-two-on-one ()
  (check
   (= (first (attempt-two-on-one (list 2 1 0 0 0 10 0 0 0 0))) 9)
   (= (first (attempt-two-on-one (list 2 0 0 0 0 10 0 0 0 1))) 1)
   (= (first (attempt-two-on-one (list 2 0 0 1 0 10 0 0 0 0))) 7)
   (= (first (attempt-two-on-one (list 2 0 0 0 0 10 0 1 0 0))) 3)
   (= (first (attempt-two-on-one (list 2 10 0 0 0 0 0 0 0 1))) 5)
   (= (first (attempt-two-on-one (list 2 1 0 0 0 0 0 0 0 10))) 5)
   (= (first (attempt-two-on-one (list 2 0 0 10 0 0 0 1 0 0))) 5)
   (= (first (attempt-two-on-one (list 2 0 0 1 0 0 0 10 0 0))) 5)))
