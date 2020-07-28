;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               tic-tac-toe.lisp
;;;
;;;   STARTED:            Sun Jul 14 23:10:29 2002
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

;;;
;;;    Set up
;;;    
(defvar *computer*)
(defvar *opponent*)

(setf *computer* 10)
(setf *opponent* 1)

(defvar *triplets* '((1 2 3) (4 5 6) (7 8 9)
		     (1 4 7) (2 5 8) (3 6 9)
		     (1 5 9) (3 5 7)))

(defvar *corners* '(1 3 7 9))
(defvar *sides* '(2 4 6 8))

(defun make-board ()
  (cons 'board (make-list 9 :initial-element 0)))

;;;
;;;    Display
;;;    
(defun convert-to-letter (v)
  (case v
    (1 "O")
    (10 "X")
    (t " ")))

(defun print-row (x y z)
  (format t "~&   ~A | ~A | ~A"
	  (convert-to-letter x)
	  (convert-to-letter y)
	  (convert-to-letter z)))

(defun print-board (board)
  (labels ((left-cell (l)
	     (format t " ~A |" (convert-to-letter (car l)))
	     (middle-cell (cdr l)))
	   (middle-cell (l)
	     (format t " ~A |" (convert-to-letter (car l)))
	     (right-cell (cdr l)))
	   (right-cell (l)
	     (format t " ~A~%" (convert-to-letter (car l)))
	     (when (cdr l)
	       (format t "------------~%")
	       (left-cell (cdr l)))) )
    (left-cell (cdr board))))

;;;
;;;    Play game
;;;    
(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))

(defun compute-sums (board)
  (mapcar #'(lambda (triplet)
	      (sum-triplet board triplet))
	  *triplets*))

(defun winnerp (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *computer*) sums)
	(member (* 3 *opponent*) sums))))

(defun play-game ()
  (if (y-or-n-p "Would you like to go first? ")
      (opponent-move (make-board))
      (computer-move (make-board))))

(defun opponent-move (board)
  (let* ((pos (read-legal-move board))
	 (new-board (make-move *opponent* pos board)))
    (print-board new-board)
    (cond ((winnerp new-board) (format t "~&You win!"))
	  ((board-full-p new-board) (format t "~&Tie game."))
	  (t (computer-move new-board)))) )

(defun read-legal-move (board)
  (format t "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
		     (<= 1 pos 9)))
	   (format t "~&Invalid input.")
	   (read-legal-move board))
	  ((not (zerop (nth pos board)))
	   (format t "~&That space is already occupied.")
	   (read-legal-move board))
	  (t pos))))

(defun board-full-p (board)
  (notany #'zerop (cdr board)))

(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
	 (pos (first best-move))
	 (strategy (second best-move))
	 (new-board (make-move *computer* pos board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~A~%" strategy)
    (print-board new-board)
    (cond ((winnerp new-board) (format t "~&I win!"))
	  ((board-full-p new-board) (format t "~&Tie game."))
	  (t (opponent-move new-board)))) )

(defun choose-best-move (board)
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (when (move-two-p board)
	(or (block-squeeze-play board)
	    (block-two-on-one board)))
      (take-center board)
      (random-move-strategy board)))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board)
	"random move"))

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
			      (= (sum-triplet board trip)
				 target-sum))
			  *triplets*)))
    (when triplet
      (find-empty-position board triplet))))

(defun find-empty-position (board squares)
  (find-if #'(lambda (pos)
	       (zerop (nth pos board)))
	   squares))

(defun block-squeeze-play (board)
  (cond ((member (list *opponent* *computer* *opponent*) (get-diagonals board)
		 :test #'equal)
	 (let ((pos (find-empty-position board *sides*)))
	   (if pos
	       (list pos "block squeeze play")
	       nil)))
	(t nil)))

(defun get-diagonals (board)
  (list (mapcar #'(lambda (x) (nth x board)) (nth 6 *triplets*))
	(mapcar #'(lambda (x) (nth x board)) (nth 7 *triplets*))))

(defun take-center (board)
  (if (zerop (nth 5 board))
      (list 5 "take center position")
      nil))

(defun block-two-on-one (board)
  (let ((diagonals (get-diagonals board)))
    (cond ((or (member (list *opponent* *opponent* *computer*)
		       diagonals :test #'equal)
	       (member (list *computer* *opponent* *opponent*)
		       diagonals :test #'equal))
	   (let ((pos (find-empty-position board *corners*)))
	     (if pos
		 (list pos "block two-on-one")
		 nil)))
	  (t nil))))

(defun move-two-p (board)
  "Has opponent made exactly two moves?"
  (= (count-if (complement #'zerop) (cdr board)) 3))

; (defun block-l-strategy (board)
  
;   )

; (defun block-2-sides (board)
;   )
