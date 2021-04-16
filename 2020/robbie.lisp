;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   LISP has been jokingly described as "the most intelligent way to misuse a computer".
;;;;   -- Edsger W. Dijkstra
;;;;
;;;;   Name:               robbie.lisp
;;;;
;;;;   Started:            Tue Sep 15 01:50:49 2020
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;       Touretzky ch. 6 (pg. 188) Robbie the Robot
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

(defpackage :robbie (:use :common-lisp :test))

(in-package :robbie)

;;;
;;;    I. Map encoded as FSM (?) in local functions.
;;;    
(defmacro dispatch (location (&rest destinations) otherwise)
  `(case ,location
     ,@(loop for destination in destinations collect (list destination (list (find-symbol (symbol-name destination)))) )
     (otherwise ,otherwise)))
    ;; (case current
    ;;   (:library (library))
    ;;   (:back-stairs (back-stairs))
    ;;   (:upstairs-bedroom (upstairs-bedroom))
    ;;   (:front-stairs (front-stairs))
    ;;   (:living-room (living-room))
    ;;   (:kitchen (kitchen))
    ;;   (:dining-room (dining-room))
    ;;   (:downstairs-bedroom (downstairs-bedroom))
    ;;   (:pantry (pantry))
    ;;   (otherwise (warn "Where are you Robbie?!")))) )

(defun robbie-fsm (current direction)
  (labels ((library ()
             (case direction
               (:south :back-stairs)
               (:east :upstairs-bedroom)
               (otherwise (illegal-move))))
           (back-stairs ()
             (case direction
               (:north :library)
               (:south :downstairs-bedroom)
               (otherwise (illegal-move))))
           (downstairs-bedroom ()
             (case direction
               (:north :back-stairs)
               (:east :dining-room)
               (otherwise (illegal-move))))
           (upstairs-bedroom ()
             (case direction
               (:west :library)
               (:south :front-stairs)
               (otherwise (illegal-move))))
           (front-stairs ()
             (case direction
               (:north :upstairs-bedroom)
               (:south :living-room)
               (otherwise (illegal-move))))
           (living-room ()
             (case direction
               (:north :front-stairs)
               (:east :kitchen)
               (:south :dining-room)
               (otherwise (illegal-move))))
           (dining-room ()
             (case direction
               (:north :living-room)
               (:east :pantry)
               (:west :downstairs-bedroom)
               (otherwise (illegal-move))))
           (kitchen ()
             (case direction
               (:west :living-room)
               (:south :pantry)
               (otherwise (illegal-move))))
           (pantry ()
             (case direction
               (:north :kitchen)
               (:west :dining-room)
               (otherwise (illegal-move))))
           (illegal-move ()
             (warn "Robbie cannot move ~A from ~A." direction current)))
    (dispatch current
      (:library :back-stairs :upstairs-bedroom :front-stairs :living-room :kitchen :dining-room :downstairs-bedroom :pantry)
      (warn "Where are you Robbie?!"))))

(defun move-robbie (start moves)
  (cond ((endp moves) start)
        (t (move-robbie (robbie-fsm start (first moves)) (rest moves)))) )

;;;
;;;   II. Map encoded as rules (See PAIP ch. 2)
;;;   
(defparameter *house-moves* '((library -> (south back-stairs) (east upstairs-bedroom))
                              (back-stairs -> (north library) (south downstairs-bedroom))
                              (downstairs-bedroom -> (north back-stairs) (east dining-room))
                              (upstairs-bedroom -> (west library) (south front-stairs))
                              (front-stairs -> (north upstairs-bedroom) (south living-room))
                              (living-room -> (north front-stairs) (east kitchen) (south dining-room))
                              (dining-room -> (north living-room) (east pantry) (west downstairs-bedroom))
                              (kitchen -> (west living-room) (south pantry))
                              (pantry -> (north kitchen) (west dining-room))))

(defun retrieve-rule (location rules)
  (assoc location rules))

(defun move-options (rule)
  (destructuring-bind (location arrow . options) rule
    (declare (ignore location arrow))
    options))

(defun options (location rules)
  (let ((rule (retrieve-rule location rules)))
    (if (null rule)
        '()
        (move-options rule))))

(defun move-robbie (location rules moves)
  "Move Robbie from LOCATION according to MOVES along a path defined by RULES."
  (cond ((endp moves) location)
        (t (destructuring-bind (move . more) moves
             (let ((options (options location rules)))
               (if (null options)
                   (warn "Where are you Robbie?!")
                   (let ((option (assoc move options)))
                     (if (null option)
                         (warn "Robbie cannot move ~A from ~A." move location)
                         (move-robbie (second option) rules more)))) )))) )

;;;
;;;   Look for cycles??
;;;   
;; (defun find-path (start finish rules)
;;   (labels ((find-path-aux (location moves)
;;              (if (eq location finish)
;;                  (nreverse moves)
;;                  (let* ((options (options location rules))
;;                         (next-move (find-forward-move (first moves) options)))
;;                    (find-path-aux (second (assoc next-move options)) (cons next-move moves)))) ))
;;     (if (eq start finish)
;;         '()
;;         (let ((move (random-move (options start rules))))
;;           (trace-path start 
;;                       (find-path-aux (second move) (list (first move)))
;;                       rules)))) )

(defun random-move (moves)
  (elt moves (random (length moves))))

(defun random-walk (count start moves)
  (loop repeat count
        for location = start then new-location
        for (direction new-location) = (random-move (options start moves)) then (random-move (options location moves))
        do (format t "Robbie's location: ~A heading ~A~%" location direction)
        finally (format t "Robbie stops: ~A~%" new-location)))

(defun find-forward-move (previous-move options)
  (let ((forward-moves (remove-if #'(lambda (option) (eq (first option) (opposite previous-move))) options)))
    (first (random-move forward-moves))))

(defun opposite (direction)
  (ecase direction
    (north 'south)
    (south 'north)
    (east 'west)
    (west 'east)))

;; ??? Path is sequence of locations
;; ??? Moves are the directions that are followed...

(defun find-path (start finish rules)
  (labels ((find-path-aux (location moves path)
             (if (eq location finish)
                 (nreverse moves)
                 (let* ((options (options location rules))
                        (new-path (cons location path))
                        (next-move (find-acyclic-move new-path options)))
                   (if (null next-move)
                       (nreverse moves)
                       (find-path-aux (second (assoc next-move options)) (cons next-move moves) new-path)))) ))
    (if (eq start finish)
        '()
        (destructuring-bind (direction destination) (random-move (options start rules))
          (trace-path start (find-path-aux destination (list direction) (list start)) rules)))) )

;; (defun find-path (start finish rules)
;;   (let ((path (list start)))
;;     (labels ((find-path-aux (location moves)
;;                (push location path)
;;                (if (eq location finish)
;;                    (nreverse moves)
;;                    (let* ((options (options location rules))
;;                           (next-move (find-acyclic-move path options)))
;;                      (if (null next-move)
;;                          (nreverse moves)
;;                          (find-path-aux (second (assoc next-move options)) (cons next-move moves)))) )))
;;       (if (eq start finish)
;;           '()
;;           (let ((move (random-move (options start rules))))
;;             (trace-path start 
;;                         (find-path-aux (second move) (list (first move)))
;;                         rules)))) ))

(defun find-acyclic-move (path options)
  (let ((acyclic-moves (remove-if #'(lambda (option) (member (second option) path)) options)))
    (if (null acyclic-moves)
        (format t "Robbie got stuck: ~A~%" path)
        (first (random-move acyclic-moves)))) )

(defun execute-move (location direction rules)
  (let ((options (options location rules)))
    (second (assoc direction options))))

(defun trace-path (start path rules)
  (cons start (loop for move in path
                    for location = (execute-move start move rules) then (execute-move location move rules)
                    collect '-->
                    collect location)))

;;;
;;;    III. Touretzky's way...
;;;
(defvar *rooms* '((living-room        (north front-stairs) 
                                      (east kitchen)
                                      (south dining-room))
                  (upstairs-bedroom   (west library)
                                      (south front-stairs))
                  (dining-room        (north living-room)
                                      (east pantry)
                                      (west downstairs-bedroom))
                  (kitchen            (west living-room)
                                      (south pantry))
                  (pantry             (north kitchen)
                                      (west dining-room))
                  (downstairs-bedroom (north back-stairs) 
                                      (east dining-room))
                  (back-stairs        (north library)
                                      (south downstairs-bedroom))
                  (front-stairs       (north upstairs-bedroom)
                                      (south living-room))
                  (library            (south back-stairs)
                                      (east upstairs-bedroom))))

(defun choices (room)
  (rest (assoc room *rooms*)))

(defun look (direction room)
  (second (assoc direction (choices room))))

(defvar *location* 'pantry)

(defun set-robbie-location (place)
  (setf *location* place))

(defun how-many-choices ()
  (length (choices *location*)))

(defun upstairsp (location)
  (member location '(library upstairs-bedroom)))

(defun onstairsp (location)
  (member location '(front-stairs back-stairs)))

(defun where ()
  (cond ((onstairsp *location*) (format t "Robbie is on the ~A.~%" (string-downcase (symbol-name *location*))))
        ((upstairsp *location*) (format t "Robbie is upstairs in the ~A.~%" (string-downcase (symbol-name *location*))))
        (t (format t "Robbie is downstairs in the ~A.~%" (string-downcase (symbol-name *location*)))) ))

(defun move (direction)
  (let ((location (look direction *location*)))
    (if (null location)
        (format t "Ouch! Robbie hit a wall.~%")
        (progn
          (set-robbie-location location)
          (where)))) )

