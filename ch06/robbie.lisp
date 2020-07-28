;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               robbie.lisp
;;;
;;;   STARTED:            Sun Jun  2 00:07:06 2002
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
(let ((rooms '((living-room (north front-stairs)
			    (south dining-room)
			    (east kitchen))
	       (upstairs-bedroom (west library) 
				 (south front-stairs))
	       (dining-room (north living-room) 
			    (east pantry)
			    (west downstairs-bedroom))
	       (kitchen (west living-room)
		        (south pantry))
	       (pantry (north kitchen)
		       (west dining-room))
	       (downstairs-bedroom (north back-stairs)
				   (east dining-room))
	       (back-stairs (south downstairs-bedroom)
			    (north library))
	       (front-stairs (north upstairs-bedroom)
			     (south living-room))
	       (library (east upstairs-bedroom)
		        (south back-stairs))))
      (loc 'pantry))

  (defun choices (room)
    (rest (assoc room rooms)))

  (defun look (direction room)
    (second (assoc direction (choices room))))

  (defun set-robbie-location (place)
    (setf loc place))

  (defun how-many-choices ()
    (length (choices loc)))

  (defun upstairsp ()
    (and (member loc '(library upstairs-bedroom)) t))
	 
  (defun onstairsp ()
    (and (member loc '(front-stairs back-stairs)) t))

  (defun where ()
    (append (list 'robbie 'is)
	    (cond ((upstairsp) (list 'upstairs 'in 'the loc))
		  ((onstairsp) (list 'on 'the loc))
		  (t (list 'downstairs 'in 'the loc)))) )

  (defun move (direction)
    (let ((new-loc (look direction loc)))
      (cond (new-loc (set-robbie-location new-loc)
		     (where))
	    (t (list 'ouch! 'robbie 'hit 'a 'wall)))) ))