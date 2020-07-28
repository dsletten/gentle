;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               local-nerds.lisp
;;;
;;;   STARTED:            Fri May 31 00:56:41 2002
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

(let ((nerd-states '(sleeping eating waiting-for-a-computer programming
			   debugging)))
  (setf (cdr (last nerd-states)) nerd-states)

  (defun next-state ()
    (let ((next-state (car nerd-states)))
      (setf nerd-states (cdr nerd-states))
      next-state))

  (defun sleepless-nerd ()
    (let ((new-state (next-state)))
      (if (eq new-state 'sleeping)
	  (sleepless-nerd)
	  new-state)))

  (defun nerd-on-caffeine ()
    (next-state)
    (next-state)))



    



