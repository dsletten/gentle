;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               nerds.lisp
;;;
;;;   STARTED:            Fri May 31 00:39:30 2002
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

(defvar *nerd-states* '(sleeping eating waiting-for-a-computer programming
		      debugging))
(setf (cdr (last *nerd-states*)) *nerd-states*)

(defmacro nerdus (state-list)
  `(prog1 (car ,state-list)
          (setf ,state-list (cdr ,state-list))))

; (defun sleepless-nerd (state-list)
;   (let ((new-state (nerdus state-list)))
;     (if (eq new-state 'sleeping)
; 	(sleepless-nerd)
; 	new-state)))

(defmacro sleepless-nerd (state-list)
  `(let ((new-state (nerdus ,state-list)))
     (if (eq new-state 'sleeping)
	 (sleepless-nerd ,state-list)
	 new-state)))

(defmacro nerd-on-caffeine (state-list)
  `(nerdus ,state-list)
  `(nerdus ,state-list))