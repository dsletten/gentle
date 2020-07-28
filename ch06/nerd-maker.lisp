;#!/usr/local/bin/clisp
;;;    Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;
;;;   NAME:               nerd-maker.lisp
;;;
;;;   STARTED:            Fri May 31 01:06:48 2002
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
(defstruct nerd
  (state (make-nerd-state)))

(defun make-nerd-state ()
  (let ((nerd-states (list 'sleeping 'eating 'waiting-for-a-computer
			   'programming 'debugging)))
    (setf (cdr (last nerd-states)) nerd-states)
    nerd-states))

(defun next-state (nerd)
  (let* ((state-list (nerd-state nerd))
	 (next-state (car state-list)))
    (setf (nerd-state nerd) (cdr state-list))
    next-state))

(defun sleepless-nerd (nerd)
  (let ((new-state (next-state nerd)))
    (if (eq new-state 'sleeping)
	(sleepless-nerd nerd)
	new-state)))

(defun nerd-on-caffeine (nerd)
  (next-state nerd)
  (next-state nerd))

; (defmacro ne (state-list)
;   `(prog1 (car ,state-list)
;           (setf ,state-list (cdr ,state-list))))

; ; (defun sleepless-nerd (state-list)
; ;   (let ((new-state (nerdus state-list)))
; ;     (if (eq new-state 'sleeping)
; ; 	(sleepless-nerd)
; ; 	new-state)))

; (defmacro sleepless-nerd (state-list)
;   `(let ((new-state (nerdus ,state-list)))
;      (if (eq new-state 'sleeping)
; 	 (sleepless-nerd ,state-list)
; 	 new-state)))

; (defmacro nerd-on-caffeine (state-list)
;   `(nerdus ,state-list)
;   `(nerdus ,state-list))