;#!/usr/local/bin/clisp

;;
;   NAME:               nerds2.lsp
;
;   STARTED:            010119
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
(setf nerd-states '((sleeping eating)
		    (eating waiting-for-a-computer)
		    (waiting-for-a-computer programming)
		    (programming debugging)
		    (debugging sleeping)))

(defun nerdus (state)
  (second (assoc state nerd-states)) )

(defun sleepless-nerd (state)
  (cond ((equal state 'debugging) (nerdus (nerdus state)))
	((equal state 'sleeping) nil)
	(t (nerdus state))) )

(defun nerd-on-caffeine (state)
  (nerdus (nerdus state)) )
