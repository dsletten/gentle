;#!/usr/local/bin/clisp

;;
;   NAME:               tour6.lsp
;
;   STARTED:            001119
;   MODIFICATIONS:
;
;   PURPOSE:
;   Touretzky Ch. 6
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
(defun swap-first-last (l)
  (let ((my-first (car l))
	(my-last (car (last l))))
    (cons my-last (reverse (cons my-first (cdr (reverse (cdr l))))))) )

(defun rotate-left (l)
  (append (cdr l) (list (car l))) )

(defun rotate-right (l)
  (reverse (rotate-left (reverse l))) )

(defun royal-we (l)
  (subst 'we 'i l) )