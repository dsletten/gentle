;#!/usr/local/bin/clisp

;;
;   NAME:               features.lsp
;
;   STARTED:            001113
;   MODIFICATIONS:
;
;   PURPOSE:
;      Touretzky ex. 6.26
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
(defun right-side (l)
  (cdr (member '-vs- l)) )

(defun left-side (l)
  (reverse (cdr (member '-vs- (reverse l)))) ) ; Or simply (right-side (reverse l)) !!

(defun count-common (l)
  (length (intersection (right-side l)
			(left-side l))) )

(defun compare (l)
  (list (count-common l) 'common 'features) )

