;#!/usr/local/bin/clisp

;;
;   NAME:               IO.lsp
;
;   STARTED:            000922
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
(defun read-file (s)
  (with-open-file (mystream s)
    (do ()
	((not (print (read-line mystream nil nil)))))) )