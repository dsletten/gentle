;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               compile-fsm.lisp
;;;;
;;;;   Started:            Fri Feb 11 20:53:22 2005
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
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
;;;;   Todo:
;;;;
;;;;

;(defpackage compile-fsm (:use common-lisp))

;(in-package compile-fsm)

(defun compile-arc (arc)
  `((eq this-input ',(arc-label arc))
    (format t "~A~%" ,(arc-action arc))
    (,(node-name (arc-to arc)) (rest input-syms))))

(defun compile-node (node)
  `(defun ,(node-name node) (input-syms)
     (let ((this-input (first input-syms)))
       (cond ((null input-syms) ',(node-name node))
	     ,@(loop for arc in (node-outputs node)
		     collect (compile-arc arc))
	     (t (error "No arc from ~A with label ~A."
		       ',(node-name node) this-input)))) ))

;; (defmacro compile-machine ()
;;   `(progn ,@(mapcar #'(lambda (node) (compile-node node)) *nodes*)))
;;;
;;;    D'oh!
;;;    
(defmacro compile-machine ()
  `(progn ,@(mapcar #'compile-node *nodes*)))
