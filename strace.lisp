;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               strace.lisp
;;;;
;;;;   Started:            Wed Dec 16 00:58:50 2020
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
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :strace (:use :common-lisp :test))

(in-package :strace)


;;;
;;;    Slade ch. 11
;;;
;; (defun hook (expander form env)
;;   "CLHS *MACROEXPAND-HOOK* example"
;;   (format t "Now expanding: ~S~%" form)
;;   (let ((result (funcall expander form env)))
;;     (format t "Result: ~S~%" result)
;;     result))

;; (defmacro machook (x y) `(/ (+ ,x ,y) 2))

;; (let ((*macroexpand-hook* #'hook)) (macroexpand '(machook 1 2)))
