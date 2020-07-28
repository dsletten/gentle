;#!/opt/local/bin/sbcl.bin --core /opt/local/lib/sbcl/sbcl.core --noinform
;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               tokenizer.lisp
;;;;
;;;;   Started:            Wed Jan 11 23:03:57 2006
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
(load "fsm.lisp")

(defpackage tokenizer (:use common-lisp fsm))

(in-package tokenizer)

(defclass target ()
  ((string :initarg :string :initform "" :reader target-string)
   (length :initarg :length :reader target-length)
   (index :initform 0 :accessor target-index)
   (lexeme-start :initform 0 :accessor target-lexeme-start)))

(defun make-target (s)
  (make-instance 'target :string s :length (length s)))

(defmethod print-object ((obj target) stream)
  (format stream "#<TARGET ~S>" (target-string obj)))

(defmethod advance ())

(defmethod retract ((obj target) n)
  (when (<= (target-index obj) (target-length obj))
    (decf (target-index obj))))

(defclass recognizer ()
  ((name :initarg :name :reader recognizer-name)
   (nodes :initarg :nodes :reader recognizer-nodes)))

(defun make-recognizer (nodes)
  (make-instance 'recognizer :nodes nodes))

(defmacro defrecognizer (name)
  `(make-recognizer ',name))

(defrecognizer integer)

(defnode start)
(defnode leading-digit)
(defnode digit)
(defnode end)

(defarc start non-zero-digit leading-digit "digit")
(defarc start non-digit fail "fail")
(defarc leading-digit any-digit digit "digit")
(defarc leading-digit non-digit fail "fail")
(defarc digit any-digit digit "digit")
(defarc digit non-digit end "integer")

