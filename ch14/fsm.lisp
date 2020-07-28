;#!/usr/local/bin/sbcl --noinform
;#!/sw/bin/clisp
;#!/usr/local/bin/clisp
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               fsm.lisp
;;;;
;;;;   Started:            Fri Feb 11 16:05:04 2005
;;;;   Modifications:      060107 Converted *NODES* and *ARCS* and slots of
;;;;                              nodes into queues.
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

(load "/Users/dsletten/lisp/packages/collections.fasl")

(defpackage fsm (:use common-lisp collections))

(in-package fsm)

(defclass node ()
  ((name :initarg :name :initform nil :reader node-name)
   (inputs :initarg :inputs :initform (make-linked-queue)
	   :accessor node-inputs)
   (outputs :initarg :outputs :initform (make-linked-queue)
	    :accessor node-outputs)))

(defun make-node (name)
  (make-instance 'node :name name))

(defmethod print-object ((obj node) stream)
  (format stream "#<NODE ~A>" (node-name obj)))

(defclass arc ()
  ((from :initarg :from :initform nil :reader arc-from)
   (to :initarg :to :initform nil :reader arc-to)
   (label :initarg :label :initform nil :reader arc-label)
   (action :initarg :action :initform nil :reader arc-action)))

(defun make-arc (from label to action)
  (make-instance 'arc :from from :label label :to to :action action))

(defmethod print-object ((obj arc) stream)
  (format stream "#<ARC ~A -> ~A -> ~A>" (node-name (arc-from obj))
	  (arc-label obj) (node-name (arc-to obj))))

(defvar *nodes* (make-linked-queue))
(defvar *arcs* (make-linked-queue))
(defvar *current-node*)

(defun initialize ()
  (make-empty *nodes*)
  (make-empty *arcs*)
  (setf *current-node* nil))

(defmacro defnode (name)
  `(add-node ',name))

(defun add-node (name)
  (let ((new-node (make-node name)))
    (enqueue *nodes* new-node)
    new-node))

(defun find-node (name)
  (or (find name (elements *nodes*) :key #'node-name)
      (error "No node named ~A exists." name)))

(defmacro defarc (from label to &optional action)
  `(add-arc ',from ',label ',to ',action))

(defun add-arc (from-name label to-name action)
  (let* ((from (find-node from-name))
	 (to (find-node to-name))
	 (new-arc (make-arc from label to action)))
    (enqueue *arcs* new-arc)
    (enqueue (node-outputs from) new-arc)
    (enqueue (node-inputs to) new-arc)
    new-arc))

(defun fsm (&optional (starting-point 'start))
  (setf *current-node* (find-node starting-point))
  (loop while (elements (node-outputs *current-node*))
	do (one-transition)))

(defun one-transition ()
  (format t "State ~A.  Input: " (node-name *current-node*))
  (let* ((ans (read))
	 (arc (find ans (elements (node-outputs *current-node*))
		    :key #'arc-label)))
    (cond (arc
	   (let ((new (arc-to arc)))
	     (format t "~A~%" (arc-action arc))
	     (setf *current-node* new)))
	  (t (format t "No arc from ~A has label ~A.~%"
		     (node-name *current-node*) ans)))) )
