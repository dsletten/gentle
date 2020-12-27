;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   If you give someone Fortran, he has Fortran. If you give someone Lisp, he has any language he pleases.
;;;;   -- Guy Steele
;;;;
;;;;   Name:               fsm-compiled.lisp
;;;;
;;;;   Started:            Sat Dec 26 05:03:10 2020
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

(defpackage :fsm-compiled (:use :common-lisp :test))

(in-package :fsm-compiled)

(defstruct (node (:print-function print-node))
  name
  (inputs '())
  (outputs '()))

(defun print-node (node stream depth)
  (declare (ignore depth))
  (format stream "#<Node ~A>" (node-name node)))

(defstruct (arc (:print-function print-arc))
  from
  to
  label
  action)

(defun print-arc (arc stream depth)
  (declare (ignore depth))
  (format stream "#<ARC ~A ---~A---> ~A>" (node-name (arc-from arc)) (arc-label arc) (node-name (arc-to arc))))

(defvar *nodes* '())
(defvar *arcs* '())

(defun initialize ()
  (setf *nodes* '()
        *arcs* '()))

(defmacro defnode (name)
  `(add-node ',name))

;;;
;;;    Preserve order of nodes.
;;;    
(defun add-node (name)
  (unless (find-node name :strict nil)
    (let ((node (make-node :name name)))
      (setf *nodes* (nconc *nodes* (list node)))
      node)))

;;;
;;;    Apparently ADD-ARC wants to detect non-existent NODEs => ERROR
;;;    But ADD-NODE needs to be able to handle new NODEs.
;;;    
(defun find-node (name &key (strict t))
  (or (find name *nodes* :key #'node-name)
      (if strict
          (error "No node named ~A exists." name)
          nil)))

;; (defun find-node (name)
;;   (find name *nodes* :key #'node-name))

(defmacro defarc (from label to &optional action)
  `(add-arc ',from ',label ',to ',action))

(defun add-arc (from-name label to-name action)
  (let* ((from (find-node from-name))
         (to (find-node to-name))
         (arc (make-arc :from from :label label :to to :action action)))
    (unless (find-arc arc)
      (setf *arcs* (nconc *arcs* (list arc))
            (node-outputs from) (nconc (node-outputs from) (list arc))
            (node-inputs to) (nconc (node-inputs to) (list arc)))
      arc)))

(defun find-arc (arc)
  (find arc *arcs* :test #'equalp))

(defnode start)
(defnode have-5)
(defnode have-10)
(defnode have-15)
(defnode have-20)
(defnode have-25)
(defnode end)

(defarc start   nickel               have-5  "Clunk!")
(defarc start   dime                 have-10 "Clink!")
(defarc start   quarter              have-25 "Ker-chunk!")
(defarc start   coin-return          start   "Nothing to return.")

(defarc have-5  nickel               have-10 "Clunk!")
(defarc have-5  dime                 have-15 "Clink!")
(defarc have-5  quarter              have-25 "Nickel change.")
(defarc have-5  coin-return          start   "Returned five cents.")

(defarc have-10 nickel               have-15 "Clunk!")
(defarc have-10 dime                 have-20 "Clink!")
(defarc have-10 quarter              have-25 "Dime change.")
(defarc have-10 coin-return          start   "Returned ten cents.")

(defarc have-15 nickel               have-20 "Clunk!")
(defarc have-15 dime                 have-25 "Clink!")
(defarc have-15 quarter              have-25 "Fifteen cents change.")
(defarc have-15 gum-button           end     "Deliver gum.")
(defarc have-15 coin-return          start   "Returned fifteen cents.")

(defarc have-20 nickel               have-25 "Clunk!")
(defarc have-20 dime                 have-25 "Nickel change.")
(defarc have-20 quarter              have-25 "Twenty cents change.")
(defarc have-20 gum-button           end     "Deliver gum, nickel change.")
(defarc have-20 mint-button          end     "Deliver mints.")
(defarc have-20 coin-return          start   "Returned twenty cents.")

(defarc have-25 nickel               have-25 "Nickel returned.")
(defarc have-25 dime                 have-25 "Dime returned.")
(defarc have-25 quarter              have-25 "Quarter returned.")
(defarc have-25 gum-button           end     "Deliver gum, dime change.")
(defarc have-25 mint-button          end     "Deliver mints, nickel change.")
(defarc have-25 chocolate-bar-button end     "Deliver chocolate bar.")
(defarc have-25 coin-return          start   "Returned twenty-five cents.")

(defun compile-arc (arc)
  `((eql input ',(arc-label arc))
    (format t "~A~%" ,(arc-action arc))
    (,(node-name (arc-to arc)) (rest inputs))))

(defun compile-node (node)
  (let ((arcs (node-outputs node)))
    `(defun ,(node-name node) (inputs)
       (let ((input (first inputs)))
         (cond ((null inputs) ',(node-name node))
               ,@(mapcar #'compile-arc arcs)
               (t (error "No arc from ~A with label ~A." ',(node-name node) input)))) )))

(defmacro compile-machine ()
  `(progn ,@(mapcar #'compile-node *nodes*)))
