;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   If you give someone Fortran, he has Fortran. If you give someone Lisp, he has any language he pleases.
;;;;   -- Guy Steele
;;;;
;;;;   Name:               fsm2.lisp
;;;;
;;;;   Started:            Fri Dec 25 03:59:52 2020
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;   This is an increasingly divergent implementation from Touretzky's original.
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

(defpackage :fsm2 (:use :common-lisp :test))

(in-package :fsm2)

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

;; (defmacro defarc (from label to &optional action)
;;   (let ((response (if (stringp action)
;;                       `(format t "~A ~%" ,action)
;;                       action)))
;;   `(add-arc ',from ',label ',to #'(lambda () ,response))))

(defmacro defarc (from label to &optional action)
  `(add-arc ',from ',label ',to #'(lambda () (declare (special inputs)) ,action)))

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

(defun terminalp (node)
  (null (node-outputs node)))

;;;
;;;    INPUTS represents the list of inputs already processed as the FSM
;;;    transitions.
;;;    It may be modified via actions associated with arcs betweens nodes.
;;;    
(defun fsm (&optional (node-name 'start))
  (let ((inputs '()))
    (declare (special inputs))
    (labels ((process (node)
               (unless (terminalp node)
                 (process (transition node)))) )
      (process (find-node node-name)))) )

(defun transition (node)
  (declare (special inputs))
  (format t "State ~A.  Input: " (node-name node))
  (let* ((*read-eval* nil)
         (ans (read))
         (arc (find ans (node-outputs node) :key #'arc-label)))
    (cond ((null arc)
           (format t "No arc from ~A has label ~A.~%" (node-name node) ans)
           node)
          (t (funcall (arc-action arc))
             (arc-to arc)))) )

(defnode start)
(defnode have-5)
(defnode have-10)
(defnode have-15)
(defnode have-20)
(defnode end)

(defun accept-nickel () (format t "Clunk!~%"))
(defun accept-dime () (format t "Clink!~%"))

;;;
;;;    The main problem here is that the INPUTS variable is a magical name. There
;;;    is no clear scope, and isn't controlled by the programmer.
;;;    
(defarc start   nickel       have-5  (progn (accept-nickel) (push :nickel inputs)))
(defarc start   dime         have-10 (progn (accept-dime) (push :dime inputs)))
(defarc start   coin-return  start   (format t "Nothing to return.~%"))
(defarc have-5  nickel       have-10 (progn (accept-nickel) (push :nickel inputs)))
(defarc have-5  dime         have-15 (progn (accept-dime) (push :dime inputs)))
(defarc have-5  coin-return  start   (progn (format t "Returning: ~A~%" inputs) (setf inputs '())))
(defarc have-10 nickel       have-15 (progn (accept-nickel) (push :nickel inputs)))
(defarc have-10 dime         have-20 (progn (accept-dime) (push :dime inputs)))
(defarc have-10 coin-return start    (progn (format t "Returning: ~A~%" inputs) (setf inputs '())))
(defarc have-15 nickel       have-20 (progn (accept-nickel) (push :nickel inputs)))
(defarc have-15 dime         have-20 "Nickel change.")
(defarc have-15 gum-button   end     "Deliver gum.")
(defarc have-15 coin-return  start   (progn (format t "Returning: ~A~%" inputs) (setf inputs '())))
(defarc have-20 nickel       have-20 "Nickel returned.")
(defarc have-20 dime         have-20 "Dime returned.")
(defarc have-20 gum-button   end     "Deliver gum, nickel change.")
(defarc have-20 mint-button  end     "Deliver mints.")
(defarc have-20 coin-return  start   (progn (format t "Returning: ~A~%" inputs) (setf inputs '())))


